"10 m, r0 = 7 mm conductor at 0.5 m depth in σ = 0.01 S/m, εr = 10 soil."
function portela_study(segments)
    st = Structure(Linear("soil", 10.0, 1.0, 0.01))
    add_node!(st, Node("Node_1", (0.0, 0.0, -0.5)))
    add_node!(st, Node("Node_2", (10.0, 0.0, -0.5)))
    add_material!(st, Linear("copper", 1.0, 1.0, 5.96e7))
    add_element!(st, Line("Line_1", "Node_1", "Node_2", 0.007, segments, "copper"))
    return Study("portela", st)
end

unit_current(node) = [Source(node, 1.0)]
node_index(study, id) = find_node_index(study.structure, id)

@testset "passivity, DC plateau and Sunde/Dwight limit (test_solve)" begin
    study = portela_study(10)
    zmag = map((10.0, 1e2, 1e3, 1e4, 1e5, 1e6)) do f
        zin = solve_frequency!(study, 2π * f, unit_current("Node_1")).voltage[node_index(study, "Node_1")]
        @test real(zin) >= -1e-9 * max(abs(zin), 1.0)
        abs(zin)
    end
    @test abs(zmag[1] - zmag[2]) < 0.05 * zmag[2]
    l, r0, depth, sigma = 10.0, 0.007, 0.5, 0.01
    r_dc = 1 / (2π * sigma * l) * (log(2l / r0) + log(2l / (2depth)) - 2)
    @test abs(zmag[1] - r_dc) < 0.15 * r_dc
end

@testset "sweep matches a manual run loop (test_sweep)" begin
    freqs = log_frequency_axis(1e2, 1e6, 5)
    @test length(freqs) == 5 && freqs[1] ≈ 1e2 && abs(freqs[5] - 1e6) < 1
    @test all(freqs[k] / freqs[k-1] ≈ freqs[2] / freqs[1] for k in 2:5)
    @test_throws TupaError log_frequency_axis(1.0, 10.0, 1)
    a = run_sweep!(portela_study(4), freqs, unit_current("Node_1"))
    zin = input_impedance(a, "Node_1")
    vmax = max_voltage_magnitude(a)
    b = portela_study(4)
    for (k, f) in enumerate(freqs)
        v = solve_frequency!(b, 2π * f, unit_current("Node_1")).voltage[node_index(b, "Node_1")]
        @test abs(v - zin[k]) < 1e-12 * abs(zin[k])
        @test vmax[k] >= abs(zin[k]) - 1e-12
    end
    @test_throws TupaError input_impedance(a, "Node_2")
end

@testset "voltage sources (ADR 0016)" begin
    freqs = [1e2, 1e4, 1e6]
    u = 100.0 + 0im
    zin = input_impedance(run_sweep!(portela_study(4), freqs, unit_current("Node_1")), "Node_1")
    volt = run_sweep!(portela_study(4), freqs, [Source("Node_1", u; voltage = true)])
    zv = input_impedance(volt, "Node_1")
    for k in eachindex(freqs)
        @test abs(volt.voltage_results[node_index(volt, "Node_1"), k] - u) < 1e-9 * abs(u)
        @test abs(zv[k] - zin[k]) < 1e-9 * abs(zin[k])
    end

    study = portela_study(2)
    om = 2π * 1e3
    i2 = 0.5 + 0.25im
    out = solve_frequency!(study, om, [Source("Node_1", 50.0; voltage = true), Source("Node_2", i2)])
    @test out.source_currents[2] == i2
    @test abs(out.voltage[node_index(study, "Node_1")] - 50) < 1e-9 * 50
    plain = solve_frequency!(study, om, [Source("Node_1", out.source_currents[1]), Source("Node_2", i2)])
    @test all(abs(a - b) < 1e-9 * max(abs(b), 1.0) for (a, b) in zip(out.voltage, plain.voltage))
end

@testset "transient vs low-frequency impedance, antialias option (test_transient)" begin
    study = portela_study(10)
    spec(aa; n = 1024) = TransientSpec(signal = double_exp_signal(1e3, "f250_2500"),
                                       source_node = "Node_1", observe_nodes = ["Node_1"],
                                       observe_electrodes = ["Line_1_e1"], nyquist_hz = 1e4,
                                       fft_points = n, antialias_start = aa)
    base = transient_response(study, spec(1.0))
    @test length(base.t) == 1024 && size(base.node_responses) == (1, 1024)
    @test all(isfinite, base.node_responses) && base.t[1] == 0 && issorted(base.t)
    @test size(base.i1_responses, 1) == 1
    zin = input_impedance(study, "Node_1")
    @test all(real(z) >= -1e-9 * max(abs(z), 1.0) for z in zin)
    ipeak = argmax(base.injected_current)
    ratio = base.node_responses[1, ipeak] / base.injected_current[ipeak]
    @test abs(ratio - abs(zin[2])) < 0.25 * abs(zin[2])
    half = transient_response(study, spec(0.25))
    @test maximum(abs, half.node_responses - base.node_responses) > 0 && all(isfinite, half.node_responses)
    same = transient_response(study, spec(1.0))
    @test maximum(abs, same.node_responses - base.node_responses) <= 1e-12 * 1e3
    @test_throws TupaError transient_response(study, spec(1.0; n = 1000))
end

function mesh_structure(rows_x, rows_y, segments, id; z = -1.0)
    st = Structure(Linear("soil", 10.0, 1.0, 0.01))
    add_material!(st, Linear("copper", 1.0, 1.0, 5.96e7))
    add_element!(st, MeshElement(id, (0.0, 0.0, z), 4.0, 4.0, rows_x, rows_y, 0.01, segments, "copper"))
    return st
end

@testset "mesh element (ADR 0020, test_mesh_element)" begin
    st = assemble!(mesh_structure(3, 3, 2, "M"))
    @test length(st.nodes) == 21 && length(st.electrodes) == 24
    at(id) = st.nodes[find_node_index(st, id)].p
    @test at("M-0000") == (0.0, 0.0, -1.0) && at("M-0202") == (4.0, 4.0, -1.0)
    @test at("M-0102") == (4.0, 2.0, -1.0)
    @test find_electrode_index(st, "M-0000-0001_e2") !== nothing

    st = assemble!(mesh_structure(3, 4, 1, "G"))
    @test length(st.nodes) == 12 && length(st.electrodes) == 17
    degree = zeros(Int, length(st.nodes))
    for e in st.electrodes
        degree[e.nodes[1]] += 1
        degree[e.nodes[2]] += 1
    end
    @test (count(==(2), degree), count(==(3), degree), count(==(4), degree)) == (4, 6, 2)

    for (rx, ry, seg, z) in ((1, 3, 1, -1.0), (101, 3, 1, -1.0), (3, 3, 0, -1.0), (3, 3, 1, 0.0))
        @test_throws TupaError assemble!(mesh_structure(rx, ry, seg, "M"; z = z))
    end

    st = mesh_structure(2, 2, 1, "M2")
    add_node!(st, Node("Top", (0.0, 0.0, -0.2)))
    add_element!(st, Line("Down", "Top", "M2-0000", 0.01, 2, "copper"))
    assemble!(st)
    @test find_electrode_index(st, "Down_e2") !== nothing && find_node_index(st, "Down_n1") !== nothing
end

@testset "assembly (test_assemble)" begin
    s = portela_study(10)
    assemble!(s.structure); assemble!(s.structure)
    @test (length(s.structure.nodes), length(s.structure.electrodes)) == (11, 10)
    @test find_electrode_index(s.structure, "Line_1_e1") !== nothing
    @test find_electrode_index(s.structure, "Line_1") === nothing

    st = Structure(Linear("soil", 10.0, 1.0, 0.01))
    add_node!(st, Node("A", (0.0, 0.0, -1.0)))
    add_material!(st, Linear("cu", 1.0, 1.0, 5.96e7))
    add_element!(st, Line("L", "A", "B", 0.01, 2, "cu"))
    err = try assemble!(st); nothing catch e; e end
    @test err isa TupaError && occursin("end node 'B' not found", err.msg)
    st = Structure(Linear("soil", 10.0, 1.0, 0.01))
    add_node!(st, Node("A", (0.0, 0.0, -1.0)))
    add_node!(st, Node("B", (1.0, 0.0, -1.0)))
    add_element!(st, Line("L", "A", "B", 0.01, 2, "missing"))
    err = try assemble!(st); nothing catch e; e end
    @test err isa TupaError && occursin("material 'missing' not found", err.msg)
end

@testset "every common/*.json loads, validates and assembles (test_validation)" begin
    cases = filter(endswith(".json"), readdir(COMMON))
    @test length(cases) >= 25
    for f in cases
        c = validate_study_references!(load_study(joinpath(COMMON, f)))
        @test !isempty(c.study.structure.electrodes)
    end
    # common/README: 185 nodes, 200 electrodes (pinned in test_mesh_element.f90)
    st = validate_study_references!(load_study(joinpath(COMMON, "portelaMesh.json"))).study.structure
    @test (length(st.nodes), length(st.electrodes)) == (185, 200)
end

@testset "rod_air: NaN-free and near the analytical resistance (ADR 0019)" begin
    c = validate_study_references!(load_study(joinpath(COMMON, "rod_air.json")))
    run_sweep!(c.study, [10.0, 100.0], c.sources)
    zin = input_impedance(c.study, c.sources[1].node)
    @test all(z -> isfinite(real(z)) && isfinite(imag(z)), zin)
    @test abs(real(zin[1]) - 20.9) < 0.5
end
