# CSV and JSON result writers (`mResultsWriter`, ADR 0012/0013/0015).
# Numbers use the Fortran `ES16.8` layout (`3.08173006E+01`) so files are
# byte-comparable with the reference implementation.

"`ES16.8`-style formatting: `d.dddddddd` `E±ee` (three-digit exponents drop the `E`, as Fortran does)."
function fmt_real(x::Real)
    isnan(x) && return "NaN"
    isinf(x) && return x > 0 ? "Infinity" : "-Infinity"
    mant, ex = split(@sprintf("%.8e", x), 'e')
    e = parse(Int, ex)
    sign = e < 0 ? '-' : '+'
    return abs(e) >= 100 ? string(mant, sign, abs(e)) : string(mant, 'E', sign, lpad(abs(e), 2, '0'))
end

fmt_complex_json(v::Complex) = string("{\"re\": ", fmt_real(real(v)), ", \"im\": ", fmt_real(imag(v)), "}")

function json_escape(s::AbstractString)
    io = IOBuffer()
    for c in s
        if c == '"'
            print(io, "\\\"")
        elseif c == '\\'
            print(io, "\\\\")
        elseif c == '\n'
            print(io, "\\n")
        elseif c == '\r'
            print(io, "\\r")
        elseif c == '\t'
            print(io, "\\t")
        elseif UInt32(c) < 0x20
            @printf(io, "\\u%04x", UInt32(c))
        else
            print(io, c)
        end
    end
    return String(take!(io))
end

# `outputs` filter: no list means everything
wanted(name::AbstractString, list) = list === nothing || name in list

function require_sweep(study::Study, who::AbstractString)
    frequency_count(study.voltage_results) == 0 &&
        raise_error("$who: study has no sweep results (call runSweep first)")
    return nothing
end

"""
    results_csv(study; nodes, electrodes, quantities) -> String

Sweep results as tidy CSV (`frequency_hz,quantity,id,re,im`,
`writeResultsCsv`); the keywords are the `outputs` filters (ADR 0013),
`nothing` meaning everything.
"""
function results_csv(study::Study; nodes = nothing, electrodes = nothing, quantities = nothing)
    require_sweep(study, "writeResultsCsv")
    io = IOBuffer()
    println(io, "frequency_hz,quantity,id,re,im")
    row(f, q, id, v) = println(io, fmt_real(f), ',', q, ',', id, ',', fmt_real(real(v)), ',',
                               fmt_real(imag(v)))
    vr, i1r, i2r = study.voltage_results, study.long_current_results, study.trans_current_results
    for k in 1:frequency_count(vr)
        f = study.sweep_freq_hz[k]
        for i in 1:entity_count(vr)
            id = entity_id(vr, i)
            wanted(id, nodes) && wanted("voltage", quantities) && row(f, "voltage", id, vr[i, k])
        end
        for i in 1:entity_count(i1r)
            id = entity_id(i1r, i)
            wanted(id, electrodes) || continue
            wanted("i1", quantities) && row(f, "i1", id, i1r[i, k])
            wanted("i2", quantities) && row(f, "i2", id, i2r[i, k])
        end
    end
    return String(take!(io))
end

join_complex(r::ResultSet, i) = join((fmt_complex_json(r[i, k]) for k in 1:frequency_count(r)), ", ")

"""
    results_json(study; nodes, electrodes, quantities) -> String

Sweep results as JSON (ADR 0012 shape, `outputs` filtering per ADR 0013,
`writeResultsJson`).
"""
function results_json(study::Study; nodes = nothing, electrodes = nothing, quantities = nothing)
    require_sweep(study, "writeResultsJson")
    vr, i1r, i2r = study.voltage_results, study.long_current_results, study.trans_current_results
    io = IOBuffer()
    println(io, "{")
    println(io, "  \"title\": \"", json_escape(study.title), "\",")
    println(io, "  \"frequencies\": [", join(fmt_real.(study.sweep_freq_hz), ", "), "],")

    println(io, "  \"nodes\": [")
    items = String[]
    for i in 1:entity_count(vr)
        id = entity_id(vr, i)
        wanted(id, nodes) && wanted("voltage", quantities) || continue
        push!(items, string("    { \"id\": \"", json_escape(id), "\", \"voltage\": [",
                            join_complex(vr, i), "] }"))
    end
    isempty(items) || println(io, join(items, ",\n"))
    println(io, "  ],")

    println(io, "  \"electrodes\": [")
    empty!(items)
    for i in 1:entity_count(i1r)
        id = entity_id(i1r, i)
        want_i1 = wanted(id, electrodes) && wanted("i1", quantities)
        want_i2 = wanted(id, electrodes) && wanted("i2", quantities)
        want_i1 || want_i2 || continue
        s = string("    { \"id\": \"", json_escape(id), "\"")
        want_i1 && (s *= string(", \"i1\": [", join_complex(i1r, i), "]"))
        want_i2 && (s *= string(", \"i2\": [", join_complex(i2r, i), "]"))
        push!(items, s * " }")
    end
    isempty(items) || println(io, join(items, ",\n"))
    println(io, "  ],")

    println(io, "  \"derived\": {")
    if !isempty(study.sweep_source_ids) && wanted("inputImpedance", quantities)
        zin = input_impedance(study, study.sweep_source_ids[1])
        println(io, "    \"inputImpedance\": [", join(fmt_complex_json.(zin), ", "), "]")
    end
    println(io, "  }")
    println(io, "}")
    return String(take!(io))
end

"Transient results as tidy CSV (`time_s,quantity,id,value`, `writeTransientCsv`)."
function transient_csv(spec::TransientSpec, r::TransientResult)
    io = IOBuffer()
    println(io, "time_s,quantity,id,value")
    for k in eachindex(r.t)
        t = fmt_real(r.t[k])
        println(io, t, ",injectedCurrent,", spec.source_node, ',', fmt_real(r.injected_current[k]))
        for (i, id) in enumerate(spec.observe_nodes)
            println(io, t, ",voltage,", id, ',', fmt_real(r.node_responses[i, k]))
        end
        for (i, id) in enumerate(spec.observe_electrodes)
            println(io, t, ",i1,", id, ',', fmt_real(r.i1_responses[i, k]))
            println(io, t, ",i2,", id, ',', fmt_real(r.i2_responses[i, k]))
        end
    end
    return String(take!(io))
end

join_real(v) = join((fmt_real(x) for x in v), ", ")

"Transient results as JSON (ADR 0015, `writeTransientJson`)."
function transient_json(title::AbstractString, spec::TransientSpec, r::TransientResult)
    io = IOBuffer()
    println(io, "{")
    println(io, "  \"title\": \"", json_escape(title), "\",")
    println(io, "  \"sourceNode\": \"", json_escape(spec.source_node), "\",")
    println(io, "  \"time\": [", join_real(r.t), "],")
    println(io, "  \"injectedCurrent\": [", join_real(r.injected_current), "],")
    println(io, "  \"nodes\": [")
    nn = length(spec.observe_nodes)
    for (i, id) in enumerate(spec.observe_nodes)
        print(io, "    { \"id\": \"", json_escape(id), "\", \"voltage\": [",
              join_real(view(r.node_responses, i, :)), "] }")
        println(io, i < nn ? "," : "")
    end
    nn == 0 && println(io)
    ne = length(spec.observe_electrodes)
    if ne == 0
        println(io, "  ],\n  \"electrodes\": []")
    else
        println(io, "  ],\n  \"electrodes\": [")
        for (i, id) in enumerate(spec.observe_electrodes)
            print(io, "    { \"id\": \"", json_escape(id), "\", \"i1\": [",
                  join_real(view(r.i1_responses, i, :)), "], \"i2\": [",
                  join_real(view(r.i2_responses, i, :)), "] }")
            println(io, i < ne ? "," : "")
        end
        println(io, "  ]")
    end
    println(io, "}")
    return String(take!(io))
end
