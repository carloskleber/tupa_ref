#=
harmonic_imp_counterpoises.jl

Harmonic input impedance Z_h(f) of the four counterpoise topologies
defined in `counterpoises.jl`, using the unified HEM/Sommerfeld kernel
selector from `hem1.jl`.

Soil parameters:
    ρ_earth = 2000 Ω·m  (σ ≈ 5e-4 S/m)
    εr      = 10

Geometry parameters (representative TL tower counterpoise):
    L    = 30 m   horizontal arm length
    s1   = 0.5 m  inner Δx separation
    s2   = 3.0 m  inclined arm Δx
    s3   = 0.5 m  inner Δy separation
    s4   = 1.5 m  inclined arm Δy
    h    = 0.5 m  burial depth (positive)
    r    = 7 mm   wire radius
    dw   = 1.0 m  narrow-config width

Injection: 1 A at the node closest to the geometric origin
(symmetric centre).

Kernel: KERNEL_MODIFIED_IMG (default). Switch to KERNEL_RIGOROUS_SOMM
in `run_all()` for the full Sommerfeld-tail evaluation (slow).

Output: harmonic_imp_counterpoises.png + .csv in this directory.
=#

include(joinpath(@__DIR__, "counterpoises.jl"))

using FFTW
using Printf
using Plots

# ------------------------------------------------------------------
# Soil + sweep parameters
# ------------------------------------------------------------------
const RHO_EARTH = 2000.0
const SIGMA0    = 1.0 / RHO_EARTH
const EPSR      = 10.0
const FREQS     = 10.0 .^ range(2, 6, length = 17)   # 100 Hz … 1 MHz

# Geometry
const L_ARM = 30.0
const S1    = 0.5
const S2    = 3.0
const S3    = 0.5
const S4    = 1.5
const H_BUR = 0.5
const R_W   = 7e-3
const DW    = 1.0
const L_SEG = 1.5     # target segment length [m]

# ------------------------------------------------------------------
# Inject 1 A at the node closest to the origin (the centre of the
# counterpoise). Return Z_h = V_inj / 1A.
# ------------------------------------------------------------------
function find_centre_node(nodes)
    nn = size(nodes, 1)
    dmin = Inf
    idx = 1
    for i in 1:nn
        d = norm(nodes[i, :])
        if d < dmin
            dmin = d
            idx = i
        end
    end
    return idx
end


function harmonic_impedance(wires, kernel::HalfSpaceKernel;
                            Lseg::Real = L_SEG)
    electrodes, nodes = seg_electrode_list(wires, Lseg)
    # Build image electrode list — mirror through z = 0
    ns = length(electrodes)
    images = Vector{Electrode}(undef, ns)
    for i in 1:ns
        sp = copy(electrodes[i].start_point); sp[3] = -sp[3]
        ep = copy(electrodes[i].end_point);   ep[3] = -ep[3]
        images[i] = new_electrode(sp, ep, electrodes[i].radius)
    end
    a_mat, b_mat = incidence(electrodes, nodes)
    inj_idx = find_centre_node(nodes)

    Zh = Vector{ComplexF64}(undef, length(FREQS))
    for (k, f) in enumerate(FREQS)
        ω = 2π * f
        s = im * ω
        ε̂1 = EPSR * EPS0 - im * SIGMA0 / ω
        κ  = SIGMA0 + im * ω * EPSR * EPS0
        k1 = ω * sqrt(MU0 * ε̂1)
        imag(k1) > 0 && (k1 = -k1)
        γ  = im * k1
        zl, zt = impedances_halfspace(electrodes, images, γ, s, 1.0, κ,
                                      ε̂1, H_BUR, kernel)
        yn = admittance(zl, zt, a_mat, b_mat)
        I_inj = zeros(ComplexF64, size(yn, 1))
        I_inj[inj_idx] = 1.0
        V = yn \ I_inj
        Zh[k] = V[inj_idx]
    end
    return Zh, length(electrodes), inj_idx
end


function run_all(; kernel::HalfSpaceKernel = KERNEL_MODIFIED_IMG,
                  outdir::AbstractString = @__DIR__)
    cases = [
        ("conventional1", conventional1(L_ARM, S1, S2, S3, S4, H_BUR, R_W)),
        ("conventional2", conventional2(L_ARM, S1, S2, S3, S4, H_BUR, R_W)),
        ("narrow",        narrow(L_ARM, S1, S2, S3, S4, H_BUR, R_W, DW)),
        ("unconventional",unconventional(L_ARM, S1, S2, S3, S4, H_BUR, R_W)),
    ]

    results = Dict{String, NamedTuple}()
    for (name, wires) in cases
        @printf "── %s ──\n" name
        @time Zh, nseg, inj = harmonic_impedance(wires, kernel)
        @printf "  %d segments, injection node = %d\n" nseg inj
        results[name] = (Zh = Zh, nseg = nseg)
    end

    # CSV
    csv = joinpath(outdir, "harmonic_imp_counterpoises.csv")
    open(csv, "w") do io
        write(io, "f")
        for (name, _) in cases
            write(io, ",|Z_$(name)|,arg_$(name)_deg")
        end
        write(io, "\n")
        for (k, f) in enumerate(FREQS)
            @printf io "%.3e" f
            for (name, _) in cases
                Zk = results[name].Zh[k]
                @printf io ",%.6e,%.3f" abs(Zk) rad2deg(angle(Zk))
            end
            write(io, "\n")
        end
    end
    println("saved $csv")

    # Magnitude plot
    p1 = plot(xlabel = "frequency [Hz]", ylabel = "|Z_h| [Ω]",
              xscale = :log10, yscale = :log10,
              title = @sprintf("Counterpoise harmonic impedance — ρ=%.0f Ωm, εr=%.1f, kernel=%s",
                               RHO_EARTH, EPSR, string(kernel)),
              legend = :topleft, lw = 2)
    for (name, _) in cases
        plot!(p1, FREQS, abs.(results[name].Zh); lw = 2, marker = :circle,
              ms = 3, label = name)
    end

    # Phase plot
    p2 = plot(xlabel = "frequency [Hz]", ylabel = "∠Z_h [deg]",
              xscale = :log10, legend = false, lw = 2)
    for (name, _) in cases
        plot!(p2, FREQS, rad2deg.(angle.(results[name].Zh)); lw = 2,
              marker = :circle, ms = 3, label = name)
    end

    fig = plot(p1, p2; layout = (2, 1), size = (900, 800))
    fname = joinpath(outdir, "harmonic_imp_counterpoises.png")
    savefig(fig, fname)
    println("saved $fname")

    return results
end

if abspath(PROGRAM_FILE) == @__FILE__
    run_all()
end
