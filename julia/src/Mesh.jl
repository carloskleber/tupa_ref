# System matrices and the frequency-domain solve (`mMesh`): topology
# matrices A, B, C, D, impedance matrices, augmented Zeq (ADR 0003) and the
# LU solve. Sign and propagation conventions follow theory.md §2, §5, §6
# (ADR 0008).

"Medium code of the air half-space."
const AIR = 1
"Medium code of the soil half-space."
const SOIL = 2

"""
Image reflection model (ROADMAP Phase 10 item 2, ADR 0024, theory.md §5):
`:frequency_dependent`, `Γ(ω) = (W_own − W_other)/(W_own + W_other)`, the
quasi-static Fresnel form of the original Matlab default mode (the default),
or `:ideal`, `Γ = +1` in soil and `−1` in air — the `|W_own| ≫ |W_other|`
limit (Matlab `SOLO_IDEAL`) and the pre-Phase-10 behaviour.
"""
const IMAGE_MODELS = (:frequency_dependent, :ideal)

"Medium constants at one frequency (theory.md §5: `c_E`, `c_M`, `γ`, image `Γ`)."
struct MediumConstants
    "`1/(4π W_air)`"
    c_e_air::ComplexF64
    "`1/(4π W_soil)`"
    c_e_soil::ComplexF64
    "`jωμ_air/(4π)`"
    c_m_air::ComplexF64
    "`jωμ_soil/(4π)`"
    c_m_soil::ComplexF64
    "Air propagation constant"
    prop_air::ComplexF64
    "Soil propagation constant"
    prop_soil::ComplexF64
    "Reflection coefficient of the image of a segment in air (ideal: −1)"
    gamma_air::ComplexF64
    "Reflection coefficient of the image of a segment in soil (ideal: +1)"
    gamma_soil::ComplexF64
end
MediumConstants() = MediumConstants(0im, 0im, 0im, 0im, 0im, 0im, -1.0 + 0im, 1.0 + 0im)

"Solution of one injection pattern: node voltages and electrode end currents."
struct Solution
    "Node voltages `u` (V)"
    voltage::Vector{ComplexF64}
    "End currents `i1` at node n1 of each segment, positive INTO the segment"
    current1::Vector{ComplexF64}
    "End currents `i2` at node n2 of each segment, positive INTO the segment"
    current2::Vector{ComplexF64}
end

"""
Discretised mesh for the frequency-domain HEM solution. The topology
matrices (theory.md §6) are kept as the end-node index vectors `n1`/`n2`;
`topology_matrices` expands them.
"""
mutable struct Mesh
    "Number of nodes"
    nno::Int
    "Number of electrode segments"
    nseg::Int
    "Start node of each segment"
    n1::Vector{Int}
    "End node of each segment"
    n2::Vector{Int}
    "Transversal impedance (nseg × nseg)"
    ztrans::Matrix{ComplexF64}
    "Longitudinal impedance (nseg × nseg)"
    zlong::Matrix{ComplexF64}
    "Medium constants at the current frequency"
    medium::MediumConstants
    "Image reflection model, one of `IMAGE_MODELS`"
    image_model::Symbol
end

"Allocate for `nn` nodes and the segments `n1[i] → n2[i]`."
Mesh(nn::Int, n1::Vector{Int}, n2::Vector{Int}) =
    Mesh(nn, length(n1), n1, n2, zeros(ComplexF64, length(n1), length(n1)),
         zeros(ComplexF64, length(n1), length(n1)), MediumConstants(), :frequency_dependent)

"""
Topology matrices `(A, B, C, D)` (theory.md §6): `A` (nseg × nno) −1 at n1,
+1 at n2; `B` (nseg × nno) −½ at n1 and n2; `C` (nno × nseg) +1 at (n1, seg);
`D` (nno × nseg) +1 at (n2, seg).
"""
function topology_matrices(m::Mesh)
    A = zeros(ComplexF64, m.nseg, m.nno); B = zeros(ComplexF64, m.nseg, m.nno)
    C = zeros(ComplexF64, m.nno, m.nseg); D = zeros(ComplexF64, m.nno, m.nseg)
    for i in 1:m.nseg
        A[i, m.n1[i]] = -1.0
        A[i, m.n2[i]] = 1.0
        B[i, m.n1[i]] = -0.5
        B[i, m.n2[i]] = -0.5
        C[m.n1[i], i] = 1.0
        D[m.n2[i], i] = 1.0
    end
    return A, B, C, D
end

"Medium constants from the complex immittance `W(ω)` of each medium (`calcParamW`; `mu_*` in H/m)."
function calc_param_w!(m::Mesh, omega::Float64, mu_air::Float64, w_air::ComplexF64,
                       mu_soil::Float64, w_soil::ComplexF64)
    jw = complex(0.0, omega)
    c_e_air, c_e_soil = 1.0 / (FOUR_PI * w_air), 1.0 / (FOUR_PI * w_soil)
    gamma_air, gamma_soil = image_coefficients(m.image_model, c_e_air, c_e_soil)
    m.medium = MediumConstants(c_e_air, c_e_soil,
                               complex(0.0, omega * mu_air / FOUR_PI),
                               complex(0.0, omega * mu_soil / FOUR_PI),
                               sqrt(jw * mu_air * w_air), sqrt(jw * mu_soil * w_soil),
                               gamma_air, gamma_soil)
    return m
end

"""
Image reflection coefficients `(Γ_air, Γ_soil)` of `model` (`calcImageCoefficients`).
With `cE = 1/(4πW)`: `Γ_own = (W_own − W_other)/(W_own + W_other) =
(cE_other − cE_own)/(cE_other + cE_own)`; applied to the image parcels of both
`Z_t` and `Z_ℓ`, as the original Matlab does.
"""
function image_coefficients(model::Symbol, c_e_air::ComplexF64, c_e_soil::ComplexF64)
    model === :ideal && return (-1.0 + 0im, 1.0 + 0im)
    return ((c_e_soil - c_e_air) / (c_e_soil + c_e_air), (c_e_air - c_e_soil) / (c_e_air + c_e_soil))
end

# (γ, c_E, c_M, image factor Γ): ideal limit "−1" in air, "+1" in soil
medium_for(m::Mesh, pos::Integer) = pos == AIR ?
    (m.medium.prop_air, m.medium.c_e_air, m.medium.c_m_air, m.medium.gamma_air) :
    (m.medium.prop_soil, m.medium.c_e_soil, m.medium.c_m_soil, m.medium.gamma_soil)

"""
Self impedance of segment `i` with its own image (theory.md §4.3, §5,
ADR 0009, `calcZSelf`):

`Ztrans = cE (e^{-γd} g ± e^{-γdi} gi) / l²`,
`Zlong  = cM (e^{-γd} g ± cosθi e^{-γdi} gi) + zint`.
"""
function calc_z_self!(m::Mesh, i::Int, pos::Integer, d::Float64, di::Float64, l::Float64,
                      zint::ComplexF64, g::Float64, gi::Float64, cos_theta_i::Float64)
    prop, ce, cm, s = medium_for(m, pos)
    fprop = exp(-d * prop)
    fpropi = exp(-di * prop)
    m.ztrans[i, i] = ce * (fprop * g + s * fpropi * gi) / (l * l)
    m.zlong[i, i] = cm * (fprop * g + s * cos_theta_i * fpropi * gi) + zint
    return m
end

"""
Mutual impedance of segments `i`, `j` (theory.md §4.1, §5, ADR 0009,
`calcZMutual`); mixed-media pairs are neglected (zero, ADR 0005).
"""
function calc_z_mutual!(m::Mesh, i::Int, j::Int, pos1::Integer, pos2::Integer, d::Float64,
                        di::Float64, la::Float64, lb::Float64, g::Float64, gi::Float64,
                        cos_theta::Float64, cos_theta_i::Float64)
    zt = zl = complex(0.0, 0.0)
    if pos1 == pos2
        prop, ce, cm, s = medium_for(m, pos1)
        fprop = exp(-d * prop)
        fpropi = exp(-di * prop)
        zt = ce * (fprop * g + s * fpropi * gi) / (la * lb)
        zl = cm * (cos_theta * fprop * g + s * cos_theta_i * fpropi * gi)
    end
    m.ztrans[i, j] = m.ztrans[j, i] = zt
    m.zlong[i, j] = m.zlong[j, i] = zl
    return m
end

"""
Assemble the augmented `(nno + 2 nseg)²` matrix (theory.md §6, `calcFreq2`):

    [ A  |  Zlong/2 | -Zlong/2 ]
    [ B  |  Ztrans  |  Ztrans  ]
    [ 0  |  C       |  D       ]
"""
function calc_freq2(m::Mesh)
    nn, ns = m.nno, m.nseg
    z = zeros(ComplexF64, nn + 2ns, nn + 2ns)
    @inbounds for i in 1:ns
        z[i, m.n1[i]] = -1.0
        z[i, m.n2[i]] = 1.0
        z[ns+i, m.n1[i]] = -0.5
        z[ns+i, m.n2[i]] = -0.5
        z[2ns+m.n1[i], nn+i] = 1.0
        z[2ns+m.n2[i], nn+ns+i] = 1.0
    end
    @inbounds for j in 1:ns, i in 1:ns
        zl = m.zlong[i, j]
        z[i, nn+j] = zl * 0.5
        z[i, nn+ns+j] = zl * -0.5
        zt = m.ztrans[i, j]
        z[ns+i, nn+j] = zt
        z[ns+i, nn+ns+j] = zt
    end
    return z
end

"""
    inject_signals(mesh, pos, sigs) -> Vector{Solution}

Solve `Zeq x = b` for several injection patterns at once (one LU, multiple
right-hand sides, ADR 0003/0016): LAPACK `zgetrf`/`zgetrs`, the two halves
of the Fortran `ZGESV`. `sigs[p][k]` is the current injected at node
`pos[k]` in pattern `p`.
"""
function inject_signals(m::Mesh, pos::AbstractVector{Int}, sigs::AbstractVector{<:AbstractVector})
    nn, ns = m.nno, m.nseg
    zeq = calc_freq2(m)
    y = zeros(ComplexF64, nn + 2ns, length(sigs))
    for (p, sig) in enumerate(sigs)
        length(sig) == length(pos) || raise_error("injectSignal: pattern length mismatch")
        for (k, node) in enumerate(pos)
            y[2ns+node, p] += sig[k]
        end
    end
    f = lu!(zeq; check = false)
    issuccess(f) || raise_error("injectSignal: singular matrix (ZGESV INFO = $(f.info))")
    ldiv!(f, y)
    return [Solution(y[1:nn, p], y[nn+1:nn+ns, p], y[nn+ns+1:nn+2ns, p]) for p in axes(y, 2)]
end

"Solve for a single injection pattern."
inject_signal(m::Mesh, pos::AbstractVector{Int}, sig::AbstractVector) = only(inject_signals(m, pos, [sig]))
