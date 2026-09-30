using LinearAlgebra;
using HCubature;
using FFTW


const TWO_PI = 6.283185307179586;
const FOUR_PI = 12.56637061435917;
const MU0 = 1.256637061435917e-6;  # permeability vac.
const EPS0 = 8.854187817620e-12;  # permittivity vac.


## Auxiliary functions

"""
Laplace transform of the vector `y(t)`.

Parameters
----------
    y : the signal vector to be transformed
    tmax : last time stamp
    nt : number of time stamps

Returns
-------
    s : the complex frequency vector
    L(y) : transformed vector
"""
function laplace_transform(y, tmax, nt)
    c = log(nt^2) / tmax
    dt = tmax / (nt - 1)
    dw = 2pi / tmax
    ns = (nt ÷ 2) + 1
    s = [c + 1im * dw * (k - 1) for k = 1:ns]
    v = [dt * exp(-c * (k - 1) * dt) * y[k] for k = 1:nt]
    return s, rfft(v)
end


"""
Inverse Laplace transform of the vector y(s).

Parameters
----------
    y : the signal vector to be transformed
    tmax : last time stamp
    nt : number of time stamps

Returns
-------
    (L^-1)(y) : transformed vector
"""
function invlaplace_transform(y, tmax, nt)
    c = log(nt^2) / tmax
    dt = tmax / (nt - 1)
    v = irfft(y, nt)
    return [v[i] * exp(c * (i - 1) * dt) / dt for i = 1:nt]
end


"""
Calculates the soil parameters `σ(s)` and `εr(s)` based on the Smith-Longmire model
as presented in [1].

[1] D. Cavka, N. Mora, F. Rachidi, A comparison of frequency-dependent soil
models: application to the analysis of grounding systems, IEEE Trans.
Electromagn. Compat. 56 (February (1)) (2014) 177–187.

Parameters
----------
    σ0 : value of the soil conductivity in low frequency in S/m
    s : complex frequency `s = c + jω` of interest in rad/s
    erinf : parameter ε∞'

Returns
-------
    σ(s) : conductivity in S/m
    ϵr(s) : relative permitivitty
"""
function smith_longmire(s, sigma0, erinf=10)
    a = [3.4e6, 2.74e5, 2.58e4, 3.38e3, 5.26e2, 1.33e2, 2.72e1, 1.25e1,
         4.8e0, 2.17e0, 9.8e-1, 3.92e-1, 1.73e-1]
    N = length(a)
    Fdc = (125.0 * sigma0)^0.8312
    sum_epsr = 0.0
    sum_sigma = 0.0
    for i = 1:N
        F = Fdc * 10^(i - 1)
        fratio2 = (s / (2im * pi * F))^2
        den = (1.0 + fratio2)
        sum_epsr += a[i] / den
        sum_sigma += a[i] * F * (fratio2 / den)
    end
    epsr = erinf + sum_epsr;
    sigma = sigma0 + 2pi * EPS0 * sum_sigma;
    return sigma, epsr
end


"""
Calculates the soil parameters σ(s) and ε(s) based on the Alipio-Visacro soil
model [1].  
    `σ = σ0 + σ0 × h(σ0) × (s / (1 MHz))^g`  
    `εr = ε∞' / ε0 + tan(π g / 2) × 1e-3 / (2π ε0 (1 MHz)^g) × σ0 × h(σ0) s^(g - 1)`

Recommended values of `h(σ0)`, g and ε∞'/ε0 are given in Fig. 8 of [1]:

    | Results                 |          h(σ0)             |    g   |  ε∞'/ε0  |
    |:------------------------|:--------------------------:|:------:|:--------:|
    | mean                    |  1.26 × (1000 σ0)^(-0.73)  |  0.54  |    12    |
    | relatively conservative |  0.95 × (1000 σ0)^(-0.73)  |  0.58  |     8    |
    | conservative            |  0.70 × (1000 σ0)^(-0.73)  |  0.62  |     4    |

[1] R. Alipio and S. Visacro, "Modeling the Frequency Dependence of Electrical
Parameters of Soil," in IEEE Transactions on Electromagnetic Compatibility,
vol. 56, no. 5, pp. 1163-1171, Oct. 2014, doi: 10.1109/TEMC.2014.2313977.

Parameters
----------
    σ0 : value of the soil conductivity in low frequency in S/m
    s : complex frequency `s = c + jω` of interest in rad/s
    h : parameters `h(σ0)`
    g : parameter `g`
    eps_ratio : parameter `ε∞'/ε0`

Returns
-------
    σ(s) : conductivity in S/m
    ϵr(s) : relative permitivitty
"""
function alipio_soil(sigma0, s, h=1.26*(1000*sigma0)^(-0.73), g=0.54, eps_ratio=12)
    f = s / TWO_PI
    sigma = sigma0 + sigma0 * h * (f/1e6)^g
    t = tan(π * g / 2) / (TWO_PI * EPS0 * (1e6)^g)
    epsr = eps_ratio + t * sigma0 * h * f^(g - 1.0)
    return sigma, epsr
end


"""
Heidler function to create lightning current waveforms [1]. For parameters'
values, see e.g. [2]. Calculates  
    `i(t) = I0/ξ (t / τ1)^n / (1 + (t / τ1)^n) × exp(-t / τ2)`
where  
    `ξ = exp( -(τ1 / τ2) × (n τ2 / τ1)^(1 / n) )`

[1] HEIDLER, Fridolin; CVETIĆ, J. A class of analytical functions to study the
lightning effects associated with the current front. European transactions on
electrical power, v. 12, n. 2, p. 141-150, 2002. doi: 10.1002/etep.4450120209

[2] A. De Conti and S. Visacro, "Analytical Representation of Single- and
Double-Peaked Lightning Current Waveforms," in IEEE Transactions on
Electromagnetic Compatibility, vol. 49, no. 2, pp. 448-451, May 2007,
doi: 10.1109/TEMC.2007.897153.

Parameters
----------
    t : time in seconds
    imax : current peak I0 in A
    τ1 : rise time in seconds
    τ2 : decay time in seconds
    n : steepness expoent

Returns
-------
    i(t) : current in A
"""
function heidler(t, imax, tau1, tau2, n)
    xi = exp( -(tau1 / tau2) * ((n * tau2 / tau1)^(1.0 / n)) )
    tt1n = (t / tau1)^n
    return imax / xi * tt1n / (1 + tt1n) * exp(-t / tau2)
end


## HEM

"""
Defines which integration (and simplification thereof) to do.  
INTG_NONE: no integration is done, it is instead calculated as the distance
    between the middle points of the conductors.  
INTG_DOUBLE: performs the normal double integration along each conductor segment.  
INTG_SINGLE: performs the normal integration along only a single conductor segment.  
INTG_MHEM: calculates the integral of the modified HEM.
"""
@enum Integration_type begin
    INTG_NONE = 1
    INTG_DOUBLE = 2
    INTG_SINGLE = 3
    INTG_MHEM = 4
    INTG_MACLAURIN = 5
    INTG_PADE = 6
end


"""Defines a conductor segment."""
mutable struct Electrode
    start_point::Array{Float64,1}
    end_point::Array{Float64,1}
    middle_point::Array{Float64,1}
    length::Float64
    radius::Float64
end


"""Creates a conductor segment."""
function new_electrode(start_point, end_point, radius)
    return Electrode(start_point, end_point, (start_point + end_point)/2.0,
                     norm(start_point - end_point), radius)
end


"""Segments a conductor."""
function segment_electrode(electrode::Electrode, num_segments::Int)
    nn = num_segments + 1;
    nodes = Array{Float64}(undef, nn, 3); # FIXME transpose all nodes
    startp = Array{Float64,1}(undef, 3);
    endp = Array{Float64,1}(undef, 3);
    for k = 1:3
        startp[k] = electrode.start_point[k];
        endp[k] = electrode.end_point[k];
    end
    increment = (endp - startp)/num_segments;
    for k = 0:num_segments
        nodes[k+1,:] = startp + k*increment;
    end
    segments = Array{Electrode,1}(undef, num_segments);
    for k = 1:num_segments
        segments[k] = new_electrode(nodes[k,:], nodes[k+1,:], electrode.radius);
    end
    return segments, nodes
end


"""
Returns the row in B that matches a. If there is no match, nothing is returned.
taken and modified from https://stackoverflow.com/a/32740306/6152534
"""
function matchrow(a, B, atol=1e-9, rtol=0)
    findfirst(i -> all(j -> isapprox(a[j], B[i,j], atol=atol, rtol=rtol),
                       1:size(B,2)), 1:size(B,1))
end


"""
Segments a list of conductors such that they end up having at most `Lmax`
length. Returns a list of the segmented conductors and their nodes.
"""
function seg_electrode_list(electrodes, Lmax)
    num_elec = 0; #after segmentation
    for i=1:length(electrodes)
        #TODO store in array to avoid repeated calculations
        num_elec += Int(ceil(electrodes[i].length/Lmax));
    end
    elecs = Array{Electrode}(undef, num_elec);
    nodes = zeros(Float64, (2*num_elec, 3));
    e = 1;
    nodes = [];
    for i = 1:length(electrodes)
        ns = Int(ceil(electrodes[i].length/Lmax));
        new_elecs, new_nodes = segment_electrode(electrodes[i], ns);
        for k=1:ns
            elecs[e] = new_elecs[k];
            e += 1;
        end
        if (nodes == [])
            nodes = new_nodes;
        else
            for k = 1:size(new_nodes)[1]
                if (matchrow(new_nodes[k:k,:], nodes) == nothing)
                    nodes = cat(nodes, new_nodes[k:k,:], dims=1);
                end
            end
        end
    end
    return elecs, nodes
end


"""
Integrand that appears in the double integral between two electrodes.  
    `exp(-γ * r) / r`
"""
function integrand_double(sender::Electrode, receiver::Electrode, gamma, t)
    point_s = t[1]*(sender.end_point - sender.start_point) + sender.start_point;
    point_r = t[2]*(receiver.end_point - receiver.start_point) + receiver.start_point;
    r = norm(point_s - point_r);
    return exp(-gamma*r)/r
end


"""
Integrand that appears in the single integral between a sender electrode and
the middle point of a receiver electrode. 
    `exp(-γ * r) / r`
"""
function integrand_single(sender::Electrode, receiver::Electrode, gamma, t)
    point_s = t[1]*(sender.end_point - sender.start_point) + sender.start_point;
    r = norm(point_s - receiver.middle_point);
    return exp(-gamma*r)/r
end


"""Modified HEM integrand."""
function logNf(sender::Electrode, receiver::Electrode, gamma, t)
    point_r = t[1]*(receiver.end_point - receiver.start_point) + receiver.start_point;
    r1 = norm(point_r - sender.start_point);
    r2 = norm(point_r - sender.end_point);
    Nf = (r1 + r2 + sender.length)/(r1 + r2 - sender.length);
    return logabs(Nf)
end


"""Integral formula for when `γ*r -> 0` and sender -> receiver."""
function self_integral(sender)
    L = sender.length;
    b = sender.radius;
    k = sqrt(b^2 + L^2)
    return 2*(b - k) + L*log(1 + 2L*(L + k)/b^2)
end


"""Calculates `log( abs(x) )` limiting the result, if needed."""
function logabs(x)
    logabs_eps = -36.04365338911715; # = log(eps())
    absx = abs(x)
    if absx < eps()
        return logabs_eps
    elseif absx > 1/eps()
        return -logabs_eps
    else
        return log(absx)
    end
end


"""Integral between two electrodes by MacLaurin Series."""
function maclaurin(sender, receiver, gamma, nmax::Int, rtol)
    xs0 = sender.start_point[1];
    xs1 = sender.end_point[1];
    d0 = sender.start_point[3] - receiver.start_point[3];
    d1 = sender.end_point[3] - receiver.end_point[3];
    xr0 = receiver.start_point[1] + d0;
    xr1 = receiver.end_point[1] + d1;
    a =( xr0*logabs( (xr0 - xs1)/(xr0 - xs0) )
      +  xr1*logabs( (xr1 - xs0)/(xr1 - xs1) )
      +  xs0*logabs( (xr0 - xs0)/(xr1 - xs0) )
      +  xs1*logabs( (xr1 - xs1)/(xr0 - xs1) ) );
    N = (nmax > 0) ? (nmax) : typemax(Int);
    fac = big(1);
    for n = 1:N
        np1 = (n + 1);
        fac *= big(n);
        b =( (-gamma)^n/(n*np1*fac)
          *( abs(xr1 - xs0)^np1 - abs(xr1 - xs1)^np1
            -abs(xr0 - xs0)^np1 + abs(xr0 - xs1)^np1) );
        c1 = nmax <= 0 && abs(b/a) < rtol
        a += b;
        c1 && break
    end
    return a
end


"""Integral between two electrodes by Pade Approximant."""
function pade(sender, receiver, gamma)
    xs0 = sender.start_point[1];
    xs1 = sender.end_point[1];
    d0 = sender.start_point[3] - receiver.start_point[3];
    d1 = sender.end_point[3] - receiver.end_point[3];
    xr0 = receiver.start_point[1] + d0;
    xr1 = receiver.end_point[1] + d1;
    γ = gamma;
    a =(-xr1*logabs(xs0 - xr1) + xr1*logabs(xs1 - xr1)
      +  xs0*logabs(xr1 - xs0) - xs1*logabs(xr1 - xs1)
      +  xr0*logabs(xs0 - xr0) - xr0*logabs(xs1 - xr0)
      -  xs0*logabs(xr0 - xs0) + xs1*logabs(xr0 - xs1) );
    b =(-xs0*logabs(2 + γ*(xs0 - xr1))
      + (xr1 - 2/γ)*logabs(2 + γ*(xs0 - xr1))
      - (xr1 - 2/γ)*logabs(2 + γ*(xs1 - xr1))
      +  xs1*logabs(2 + γ*(xs1 - xr1))
      +  xs0*logabs(2 + γ*(xs0 - xr0))
      - (xr0 - 2/γ)*logabs(2 + γ*(xs0 - xr0))
      + (xr0 - 2/γ)*logabs(2 + γ*(xs1 - xr0))
      -  xs1*logabs(2 + γ*(xs1 - xr0)) );
    return -(a + 2b)
end


"""Performs the integral between two electrodes with customizable parameters."""
function integral(sender::Electrode, receiver::Electrode, gamma,
                  intg_type=INTG_DOUBLE, max_eval=typemax(Int),
                  atol=0, rtol=sqrt(eps(Float64)), error_norm=norm, initdiv=1)
    ls = sender.length;
    lr = receiver.length;
    if (intg_type == INTG_NONE)
        intg = norm(sender.middle_point - receiver.middle_point);
        #intg = exp(-gamma*r)/r*ls*lr;
    elseif (intg_type == INTG_DOUBLE)
        f(t) = integrand_double(sender, receiver, gamma, t);
        intg, err = hcubature(f, [0., 0.], [1., 1.], norm=norm, rtol=rtol,
                              atol=atol, maxevals=max_eval, initdiv=initdiv);
        intg = intg*ls*lr;
    elseif (intg_type == INTG_SINGLE)
        g(t) = integrand_single(sender, receiver, gamma, t);
        intg, err = hcubature(g, [0.], [1.], norm=norm, rtol=rtol,
                              atol=atol, maxevals=max_eval, initdiv=initdiv);
        intg = intg*ls;
    elseif (intg_type == INTG_MHEM)
        h(t) = logNf(sender, receiver, gamma, t);
        intg, err = hcubature(h, [0.], [1.], norm=norm, rtol=rtol, atol=atol,
                              maxevals=max_eval, initdiv=initdiv);
        intg = intg*ls;
    elseif (intg_type == INTG_MACLAURIN)
          intg = maclaurin(sender, receiver, gamma, max_eval, rtol);
    elseif (intg_type == INTG_PADE)
          #intg = pade(sender, receiver, gamma); # certo
        intg = -pade(receiver, sender, gamma); # errado
      else
        msg = join(["Unidentified integration type: ", intg_type]);
        throw(ArgumentError(msg))
    end
    return intg
end


"""
Calculates the impedance matrics `ZL` and `ZT` through in-place modification
of them. `ZL` and `ZT` are assumed symmetric and only the lower half of them is set.
"""
function calculate_impedances!(zl, zt, electrodes, gamma, s, mur, kappa,
                               max_eval=typemax(Int), atol=0,
                               rtol=sqrt(eps(Float64)), error_norm=norm,
                               intg_type=INTG_DOUBLE, initdiv=1)
    if (intg_type == INTG_MHEM || intg_type == INTG_NONE)
        iwu_4pi = 1.0;
        one_4pik = 1.0;
    else
        iwu_4pi = s * mur * MU0 / (FOUR_PI);
        one_4pik = 1.0 / (FOUR_PI * kappa);
    end
    ns = length(electrodes);
    for i = 1:ns
        v1 = electrodes[i].end_point - electrodes[i].start_point;
        ls = electrodes[i].length;
        if intg_type == INTG_DOUBLE
            r = electrodes[i].radius;
            sender = new_electrode([0, 0, 0], [ls, 0, 0], r);
            receiver = new_electrode([0, r, 0], [ls, r, 0], r);
            intg = integral(sender, receiver, gamma, intg_type,
                            max_eval, atol, rtol, error_norm, initdiv);
        else
            intg = self_integral(electrodes[i]);
        end
        zl[i,i] = iwu_4pi*intg;
        zt[i,i] = one_4pik/(ls^2)*intg;
        for k = (i+1):ns
            v2 = electrodes[k].end_point - electrodes[k].start_point;
            lr = electrodes[k].length;
            cost = dot(v1,v2)/(ls*lr);
            intg = integral(electrodes[i], electrodes[k], gamma, intg_type,
                            max_eval, atol, rtol, error_norm, initdiv);
            zl[k,i] = iwu_4pi*intg*cost;
            zt[k,i] = one_4pik/(ls*lr)*intg;
        end
    end
    return zl, zt
end


"""
Calculates the impedance matrics `ZL` and `ZT`.
`ZL` and `ZT` are assumed symmetric and only the lower half of them is set.
"""
function calculate_impedances(electrodes, gamma, s, mur, kappa,
                              max_eval=typemax(Int), atol=0,
                              rtol=sqrt(eps(Float64)), error_norm=norm,
                              intg_type=INTG_DOUBLE, initdiv=1)
    ns = length(electrodes);
    zl = Array{ComplexF64}(undef, (ns,ns));
    zt = Array{ComplexF64}(undef, (ns,ns));
    calculate_impedances!(zl, zt, electrodes, gamma, s, mur, kappa,
                          max_eval, atol, rtol, error_norm, intg_type, initdiv);
    return zl, zt
end


"""
Adds the effect of the images in the impedance matrics `ZL` and `ZT` through
in-place modification of them.
`ZL` and `ZT` are assumed symmetric and only the lower half of them is used.
"""
function impedances_images!(zli, zti, electrodes, images, gamma, s, mur, kappa,
                            ref_l, ref_t, max_eval=typemax(Int), atol=0,
                            rtol=sqrt(eps(Float64)), error_norm=norm,
                            intg_type=INTG_DOUBLE, initdiv=1)
    if (intg_type == INTG_MHEM || intg_type == INTG_NONE)
        iwu_4pi = 1.0;
        one_4pik = 1.0;
    else
        iwu_4pi = s * mur * MU0 / (FOUR_PI) * ref_t;
        one_4pik = 1.0 / (FOUR_PI * kappa) * ref_l;
    end
    ns = length(electrodes);
    for i = 1:ns
        v1 = electrodes[i].end_point - electrodes[i].start_point;
        ls = electrodes[i].length;
        for k = i:ns
            v2 = electrodes[k].end_point - electrodes[k].start_point;
            lr = electrodes[k].length;
            cost = dot(v1,v2)/(ls*lr);
            intg = integral(electrodes[i], images[k], gamma, intg_type,
                            max_eval, atol, rtol, error_norm, initdiv);
            zli[k,i] += iwu_4pi*intg*cost;
            zti[k,i] += one_4pik/(ls*lr)*intg;
        end
    end
end


"""
Adds the effect of the images in the impedance matrics `ZL` and `ZT`.
`ZL` and `ZT` are assumed symmetric and only the lower half of them is used.
"""
function impedances_images(electrodes, images, gamma, s, mur, kappa,
                           ref_l, ref_t, max_eval=typemax(Int), atol=0,
                           rtol=sqrt(eps(Float64)), error_norm=norm,
                           intg_type=INTG_DOUBLE, initdiv=1)
    ns = length(electrodes);
    zli = zeros(ComplexF64, ns, ns);
    zti = zeros(ComplexF64, ns, ns);
    impedances_images!(zli, zti, electrodes, images, gamma, s, mur, kappa,
                       ref_l, ref_t, max_eval, atol, rtol,
                       error_norm, intg_type, initdiv);
    return zli, zti
end


"""
Builds incidence matrices A and B for calculating the nodal admittance matrix:  
`YN = AT*inv(ZL)*A + BT*inv(ZT)*B`
"""
function incidence(electrodes, nodes; atol=0, rtol=1e-4)
    ns = length(electrodes);
    nn = size(nodes)[1];
    a = zeros(ComplexF64, (ns,nn));
    b = zeros(ComplexF64, (ns,nn));
    for k = 1:nn
        for i = 1:ns
            if isapprox(collect(electrodes[i].start_point), nodes[k,:],
                        atol=atol, rtol=rtol)
                a[i,k] = 1.0;
                b[i,k] = 0.5;
            elseif isapprox(collect(electrodes[i].end_point), nodes[k,:],
                            atol=atol, rtol=rtol)
                a[i,k] = -1.0;
                b[i,k] = 0.5;
            end
        end
    end
    return a, b
end


"""
Builds the Nodal Admittance matrix `YN` using low level BLAS and LAPACK for
in-place modification of the inputs `YN`, `ZL` and `ZT`.
`ZL` and `ZT` are assumed symmetric and only the lower half of them is used.
An auxiliary `C` matrix of size `(num_electrodes, num_nodes)` can be provided
to store the intermediate results.
"""
function admittance!(yn, zl, zt, a, b, c=nothing)
    ns, nn = size(a);
    if c === nothing
        c = Array{ComplexF64}(undef, (ns, nn));
    end
    uplo = 'L'
    zl, ipiv, info = LAPACK.sytrf!(uplo, zl);
    LAPACK.sytri!(uplo, zl, ipiv);
    zt, ipiv, info = LAPACK.sytrf!(uplo, zt);
    LAPACK.sytri!(uplo, zt, ipiv);
    BLAS.symm!(uplo, 'L', complex(1.0), zl, a, complex(0.0), c)  # mC := inv(zl)*mA + mC*0
    BLAS.gemm!('T', 'N', complex(1.0), a, c, complex(0.0), yn)  # yn := mAT*mC + yn*0
    BLAS.symm!(uplo, 'L', complex(1.0), zt, b, complex(0.0), c)  # mC := inv(zt)*mB + mC*0
    BLAS.gemm!('T', 'N', complex(1.0), b, c, complex(1.0), yn)  # yn := mBT*mC + yn
    return yn
end


"""
Builds the Nodal Admittance matrix.
`ZL` and `ZT` are assumed symmetric and only the lower half of them is used.
"""
function admittance(zl, zt, a, b)
    ns, nn = size(a);
    yn = Array{ComplexF64}(undef, (nn, nn));
    return admittance!(yn, copy(zl), copy(zt), a, b)
end


"""
Builds the Global Immittance matrix using low level BLAS and LAPACK for
in-place modification of the input `wg`.
"""
function immittance!(wg, zl, zt, a, b, ye=nothing)
    ns, nn = size(a);
    m0 = zeros(ComplexF64, ns, ns);
    if ye == nothing
        ye = view(m0, 1:nn, 1:nn);
    end
    p1 = nn + 1;
    p2 = nn + ns;
    p3 = 2ns + nn;
    @views begin
        wg[1:nn, 1:nn] = ye;
        wg[p1:p2, 1:nn] = -a;
        wg[(p2+1):p3, 1:nn] = -b;
        wg[1:nn, p1:p2] = transpose(a);
        wg[p1:p2, p1:p2] = zl;
        wg[(p2+1):p3, p1:p2] = m0;
        wg[1:nn, (p2+1):p3] = transpose(b);
        wg[p1:p2, (p2+1):p3] = m0;
        wg[(p2+1):p3, (p2+1):p3] = zt;
    end
    return wg
end


"""Builds the Global Immittance matrix."""
function immittance(zl, zt, a, b, ye=nothing)
    ns, nn = size(a);
    m = 2ns + nn;
    wg = Array{ComplexF64}(undef, m, m);
    return immittance!(wg, zl, zt, a, b, ye)
end


## Grid specialized routines

"""
Strutcture to represent a rectangular grid to be used in specialized routines.
This grid has dimensions `(Lx*Ly)`, a total of (before segmentation)
    `nv = (vx*vy)`
vertices and
    `ne = vy*(vx - 1) + vx*(vy - 1)`
edges. Each edge is divided into N segments so that the total number of nodes
after segmentation is
    `nn = vx*vy + vx*(vy - 1)*(Ny - 1) + vy*(vx - 1)*(Nx - 1)`
and the total number of segments is
    `ns = Nx*vx*(vy - 1) + Ny*vy*(vx - 1)`

    1           vx
    o---o---o---o  1
    |   |   |   |
    o---o---o---o
    |   |   |   |
    o---o---o---o  vy
    |<-- Lx --->|
    
Attributes
----------
    vertices_x : vx, number of vertices in the X direction;
    vertices_y : vy, number of vertices in the Y direction;
    length_x : Lx, total grid length in the X direction;
    length_y : Ly, total grid length in the Y direction;
    edge_segments_x : Nx, number of segments that each edge in the X direction has;
    edge_segments_y : Ny, number of segments that each edge in the Y direction has.
    radius : conductors' radius
    depth : z-coordinate of the grid
"""
struct Grid
    vertices_x::Int
    vertices_y::Int
    length_x::Float64
    length_y::Float64
    edge_segments_x::Int
    edge_segments_y::Int
    radius::Float64
    depth::Float64
end


"""Returns the number of segments the Grid has."""
function num_segments(grid::Grid)
    N = grid.edge_segments_x;
    vx = grid.vertices_x;
    M = grid.edge_segments_y;
    vy = grid.vertices_y;
    return ( N*vy*(vx - 1) + M*vx*(vy - 1) )
end


"""Returns the number of nodes the Grid has."""
function num_nodes(grid::Grid)
    N = grid.edge_segments_x;
    vx = grid.vertices_x;
    M = grid.edge_segments_y;
    vy = grid.vertices_y;
    return ( vx*vy + vx*(vy - 1)*(M - 1) + vy*(vx - 1)*(N - 1) )
end


"""Generates a list of electrodes and nodes from the Grid."""
function electrode_grid(grid)
    N = grid.edge_segments_x;
    Lx = grid.length_x;
    vx = grid.vertices_x;
    lx = Lx/(N*(vx - 1));
    M = grid.edge_segments_y;
    Ly = grid.length_y;
    vy = grid.vertices_y;
    ly = Ly/(M*(vy - 1));
    num_seg_horizontal = N*vy*(vx - 1);
    num_seg_vertical = M*vx*(vy - 1);
    num_seg = num_seg_horizontal + num_seg_vertical;

    num_elec = N*vy*(vx - 1) + M*vx*(vy - 1);
    num_nodes = vx*vy + vx*(vy - 1)*(M - 1) + vy*(vx - 1)*(N - 1);
    electrodes = Array{Electrode}(undef, num_elec);
    nodes = Array{Float64}(undef, 3, num_nodes);
    nd = 1;
    ed = 1;
    # Make horizontal electrodes
    for h = 1:vy
        for n = 1:(vx - 1)
            for k = 1:N
                x0 = lx*(N*(n - 1) + k - 1);
                y0 = ly*M*(h - 1);
                start_point = [x0, y0, grid.depth];
                end_point = [x0 + lx, y0, grid.depth];
                electrodes[ed] = new_electrode(start_point, end_point, grid.radius);
                ed += 1;
                if (n == 1 && k == 1)
                    nodes[1, nd] = start_point[1];
                    nodes[2, nd] = start_point[2];
                    nodes[3, nd] = start_point[3];
                    nd += 1;
                end
                nodes[1, nd] = end_point[1];
                nodes[2, nd] = end_point[2];
                nodes[3, nd] = end_point[3];
                nd += 1;
            end
        end
    end
    # Make vertical electrodes
    for g = 1:vx
        for m = 1:(vy - 1)
            for k = 1:M
                x0 = lx*N*(g - 1);
                y0 = ly*(M*(m - 1) + k - 1);
                start_point = [x0, y0, grid.depth];
                end_point = [x0, y0 + ly, grid.depth];
                electrodes[ed] = new_electrode(start_point, end_point, grid.radius);
                ed += 1;
                if (k < M)
                    nodes[1, nd] = end_point[1];
                    nodes[2, nd] = end_point[2];
                    nodes[3, nd] = end_point[3];
                    nd += 1;
                end
            end
        end
    end
    return electrodes, transpose(nodes)
end


"""
Makes a column and line permutation copy, depending on the values of
pc and pl. If both are false, then makes a plain copy.
    The line i of the matrix is permuted with line (N - i + 1).
    The column k of the matrix is permuted with column (M - k + 1).

Parameters
----------
    dest : destination array (where the copy of the permutated matrix is stored)
    src : source array (the matrix to be copied and permuted)
    pc : permute columns?
    pl : permute lines?
"""
function pcl!(dest, src; pc=true, pl=true)
    n, m = size(src)
    n0, m0 = size(dest)
    if n != n0 || m != m0
        msg = "Dimensions of dest and src arrays do not match."
        throw(ArgumentError(msg))
    end
    for k = 1:m
        for i = 1:n
            if pc && pl
                dest[i, k] = src[n-i+1, m-k+1]
            elseif pc
                dest[i, k] = src[i, m-k+1]
            elseif pl
                dest[i, k] = src[n-i+1, k]
            else
                dest[i, k] = src[i, k]
            end
        end
    end
end


"""
Column permutation copy.
    The column `k` of the matrix is permuted with column `(M - k + 1)`.

Parameters
----------
    dest : destination array (where the copy of the permutated matrix is stored)
    src : source array (the matrix to be copied and permuted)
"""
function pc!(dest, src)
    pcl!(dest, src; pc=true, pl=false)
end


"""
Line permutation copy.
    The line `i` of the matrix is permuted with line `(N - i + 1)`.

Parameters
----------
    dest : destination array (where the copy of the permutated matrix is stored)
    src : source array (the matrix to be copied and permuted)
"""
function pl!(dest, src)
    pcl!(dest, src; pc=false, pl=true)
end


"""
Specialized routine to build the impedance matrices `ZL` and `ZT` from a Grid
exploiting its geometric symmetry. The inputs `zl` and `zt` are modified.

See:
    Vieira, Pedro Henrique N., Rodolfo A. R. Moura, Marco Aurélio O. Schroeder and Antonio C. S. Lima.
    "Symmetry exploitation to reduce impedance evaluations in grounding grids."
    International Journal of Electrical Power & Energy Systems 123, 2020.
"""
function impedances_grid!(zl, zt, grid, gamma, s, mur, kappa,
                          max_eval=typemax(Int), atol=0,
                          rtol=sqrt(eps(Float64)), error_norm=norm,
                          intg_type=INTG_DOUBLE, initdiv=1, images=false)
    N = grid.edge_segments_x;
    Lx = grid.length_x;
    vx = grid.vertices_x;
    lx = Lx/(N*(vx - 1));
    M = grid.edge_segments_y;
    Ly = grid.length_y;
    vy = grid.vertices_y;
    ly = Ly/(M*(vy - 1));
    square = ((abs(lx - ly) < eps()) && (N == M) && (vx == vy));
    depth1 = grid.depth;
    if (images)
        depth2 = -depth1;
    else
        depth2 = depth1;
    end
    num_seg_horizontal = N*vy*(vx - 1);
    num_seg_vertical = M*vx*(vy - 1);
    num_seg = num_seg_horizontal + num_seg_vertical;
    seg_horizontal(h, n, k) = ((h - 1)*(vx - 1) + n - 1)*N + k;
    seg_vertical(m, g, k) = num_seg_horizontal + ((g - 1)*(vy - 1) + m - 1)*M + k;
    Z = zt;
    # first SEGMENT to all horizontal others: Z(X[1,1,1]; X[h2,n2,k2])
    sender = new_electrode([0., 0., depth1], [lx, 0., depth1], grid.radius);
    receiver = new_electrode([0., 0., depth2], [lx, 0., depth2], grid.radius);
    @views begin
        for h2 = 1:vy
            y0 = ly*M*(h2 - 1);
            receiver.start_point[2] = y0;
            receiver.end_point[2] = y0;
            receiver.middle_point[2] = y0;
            for n2 = 1:(vx - 1)
                for k2 = 1:N
                    x0 = lx*(N*(n2 - 1) + k2 - 1);
                    receiver.start_point[1] = x0;
                    receiver.end_point[1] = x0 + lx;
                    receiver.middle_point[1] = x0 + lx/2;
                    id2 = seg_horizontal(h2, n2, k2);
                    Z[1, id2] = integral(sender, receiver, gamma, intg_type,
                                         max_eval, atol, rtol, error_norm, initdiv);
                    Z[id2, 1] = Z[1, id2];
                end # for k2
            end # for n2
        end # for h2
        # first edge to itself: Z(X[1,1,k1]; X[1,1,k2])
        for k1 = 2:N
            for k2 = k1:N
                Z[k1, k2] = Z[k1 - 1, k2 - 1];
                Z[k2, k1] = Z[k1, k2];
            end
        end
        # first EDGE to all horizontal others: Z(X[1,1,k1]; X[h2,n2,k2])
        for k1 = 2:N
            for h2 = 1:vy
                for n2 = 1:(vx - 1)
                    for k2 = 1:N
                        id2 = seg_horizontal(h2, n2, k2);
                        if (n2 == 1 && k2 == 1)
                            id1 = seg_horizontal(h2, n2, k1);
                            Z[k1, id2] = Z[1, id1];
                        else
                            Z[k1, id2] = Z[k1 - 1, id2 - 1];
                        end
                        Z[id2, k1] = Z[k1, id2];
                    end # for k2
                end # for n2
            end # for h2
        end # for k1
        # other horizontal to horizontal edges: Z(X[h1,n1,k1]; X[h2,n2,k2])
        for h1 = 1:vy
            for n1 = 1:(vx - 1)
                if (h1 > 1 || n1 > 1) # skip first edge
                    id11 = seg_horizontal(h1, n1, 1);
                    id12 = id11 + N - 1;
                    for h2 = 1:vy
                        for n2 = 1:(vx - 1)
                            id21 = seg_horizontal(h2, n2, 1);
                            id22 = id21 + N - 1;
                            if (h1 <= h2 && n1 <= n2)
                                idx1 = seg_horizontal(h2 - h1 + 1, n2 - n1 + 1, 1);
                                idx2 = idx1 + N - 1;
                                pcl!(Z[id11:id12, id21:id22], Z[1:N, idx1:idx2],
                                     pc=false, pl=false);
                            elseif (h1 <= h2 && n1 > n2)
                                idx1 = seg_horizontal(h2 - h1 + 1, n1 - n2 + 1, 1);
                                idx2 = idx1 + N - 1;
                                pcl!(Z[id11:id12, id21:id22], transpose(Z[1:N, idx1:idx2]),
                                     pc=false, pl=false);
                            else
                                pcl!(Z[id11:id12, id21:id22],
                                     transpose(Z[id21:id22, id11:id12]),
                                     pc=false, pl=false);
                            end
                        end # for n2
                    end # for h2
                end # if
            end # for n1
        end # for h1
        # first EDGE to vertical ones: Z(X[1,1,k1]; Y[m1,g1,k2])
        receiver.length = ly;
        for k1 = 1:N
            x0 = lx*(k1 - 1);
            sender.start_point[1] = x0;
            sender.end_point[1] = x0 + lx;
            sender.middle_point[1] = x0 + lx/2;
            for g1 = 1:vx
                x0 = lx*N*(g1 - 1);
                receiver.start_point[1] = x0;
                receiver.end_point[1] = x0;
                receiver.middle_point[1] = x0;
                for m1 = 1:(vy - 1)
                    for k2 = 1:M
                        id2 = seg_vertical(m1, g1, k2);
                        c1 = (k1 > N/2 + 1);
                        c2 = (k1 > N/2) && (N%2 == 0);
                        c3 = (g1 == 1);
                        c4 = (g1 == 2);
                        if (c3 && (c1 || c2))
                            idx = seg_vertical(m1, 2, k2);
                            Z[k1, id2] = Z[N - k1 + 1, idx];
                        elseif (c4 && (c1 || c2))
                            idx = seg_vertical(m1, 1, k2);
                            Z[k1, id2] = Z[N - k1 + 1, idx];
                        else
                            y0 = ly*(M*(m1 - 1) + k2 - 1);
                            receiver.start_point[2] = y0;
                            receiver.end_point[2] = y0 + ly;
                            receiver.middle_point[2] = y0 + ly/2;
                            Z[k1, id2] = integral(sender, receiver, gamma,
                                                  intg_type, max_eval,
                                                  atol, rtol, error_norm, initdiv);
                        end # if
                        Z[id2, k1] = Z[k1, id2];
                    end # for k2
                end # for m1
            end # for g1
        end # for k1
        # other horizontal to vertical edges: Z(X[h1,n1,k1]; Y[m1,g1,k2])
        for h1 = 1:vy
            for n1 = 1:(vx - 1)
                id11 = seg_horizontal(h1, n1, 1);
                id12 = id11 + N - 1;
                if (h1 > 1 || n1 > 1) # skip first horizontal edge
                    for g1 = 1:vx
                        for m1 = 1:(vy - 1)
                            id21 = seg_vertical(m1, g1, 1);
                            id22 = id21 + M - 1;
                            if (h1 <= m1 && n1 <= g1)
                                idx1 = seg_vertical(m1 - h1 + 1, g1 - n1 + 1, 1);
                                idx2 = idx1 + M - 1;
                                pcl!(Z[id11:id12, id21:id22], Z[1:N, idx1:idx2],
                                     pc=false, pl=false);
                            elseif (h1 <= m1 && n1 > g1)
                                idx1 = seg_vertical(m1 - h1 + 1, n1 - g1 + 2, 1);
                                idx2 = idx1 + M - 1;
                                pl!(Z[id11:id12, id21:id22], Z[1:N, idx1:idx2]);
                            elseif (h1 > m1 && n1 <= g1)
                                idx1 = seg_vertical(h1 - m1, g1 - n1 + 1, 1);
                                idx2 = idx1 + M - 1;
                                pc!(Z[id11:id12, id21:id22], Z[1:N, idx1:idx2]);
                            else
                                idx1 = seg_vertical(h1 - m1, n1 - g1 + 2, 1);
                                idx2 = idx1 + M - 1;
                                pcl!(Z[id11:id12, id21:id22], Z[1:N, idx1:idx2]);
                            end
                            pcl!(Z[id21:id22, id11:id12],
                                 transpose(Z[id11:id12, id21:id22]),
                                 pc=false, pl=false);
                        end # for m1
                    end # for g1
                end # if
            end # for n1
        end # for h1
        if square # lx == ly && N == M && vx == vy
            id11 = seg_vertical(1, 1, 1);
            n = num_seg_horizontal;
            pcl!(Z[id11:end, id11:end], Z[1:n, 1:n], pc=false, pl=false);
        else
            # first vertical SEGMENT to all vertical others: Z(Y[1,1,1]; Y[m2,g2,k2])
            sender.start_point[1] = 0.0;
            sender.end_point[1] = 0.0;
            sender.middle_point[1] = 0.0;
            sender.start_point[2] = 0.0;
            sender.end_point[2] = ly;
            sender.middle_point[2] = ly/2;
            sender.length = ly;
            id1 = num_seg_horizontal + 1;
            for g2 = 1:vx
                x0 = lx*N*(g2 - 1);
                receiver.start_point[1] = x0;
                receiver.end_point[1] = x0;
                receiver.middle_point[1] = x0;
                for m2 = 1:(vy - 1)
                    for k2 = 1:M
                        y0 = ly*(M*(m2 - 1) + k2 - 1);
                        receiver.start_point[2] = y0;
                        receiver.end_point[2] = y0 + ly;
                        receiver.middle_point[2] = y0 + ly/2;
                        id1 = num_seg_horizontal + 1;
                        id2 = seg_vertical(m2, g2, k2);
                        Z[id1, id2] = integral(sender, receiver, gamma, intg_type,
                                               max_eval, atol, rtol, error_norm,
                                               initdiv);
                        Z[id2, id1] = Z[id1, id2];
                   end # for k2
               end # for m2
            end # for g2
            # first vertical edge to itself: Z(Y[1,1,k1]; Y[1,1,k2])
            for k1 = 2:M
                id1 = seg_vertical(1, 1, k1);
                for k2 = k1:M
                    id2 = seg_vertical(1, 1, k2);
                    Z[id1, id2] = Z[id1 - 1, id2 - 1];
                    Z[id2, id1] = Z[id1, id2];
                end
            end
            # first vertical EDGE to all vertical others: Z(Y[1,1,k1]; Y[g2,m2,k2])
            for k1 = 2:M
                id1 = seg_vertical(1, 1, k1);
                for g2 = 1:vx
                    for m2 = 1:(vy - 1)
                        for k2 = 1:M
                            id2 = seg_vertical(m2, g2, k2);
                            if (m2 == 1 && k2 == 1)
                                idk = seg_vertical(m2, g2, k1);
                                Z[id1, id2] = Z[seg_vertical(1, 1, 1), idk];
                            else
                                Z[id1, id2] = Z[id1 - 1, id2 - 1];
                            end
                            Z[id2, id1] = Z[id1, id2];
                        end # for k2
                    end # for m2
                end # for g2
            end # for k1
            # vertical to vertical edges: Z(Y[m1,g1,k1]; Y[m2,g2,k2])
            iv1 = num_seg_horizontal + 1;
            iv2 = iv1 + M - 1;
            for g1 = 1:vx
                for m1 = 1:(vy - 1)
                    id11 = seg_vertical(m1, g1, 1);
                    id12 = id11 + M - 1;
                    if (g1 > 1 || m1 > 1) # skip first vertical edge
                        for g2 = 1:vx
                            for m2 = 1:(vy - 1)
                                id21 = seg_vertical(m2, g2, 1);
                                id22 = id21 + M - 1;
                                if (g1 <= g2 && m1 <= m2)
                                    idx1 = seg_vertical(m2 - m1 + 1, g2 - g1 + 1, 1);
                                    idx2 = idx1 + M - 1;
                                    pcl!(Z[id11:id12, id21:id22], Z[iv1:iv2, idx1:idx2],
                                         pc=false, pl=false);
                                elseif (g1 <= g2 && m1 > m2)
                                    idx1 = seg_vertical(m1 - m2 + 1, g2 - g1 + 1, 1);
                                    idx2 = idx1 + M - 1;
                                    pcl!(Z[id11:id12, id21:id22],
                                         transpose(Z[iv1:iv2, idx1:idx2]),
                                         pc=false, pl=false);
                                else
                                    pcl!(Z[id11:id12, id21:id22],
                                         transpose(Z[id21:id22, id11:id12]),
                                         pc=false, pl=false);
                                end
                            end # for m2
                        end # for g2
                    end # if
                end # for m1
            end # for g1
        end # if square ************************************
        iwu_4pi = s*mur*MU0/(FOUR_PI);
        one_4pik = 1.0/(FOUR_PI*kappa);
        one_4pik_lx2 = one_4pik/(lx^2);
        one_4pik_ly2 = one_4pik/(ly^2);
        one_4pik_lylx = one_4pik/(ly*lx);
        n = num_seg_horizontal;
        for k = 1:n
            for i = 1:n
                zl[i, k] = iwu_4pi*Z[i, k];
                zt[i, k] = one_4pik_lx2*Z[i, k];
            end
            for i = (n+1):num_seg
                zl[i, k] = 0.0;
                zt[i, k] = one_4pik_lylx*Z[i, k];
            end
        end
        for k = (n+1):num_seg
            for i = 1:n
                zl[i, k] = 0.0;
                zt[i, k] = one_4pik_lylx*Z[i, k];
            end
            for i = (n+1):num_seg
                zl[i, k] = iwu_4pi*Z[i, k];
                zt[i, k] = one_4pik_ly2*Z[i, k];
            end
        end
    end # @views
end


"""
Specialized routine to build the impedance matrices `ZL` and `ZT` from a Grid
exploiting its geometric symmetry.
"""
function impedances_grid(grid, gamma, s, mur, kappa, max_eval=typemax(Int),
                         atol=0, rtol=sqrt(eps(Float64)),
                         error_norm=norm, intg_type=INTG_DOUBLE, initdiv=1,
                         images=false)
    num_seg = num_segments(grid);
    zl = Array{ComplexF64}(undef, num_seg, num_seg);
    zt = Array{ComplexF64}(undef, num_seg, num_seg);
    impedances_grid!(zl, zt, grid, gamma, s, mur, kappa,
                     max_eval, atol, rtol, error_norm,
                     intg_type, initdiv, images);
    return zl, zt
end


## Other symmetries

"""
Calculate the impedance matrices taking advantage of the geometric symmetry
of a single straight conductor of radius r and total length `L` divided into
`num_seg` segments.

The argument `imag_dist` is the distance to the images. If zero, then the
impedances to the "real" segments is calculated.
"""
function impedances_straight!(zl, zt, L, r, num_seg, gamma, s, mur, kappa,
                              ref_l=1, ref_t=1, imag_dist=0, max_eval=typemax(Int),
                              atol=0, rtol=sqrt(eps(Float64)),
                              error_norm=norm, intg_type=INTG_DOUBLE, initdiv=1)
    len = L/num_seg;
    iwu_4pi = s*mur*MU0/(FOUR_PI)*ref_l;
    one_4pikl2 = 1.0/(FOUR_PI*kappa*len^2)*ref_t;
    sender = new_electrode([0, 0, 0], [len, 0, 0], r);
    receiver = new_electrode([0, 0, imag_dist], [len, 0, imag_dist], r);
    if imag_dist < r
        if intg_type == INTG_DOUBLE
            receiver = new_electrode([0, r, 0], [len, r, 0], r);
            intg = integral(sender, receiver, gamma, intg_type,
                            max_eval, atol, rtol, error_norm, initdiv);
            #L = sender.length;
            #b = sender.radius;
            #k = sqrt(1 + (b/L)^2)
            #intg = 2L*(log((k + 1)/(b/L)) - k + b/L) # certo
            #intg = 2L*(log((k + 1)/(b/L) - k + b/L)) # errado
        else
            intg = self_integral(sender);
        end
    else
        intg = integral(sender, receiver, gamma, intg_type, max_eval,
                        atol, rtol, error_norm, initdiv);
    end
    zl[1,1] = iwu_4pi*intg;
    zt[1,1] = one_4pikl2*intg;
    for k = 2:num_seg
        receiver.end_point[1] += len;
        receiver.start_point[1] += len;
        receiver.middle_point[1] += len;
        intg = integral(sender, receiver, gamma, intg_type, max_eval,
                        atol, rtol, error_norm, initdiv);
        zl[k,1] = iwu_4pi*intg;
        zt[k,1] = one_4pikl2*intg;
        zl[1,k] = zl[k,1];
        zt[1,k] = zt[k,1];
    end
    for k = 2:num_seg
        for i = 2:num_seg
            zl[i,k] = zl[i-1, k-1];
            zt[i,k] = zt[i-1, k-1];
        end
    end
    return zl, zt
end

# Field computation

"""Calculates the vector magnetic potential A."""
function magnetic_potential(point, electrodes, il, gamma, mur)
    fake = new_electrode(point, point, 1)
    va = zeros(ComplexF64, 3)
    for i in eachindex(electrodes)
        integ = integral(electrodes[i], fake, gamma, INTG_MHEM)
        dx = electrodes[i].end_point - electrodes[i].start_point
        L = norm(dx)
        va .+= integ * il[i] / electrodes[i].length .* dx / L * mur * MU0 / (4.0 * pi)
    end
    return va
end


"""Calculates the electric field at a point. Uses mHEM aproximation."""
function electric_field_mhem(point, electrodes, il, it, gamma, s, mur, kappa)
    efield = zeros(ComplexF64, 3)
    fake = new_electrode(point, point, 1)
    for i in eachindex(electrodes)
        #integ = integral(electrodes[i], fake, gamma, INTG_MHEM)
        #rmean = norm(fake.middle_point - electrodes[i].middle_point)
        #ec = integ * (1.0 + gamma * rmean) * it[i] / (4.0 * pi * kappa)
        integ = integral(electrodes[i], fake, gamma, INTG_DOUBLE)
        ec = integ * (1.0 + gamma * rmean) * it[i] / (4.0 * pi * kappa)
        # EA should be multiplied by the electrode length L, but it is then
        # divided L to calculate the cosine with each axis. Hence, omitted.
        ea = integ * (-s) * mur * MU0 * il[i] / (4.0 * pi)
        for k = 1:3
            cosk = (electrodes[i].middle_point[k] - point[k]) / rmean
            efield[k] += ec * cosk
            cosl = (electrodes[i].end_point[k] - electrodes[i].start_point[k])
            efield[k] += ea * cosl
        end
    end
    return efield
end


"""Calculates the electric field at a point. Uses mHEM aproximation."""
function electric_field(point, electrodes, il, it, gamma, s, mur, kappa)
    efield = zeros(ComplexF64, 3)
    for i in eachindex(electrodes)
        function elec_field_integrand(t)
            p2 = electrodes[i].end_point
            p1 = electrodes[i].start_point
            dv = point - (t[1] * (p2 - p1) + p1)
            r = norm(dv)
            if r < eps()
                r = eps()
            end
            return (1 + gamma * r) * exp(-gamma * r) / r^3.0 .* dv
        end
        integ, error = hcubature(elec_field_integrand, [0.0], [1.0])
        efield .+= integ * it[i] / (4.0 * pi * kappa)
        if abs(s) > 0.0
            efield .+= (-s) .* magnetic_potential(point, electrodes, il, gamma, mur)
        end
    end
    return efield
end
