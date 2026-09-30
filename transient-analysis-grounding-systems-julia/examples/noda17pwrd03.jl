#=
Reproducing the results in [1] of a time domain surge response.

[1] Noda, Taku, and Shigeru Yokoyama. "Thin wire representation in finite difference time domain
surge simulation." IEEE Transactions on Power Delivery 17.3 (2002): 840-847.
=#
using LinearAlgebra
using FFTW
using Plots

include("../hem.jl");

function load_file(fname, cols=1, sep=",")
    stringvec = split(read(fname, String), "\n");
    m = map(x -> map(i -> parse(Float64, i), split(x, sep)), stringvec[1:end-1]);
    m2 = collect(Iterators.flatten(m));
    return transpose(reshape(transpose(m2), cols, Int(length(m2)/cols)));
end;

function laplace_transform(f::Vector{ComplexF64}, t::Vector{Float64}, s::Vector{ComplexF64})
    nt = length(t);
    nf = length(s);
    res = zeros(Complex{Float64}, nf);
    for k = 1:nf
        for i = 1:(nt-1)
            e0 = exp(s[k]*t[i]);
            e1 = exp(s[k]*t[i+1]);
            dt = t[i+1] - t[i];
            x = (f[i+1] - f[i])/s[k];
            res[k] += (e1*(f[i+1]*dt - x) - e0*(f[i]*dt - x))/dt;
        end
        res[k] = 1/s[k]*res[k];
    end
    return res
end;

function simulate(mhem=true)
    ## Parameters
    mu0 = pi*4e-7;
    mur = 1;
    eps0 = 8.854e-12;
    epsr = 1;
    sigma1 = 0;
    #rhoc = 1.9 10^-6;
    rhoc = 1.68e-8;
    sigma_cu = 1/rhoc;
    rho_lead = 2.20e-7;
    sigma_lead = 1/rho_lead;

    rsource = 50.0;
    gf = 1.0/rsource;
    rh = 15e-3;
    rv = 10e-3;
    h = 0.5;
    l = 4.0;

    #= Frequencies
    Due to numerical erros, to smooth the response, its necessary to use a
    final time much greater than that up to which is desired.
    =#
    T = 0.7e-7*2;
    dt = 2.0e-9;
    n = T/dt;
    t = collect(0.0:dt:(T-dt));
    sc = log(n^2)/T;
    kk = collect(0:1:n/2);
    dw = 2.0*pi/(n*dt);
    function sigma(j, alpha=0.53836)
        return alpha + (1 - alpha)*cos(2*pi*j/n)
    end;
    sk = -1im*sc*ones(length(kk)) + dw*kk;
    nf = length(sk);
    freq = real(sk)/(2*pi);
    omega = 2*pi*freq[nf];
    lambda = (2*pi/omega)*(1/sqrt( epsr*eps0*mu0/2*(1 + sqrt(1 + (sigma1/(omega*epsr*eps0))^2)) ));

    ## Electrodes
    x = 10;
    nv = Int(ceil(h/(lambda/x)));
    nh = Int(ceil(l/(lambda/x)));
    vertical = new_electrode([0, 0, 0], [0, 0, 0.5], 10e-3);
    horizontal = new_electrode([0, 0, 0.5], [4.0, 0, 0.5], 15e-3);
    elecv, nodesv = segment_electrode(vertical, Int(nv));
    elech, nodesh = segment_electrode(horizontal, Int(nh));
    electrodes = elecv;
    append!(electrodes, elech);
    ns = length(electrodes);
    nodes = cat(nodesv[1:end-1,:], nodesh, dims=1);
    nn = size(nodes)[1];

    #create images
    images = Array{Electrode}(undef, ns);
    for i=1:ns
        start_point = [electrodes[i].start_point[1],
                       electrodes[i].start_point[2],
                       -electrodes[i].start_point[3]];
        end_point = [electrodes[i].end_point[1],
                     electrodes[i].end_point[2],
                         -electrodes[i].end_point[3]];
        r = electrodes[i].radius;
        images[i] = new_electrode(start_point, end_point, r);
    end

    mA, mB = incidence(electrodes, nodes);
    exci = Array{ComplexF64}(undef, nn);
    zh = Array{ComplexF64}(undef, nf);
    zl = Array{ComplexF64}(undef, (ns,ns));
    zt = Array{ComplexF64}(undef, (ns,ns));
    zli = Array{ComplexF64}(undef, (ns,ns));
    zti = Array{ComplexF64}(undef, (ns,ns));
    yn = Array{ComplexF64}(undef, (nn,nn));
    vout = zeros(Complex{Float64}, (nf,nn));

    ## Source input
    if Sys.iswindows()
        path = "examples\\noda17pwrd03_auxfiles\\";
    else
        path = "examples/noda17pwrd03_auxfiles/";
    end
    source = load_file(join([path, "source.txt"]), 2);
    source[:,1] = source[:,1]*1e-9;
    vout_art = load_file(join([path, "voltage.txt"]), 2);
    iout_art = load_file(join([path, "current.txt"]), 2);
    ent_freq = laplace_transform(Vector{ComplexF64}(source[:,2]),
                                 Vector{Float64}(source[:,1]), -1.0im*sk);

    # Integration Parameters
    max_eval = typemax(Int);
    req_abs_error = 1e-3;
    req_rel_error = 1e-4;
    error_norm = norm;
    if mhem
        intg_type = INTG_MHEM;
        # calculate distances to avoid repetition
        rbar = Array{Float64}(undef, (ns,ns))
        rbari = copy(rbar)
        for k = 1:ns
            p1 = collect(electrodes[k].middle_point)
            for i = k:ns
                p2 = collect(electrodes[i].middle_point)
                p3 = collect(images[i].middle_point)
                rbar[i,k] = norm(p1 - p2)
                rbari[i,k] = norm(p1 - p3)
            end
        end
        mpotzl, mpotzt = calculate_impedances(electrodes, 0.0, 1.0, 1.0, 1.0,
                                              max_eval, req_abs_error,
                                              req_rel_error, error_norm,
                                              intg_type);
        mpotzli, mpotzti = impedances_images(electrodes, images,
                                             0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                                             max_eval, req_abs_error,
                                             req_rel_error, error_norm,
                                             intg_type);
    else
        intg_type = INTG_DOUBLE;
    end
    ## Freq. loop
    for f = 1:nf
        jw = 1.0im*sk[f];
        kappa = jw*eps0;
        k1 = sqrt(jw*mu0*kappa);
        kappa_cu = sigma_cu + jw*epsr*eps0;
        ref_t = (kappa - kappa_cu)/(kappa + kappa_cu);
        ref_l = ref_t;
        if mhem
            iwu_4pi = jw * MU0 / (FOUR_PI);
            one_4pik = 1.0 / (FOUR_PI * kappa);
            for k=1:ns
                for i=k:ns
                    zl[i,k] = exp(-k1 * rbar[i,k]) * iwu_4pi * mpotzl[i,k];
                    zt[i,k] = exp(-k1 * rbar[i,k]) * one_4pik * mpotzt[i,k];
                    zl[i,k] += ref_l * exp(-k1 * rbari[i,k]) * iwu_4pi * mpotzli[i,k];
                    zt[i,k] += ref_t * exp(-k1 * rbari[i,k]) * one_4pik * mpotzti[i,k];
                end
            end
        else
            calculate_impedances!(zl, zt, electrodes, k1, jw, mur, kappa,
                                  max_eval, req_abs_error, req_rel_error,
                                  error_norm, intg_type);
            impedances_images!(zl, zt, electrodes, images, k1, jw, mur, kappa,
                               ref_l, ref_t, max_eval, req_abs_error,
                               req_rel_error, error_norm, intg_type);
        end
        exci .= 0.0;
        exci[1] = ent_freq[f]*gf;
        admittance!(yn, zl, zt, mA, mB)
        yn[1,1] += gf;
        ldiv!(lu!(yn), exci)
        vout[f,:] = exci[:];
    end;

    ## Time response
    outlow = map(i -> vout[Int(i+1), 1]*sigma(i), kk);
    upperhalf = reverse(conj(outlow));
    pop!(upperhalf);
    lowerhalf = copy(outlow);
    pop!(lowerhalf);
    append!(lowerhalf, upperhalf);
    F = lowerhalf;
    f = real(ifft(F));
    outv = map(i -> exp(sc*t[i])/dt*f[i], 1:length(t));
    # ======
    iout = -(vout[:,1] - ent_freq)*gf;
    outlow = map(i -> iout[Int(i+1),1]*sigma(i), kk);
    upperhalf = reverse(conj(outlow));
    pop!(upperhalf);
    lowerhalf = copy(outlow);
    pop!(lowerhalf);
    append!(lowerhalf, upperhalf);
    F = lowerhalf;
    f = real(ifft(F));
    outi = map(i -> exp(sc*t[i])/dt*f[i], 1:length(t));
    return outv, outi, source, vout_art, iout_art, t
end;

mhem = true;
outv, outi, source, vout_art, iout_art, t = @time simulate(mhem);

plot([t*1e9, source[:,1]*1e9, vout_art[:,1]], [outv, source[:,2], vout_art[:,2]],
     xlims = (0, 50), ylims = (0, 80), xlabel="t (ns)", ylabel="V (V)",
     label=["calculated" "source" "article"],
     color=["red" "green" "blue"], marker=true, title="Vout")

plot([t*1e9, iout_art[:,1]], [outi, iout_art[:,2]],
     xlims = (0, 50), ylims = (-0.2, 0.5), xlabel="t (ns)", ylabel="I (A)",
     label=["calculated" "article"],
     color=["red" "blue"], marker=true, title="Iout")
