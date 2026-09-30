#=
Reproducing the results in [1] for a grounding grid.

[1] L. D. Grcev and M. Heimbach, "Frequency dependent and transient
characteristics of substation grounding systems," in IEEE Transactions on
Power Delivery, vol. 12, no. 1, pp. 172-178, Jan. 1997.
doi: 10.1109/61.568238
=#
using Plots
include("../hem.jl");

"""
Runs the simulation.

Parameters
----------
    gs : size of the square grid, in [m]. Must be an integer multiple of 10
    freq : array of frequencies of interest
    Lmax : segments maximum length [m]
    mhem : use modified HEM formulation? See:
        Lima, A.C., Moura, R.A., Vieira, P.H., Schroeder, M.A., & Correia de Barros, M.T.
        "A Computational Improvement in Grounding Systems Transient Analysis."
        IEEE Transactions on Electromagnetic Compatibility, vol. 62, pp. 765-773, 2020.
    symmetry : exploit grid symmetry to calculate the impedances faster? See:
        Vieira, Pedro Henrique N., Rodolfo A. R. Moura, Marco Aurélio O. Schroeder and Antonio C. S. Lima.
        "Symmetry exploitation to reduce impedance evaluations in grounding grids."
        International Journal of Electrical Power & Energy Systems 123, 2020.

Returns
-------
    zh : Harmonic Impedance
"""
function simulate(gs::Int, freq, Lmax, mhem::Bool, symmetry::Bool)
    ## Parameters
    # Soil
    mu0 = MU0;
    mur = 1.0;
    eps0 = EPS0;
    epsr = 10;
    σ1 = 1.0/1000.0;
    # Frequencies
    nf = length(freq);
    #Ω = 2*pi*freq[nf];
    #λ = (2*pi/Ω)*(1/sqrt( epsr*eps0*mu0/2*(1 + sqrt(1 + (σ1/(Ω*epsr*eps0))^2)) ));

    # Grid
    r = 7e-3;
    h = -0.5;
    n = Int(gs/10) + 1;
    div = Int(ceil(10 / Lmax))
    grid = Grid(n, n, gs, gs, div, div, r, h);
    electrodes, nodes = electrode_grid(grid);
    ns = length(electrodes)
    nn = size(nodes)[1]
    println("GS", gs)
    println("Num. segments = ", ns)
    println("Num. nodes = ", nn)
    inj_node = matchrow([0.,0.,h], nodes)

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
    # Integration Parameters
    max_eval = typemax(Int);
    req_abs_error = 1e-4;
    req_rel_error = 1e-5;
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
        if symmetry
            mpotzl, mpotzt = impedances_grid(grid, 0.0, 1.0, 1.0, 1.0, max_eval,
                                             req_abs_error, req_rel_error,
                                             error_norm, intg_type);
            mpotzli, mpotzti = impedances_grid(grid, 0.0, 1.0, 1.0, 1.0, max_eval,
                                              req_abs_error, req_rel_error,
                                              error_norm, intg_type, 1, true);
        else
            mpotzl, mpotzt = calculate_impedances(electrodes, 0.0, 1.0, 1.0, 1.0,
                                                  max_eval, req_abs_error,
                                                  req_rel_error, error_norm,
                                                  intg_type);
            mpotzli, mpotzti = impedances_images(electrodes, images,
                                                 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                                                 max_eval, req_abs_error,
                                                 req_rel_error, error_norm,
                                                 intg_type);
        end
    else
        intg_type = INTG_DOUBLE;
    end
    mA, mB = incidence(electrodes, nodes);
    zh = Array{ComplexF64}(undef, nf);
    # Frequency loop, Run in parallel:
    BLAS.set_num_threads(1)  # this is important! We want to multithread the frequency loop, not the matrix operations
    zls = [Array{ComplexF64}(undef, (ns,ns)) for t = 1:Threads.nthreads()]
    zts = [Array{ComplexF64}(undef, (ns,ns)) for t = 1:Threads.nthreads()]
    ies = [Array{ComplexF64}(undef, nn) for t = 1:Threads.nthreads()]
    yns = [Array{ComplexF64}(undef, (nn,nn)) for t = 1:Threads.nthreads()]
    mCs = [Array{ComplexF64}(undef, (ns,nn)) for t = 1:Threads.nthreads()]  # auxiliary matrix
    if symmetry
        zlis = [Array{ComplexF64}(undef, (ns,ns)) for t = 1:Threads.nthreads()]
        ztis = [Array{ComplexF64}(undef, (ns,ns)) for t = 1:Threads.nthreads()]
    end
    # alocate buffers per task, not per thread. This is correct regardless of scheduler, thread pool, or Julia version:
    Threads.@threads for f = 1:nf
        t = Threads.threadid()
        zl = Array{ComplexF64}(undef, (ns,ns))
        zt = Array{ComplexF64}(undef, (ns,ns))
        ie = Array{ComplexF64}(undef, nn)
        yn = Array{ComplexF64}(undef, (nn,nn))
        mC = Array{ComplexF64}(undef, (ns,nn))
        jw = 1.0im*TWO_PI*freq[f];
        kappa = σ1 + jw*epsr*eps0;
        k1 = sqrt(jw*mu0*kappa);
        kappa_air = jw*eps0;
        ref_t = (kappa - kappa_air)/(kappa + kappa_air);
        ref_l = 1.0;
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
            if symmetry
                zli = zlis[t]
                zti = ztis[t]
                zli .= 0.0
                zti .= 0.0
                impedances_grid!(zl, zt, grid, k1, jw, mur, kappa, max_eval,
                                 req_abs_error, req_rel_error, error_norm, intg_type);
                impedances_grid!(zli, zti, grid, k1, jw, mur, kappa, max_eval,
                                 req_abs_error, req_rel_error, error_norm,
                                 intg_type, 1, true);
                zl .+= ref_l.*zli;
                zt .+= ref_t.*zti;
            else
                calculate_impedances!(zl, zt, electrodes, k1, jw, mur, kappa,
                                      max_eval, req_abs_error, req_rel_error,
                                      error_norm, intg_type);
                impedances_images!(zl, zt, electrodes, images, k1, jw, mur, kappa,
                                   ref_l, ref_t, max_eval, req_abs_error,
                                   req_rel_error, error_norm, intg_type);
            end
        end
        ie .= 0.0;
        ie[inj_node] = 1.0;
        admittance!(yn, zl, zt, mA, mB, mC)
        ldiv!(lu!(yn), ie)
        zh[f] = ie[inj_node];
    end;
    return zh
end

precompile(simulate, (Int, Vector{Float64}, Float64, Bool, Bool))

if Threads.nthreads() == 1
    println("Using only 1 thread. Consider launching julia with multiple threads.")
    println("see:\n  https://docs.julialang.org/en/v1/manual/multi-threading/#man-multithreading")
end

nf = 100;
freq = exp10.(range(2, stop=6.4, length=nf)); #logspace
Lmax = 1.0;
mhem = true;
symmetry = false;
gs_arr = [10, 20, 30, 60, 120];
ng = length(gs_arr);
zh = Array{ComplexF64}(undef, nf, ng);
for i = 1:ng
    gs = gs_arr[i];
    @time zh[:,i] = simulate(gs, freq, Lmax, mhem, symmetry);
end

# Plot
colors = [:red, :blue, :black, :orange, :purple];

begin
    p = plot(xaxis=:log, legend=:topleft, xlabel="f (Hz)", ylabel="|Zh (Ω)|");
    for i = 1:ng
        plot!(freq, abs.(zh[:,i]), label=join(["GS ", gs_arr[i]]), linecolor=colors[i])
    end
    p
end

begin
    p = plot(xaxis=:log, legend=:topleft, xlabel="f (Hz)", ylabel="Phase Zh (deg)");
    for i = 1:ng
        plot!(freq, 180/π*angle.(zh[:,i]), label=join(["GS ", gs_arr[i]]), linecolor=colors[i])
    end
    p
end
