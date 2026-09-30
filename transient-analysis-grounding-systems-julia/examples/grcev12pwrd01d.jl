#=
Reproducing the results in [1] for a grounding grid.

[1] L. D. Grcev and M. Heimbach, "Frequency dependent and transient
characteristics of substation grounding systems," in IEEE Transactions on
Power Delivery, vol. 12, no. 1, pp. 172-178, Jan. 1997.
doi: 10.1109/61.568238
=#
# to test the integral routines removed GS120
using Plots
include("../hem1.jl");

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
    Ω = 2*pi*freq[nf];
    λ = (2*pi/Ω)*(1/sqrt( epsr*eps0*mu0/2*(1 + sqrt(1 + (σ1/(Ω*epsr*eps0))^2)) ));
    Lmax = min(Lmax, λ/10);
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
    # Allocate buffers per task, not per thread. This is correct regardless of scheduler,
    # thread pool, or Julia version. Using threadid() to index pre-allocated arrays is unsafe
    # because Threads.threadid() can return values up to Threads.maxthreadid() (which includes
    # the interactive thread pool), while Threads.nthreads() only counts the default pool.
    Threads.@threads for f = 1:nf
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
                zli = Array{ComplexF64}(undef, (ns,ns))
                zti = Array{ComplexF64}(undef, (ns,ns))
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

# NOTE on Julia 1.12 world-age semantics
# ---------------------------------------
# We deliberately do NOT call `precompile(simulate, ...)` here. In Julia 1.12,
# precompile captures a binding at one world age while the calling code may
# resolve it at another, producing the warning:
#   "Detected access to binding Main.simulate in a world prior to its definition world."
# Symptoms include stale cached method versions being called instead of the
# current source, which manifests as confusing errors at line numbers that no
# longer have the offending expression. The robust workaround is two-fold:
#   (1) Wrap the execution in a function (this `main()`). A function body is
#       compiled atomically and always sees the latest `simulate`.
#   (2) If you ever change `simulate`, restart Julia (in VSCode:
#       "Julia: Restart REPL" from the command palette) before re-running.

function main()
    if Threads.nthreads() == 1
        println("Using only 1 thread. Consider launching julia with multiple threads.")
        println("see:\n  https://docs.julialang.org/en/v1/manual/multi-threading/#man-multithreading")
    end

    nf = 100;
    freq = exp10.(range(2, stop=7, length=nf));   # logspace, up to 1e7 Hz
    Lmax = 1.0;
    mhem = true;        # set true for the modified-HEM formulation
    symmetry = true;    # set false to skip the symmetry-exploitation path
    gs_arr = [10, 20, 30, 60, 90];  # grid sizes to simulate, in [m]
    ng = length(gs_arr);
    zh = Array{ComplexF64}(undef, nf, ng);
    for i = 1:ng
        gs = gs_arr[i];
        @time zh[:, i] = simulate(gs, freq, Lmax, mhem, symmetry);
    end
    return freq, zh, gs_arr, ng
end

# Run the simulation. Returns to module scope so the plotting block below
# can read them without needing to be inside `main()`.
freq, zh, gs_arr, ng = main()

# ============================================================================
# Plotting
# ----------------------------------------------------------------------------
# Both panels are shown together in a single combined figure so the VSCode
# plot pane (which only auto-renders the last expression of the script) shows
# everything, and PDFs are written to disk so the figures survive `julia
# file.jl` exiting and are paper-ready.
# ============================================================================

# Okabe-Ito colorblind-safe palette. Skip the yellow #F0E442 (poor contrast on
# white). Distinguishable for all common forms of color vision deficiency and
# also when printed in greyscale.
const OKABE_ITO = ["#E69F00",  # orange
                   "#56B4E9",  # sky blue
                   "#009E73",  # bluish green
                   "#0072B2",  # blue
                   "#D55E00",  # vermillion
                   "#CC79A7",  # reddish purple
                   "#000000"]  # black

# Pair each color with a linestyle for redundant encoding (helps in B&W print
# and improves separability of overlapping curves on log axes).
const LINESTYLES = [:solid, :dash, :dot, :dashdot, :dashdotdot, :solid, :dash]

# Publication-style plot defaults. Set `fontfamily` to "Computer Modern" if
# the cm-unicode fonts are installed (typical with a TeX Live install);
# otherwise fall back to a generic serif.
default(fontfamily   = "Computer Modern",
        framestyle   = :box,
        grid         = true,
        gridalpha    = 0.30,
        minorgrid    = true,
        minorgridalpha = 0.15,
        linewidth    = 2,
        guidefontsize  = 12,
        tickfontsize   = 10,
        legendfontsize = 10,
        foreground_color_legend = nothing,  # no legend box border
        background_color_legend = RGBA(1, 1, 1, 0.85))

# --- Magnitude ---------------------------------------------------------------
p_mag = plot(xaxis = :log,
             legend = :topleft,
             xlabel = "Frequency (Hz)",
             ylabel = "|Z_h|  (Ω)",
             title  = "Harmonic Impedance — Magnitude")
for i = 1:ng
    plot!(p_mag, freq, abs.(zh[:, i]);
          label     = "GS $(gs_arr[i]) m",
          linecolor = OKABE_ITO[i],
          linestyle = LINESTYLES[i])
end

# --- Phase -------------------------------------------------------------------
p_phase = plot(xaxis = :log,
               legend = :bottomleft,
               xlabel = "Frequency (Hz)",
               ylabel = "∠Z_h  (deg)",
               title  = "Harmonic Impedance — Phase")
for i = 1:ng
    plot!(p_phase, freq, 180/π .* angle.(zh[:, i]);
          label     = "GS $(gs_arr[i]) m",
          linecolor = OKABE_ITO[i],
          linestyle = LINESTYLES[i])
end

# --- Combined figure for on-screen viewing ----------------------------------
# Stacked vertically. Change to layout = (1, 2) for side-by-side.
p_combined = plot(p_mag, p_phase;
                  layout = (2, 1),
                  size   = (800, 850),
                  plot_title = "")

# --- Save (always works, regardless of how Julia is launched) ---------------
savefig(p_mag,      "grcev12pwrd01_magnitude.pdf")
savefig(p_phase,    "grcev12pwrd01_phase.pdf")
savefig(p_combined, "grcev12pwrd01_combined.pdf")
# Also save PNGs if you want quick previews without a PDF viewer:
# savefig(p_combined, "grcev12pwrd01_combined.png")
println("Saved: grcev12pwrd01_{magnitude,phase,combined}.pdf")

# --- Display ----------------------------------------------------------------
# Works in the VSCode plot pane and in any interactive Julia session
# (REPL, `julia -i file.jl`, Jupyter, Pluto). In a non-interactive
# `julia file.jl` run the PDFs above are the deliverable.
display(p_combined)

# When run as `julia file.jl` (non-interactive), the window closes the
# instant the script exits. To inspect plots on screen after a terminal
# run, launch Julia with `julia -i grcev12pwrd01a.jl` instead, which
# drops you into the REPL with all plots still alive.
