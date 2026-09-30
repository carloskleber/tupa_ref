using LinearAlgebra
using Plots
using Printf

include(joinpath(@__DIR__, "..", "hem.jl"))

"Precompute the frequency-independent modified-HEM geometry terms."
function straight_geometry(start_point, end_point, radius, number_of_segments)
    conductor = new_electrode(Float64.(start_point), Float64.(end_point), Float64(radius))
    electrodes, nodes = segment_electrode(conductor, number_of_segments)
    images = [new_electrode([e.start_point[1], e.start_point[2], -e.start_point[3]],
                            [e.end_point[1], e.end_point[2], -e.end_point[3]],
                            e.radius) for e in electrodes]
    ns = length(electrodes)

    direct_l, direct_t = calculate_impedances(
        electrodes, 0.0, 1.0, 1.0, 1.0,
        typemax(Int), 1e-4, 1e-5, norm, INTG_MHEM,
    )
    image_l, image_t = impedances_images(
        electrodes, images, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0,
        typemax(Int), 1e-4, 1e-5, norm, INTG_MHEM,
    )

    direct_distance = zeros(ns, ns)
    image_distance = zeros(ns, ns)
    for j in 1:ns, i in j:ns
        direct_distance[i, j] = norm(electrodes[j].middle_point - electrodes[i].middle_point)
        image_distance[i, j] = norm(electrodes[j].middle_point - images[i].middle_point)
    end
    a, b = incidence(electrodes, nodes)
    injection_node = matchrow(Float64.(start_point), nodes)
    (; electrodes, nodes, direct_l, direct_t, image_l, image_t,
       direct_distance, image_distance, a, b, injection_node)
end

"Calculate endpoint harmonic impedance using the archive's native mHEM formulation."
function harmonic_impedance(geometry, frequencies; rho, epsr=10.0, mur=1.0)
    ns = length(geometry.electrodes)
    nn = size(geometry.nodes, 1)
    impedance = Vector{ComplexF64}(undef, length(frequencies))
    zl = zeros(ComplexF64, ns, ns)
    zt = zeros(ComplexF64, ns, ns)

    for (frequency_index, frequency) in pairs(frequencies)
        omega = 2pi * frequency
        jw = im * omega
        kappa = inv(rho) + jw * epsr * EPS0
        kappa_air = jw * EPS0
        propagation = sqrt(jw * mur * MU0 * kappa)
        transverse_reflection = (kappa - kappa_air) / (kappa + kappa_air)
        magnetic_factor = jw * mur * MU0 / FOUR_PI
        electric_factor = inv(FOUR_PI * kappa)
        fill!(zl, 0)
        fill!(zt, 0)

        for j in 1:ns, i in j:ns
            direct_decay = exp(-propagation * geometry.direct_distance[i, j])
            image_decay = exp(-propagation * geometry.image_distance[i, j])
            zl[i, j] = magnetic_factor * (
                direct_decay * geometry.direct_l[i, j] +
                image_decay * geometry.image_l[i, j]
            )
            zt[i, j] = electric_factor * (
                direct_decay * geometry.direct_t[i, j] +
                transverse_reflection * image_decay * geometry.image_t[i, j]
            )
        end

        yn = admittance(zl, zt, geometry.a, geometry.b)
        current = zeros(ComplexF64, nn)
        current[geometry.injection_node] = 1
        voltage = yn \ current
        impedance[frequency_index] = voltage[geometry.injection_node]
    end
    impedance
end

function write_comparison_csv(path, cases)
    open(path, "w") do io
        println(io, "solver,length_m,rho_ohm_m,frequency_hz,z_real_ohm,z_imag_ohm,z_magnitude_ohm,z_phase_deg")
        for case in cases
            for (frequency, z) in zip(case.frequencies, case.impedance)
                @printf(io, "mHEM,%.6g,%.6g,%.16e,%.16e,%.16e,%.16e,%.16e\n",
                        case.length_m, case.rho, frequency, real(z), imag(z),
                        abs(z), rad2deg(angle(z)))
            end
        end
    end
    println("wrote $path")
end

function plot_comparison_cases(path, cases; title)
    magnitude = plot(xscale=:log10, yscale=:log10, grid=true,
                     xlabel="Frequency (Hz)", ylabel="|Zₕ| (Ω)", legend=:best)
    phase = plot(xscale=:log10, grid=true,
                 xlabel="Frequency (Hz)", ylabel="Phase (degrees)", legend=:best)
    for case in cases
        label = case.label
        plot!(magnitude, case.frequencies, abs.(case.impedance), label=label, linewidth=2)
        plot!(phase, case.frequencies, rad2deg.(angle.(case.impedance)), label=label, linewidth=2)
    end
    figure = plot(magnitude, phase; layout=(2, 1), size=(950, 800), plot_title=title)
    savefig(figure, path)
    println("wrote $path")
end
