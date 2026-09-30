#= Functions to generate different transmission line counterpoise configurations. =#
using Plots
include("hem1.jl")


"""Plot the electrodes in the xy-plane."""
function plot_electrodes(electrodes, nodes=false)
    fig = plot(leg=false, aspect_ratio=1)#, border=:none)
    for e in electrodes
        x0 = e.start_point[1]
		y0 = e.start_point[2]
        x1 = e.end_point[1]
		y1 = e.end_point[2]
        plot!([x0, x1], [y0, y1], line=(1, :black, :solid))
        nodes && scatter!([x0, x1], [y0, y1], markercolor=:black, markersize=3)
    end
    return fig
end


## Counterpoise configurations
"""
Build an electrode list of a conventional counterpoise.

        L      s2   s1   s2     L
    |<------>|<-->|<-->|<-->|<------>|
    ----------              ----------
              \\            /           /\\
               \\          /            s4
                \\        /             \\/  /\\
                                           s3
                /        \\             /\\  \\/
               /          \\            s4
              /            \\           \\/
    ----------              ----------

Parameters
----------
    L : horizontal arm length
    s1 : inner Δx separation
    s2 : inclined arm Δx
    s3 : inner Δy separation
    s4 : inclined arm Δy
    h : burial depth
    r : wire radius

Returns
-------
    electrode list
"""
function conventional1(L, s1, s2, s3, s4, h, r)
    x11 = s1 / 2.0
    x12 = s3 / 2.0
    x21 = x11 + s2
    x22 = x12 + s4
    x31 = x21 + L
    wires = [
        new_electrode([x11, x12, h], [x21, x22, h], r),
        new_electrode([x21, x22, h], [x31, x22, h], r),
        new_electrode([-x11, x12, h], [-x21, x22, h], r),
        new_electrode([-x21, x22, h], [-x31, x22, h], r),
        new_electrode([x11, -x12, h], [x21, -x22, h], r),
        new_electrode([x21, -x22, h], [x31, -x22, h], r),
        new_electrode([-x11, -x12, h], [-x21, -x22, h], r),
        new_electrode([-x21, -x22, h], [-x31, -x22, h], r),
    ]
    return wires
end


"""
Build an electrode list of a conventional counterpoise. Version 2.

        L      s2   s1   s2     L
    |<------>|<-->|<-->|<-->|<------>|
    ----------              ----------   \\/
               \\          /              s4
                          \\              /\\    /\\
    ----------              ----------         s3
               \\                         \\/    \\/
               /           \\             s4
    ----------              ----------   /\\

Parameters
----------
    L : horizontal arm length
    s1 : inner Δx separation
    s2 : inclined arm Δx
    s3 : inner Δy separation
    s4 : inclined arm Δy
    h : burial depth
    r : wire radius

Returns
-------
    electrode list
"""
function conventional2(L, s1, s2, s3, s4, h, r)
    x11 = s1 / 2.0
    x12 = s3 / 2.0
    x21 = x11 + s2
    x31 = x21 + L
    wires1 = conventional1(L, s1, s2, s3, s4, h, r)
    wires2 = [
        new_electrode([x11, x12, h], [x21, 0, h], r),
        new_electrode([x21, 0, h], [x31, 0, h], r),
        new_electrode([-x11, -x11, h], [-x21, 0, h], r),
        new_electrode([-x21, 0, h], [-x31, 0, h], r),
    ]
    return [wires1; wires2]
end


"""
Build an electrode list of a narrow counterpoise.

        L      s2   s1   s2     L
    |<------>|<-->|<-->|<-->|<------>| 

              |\\         /|             \\/
              | \\       / |             s4
    -----------           -----------   /\\   \\/
                                        s3   dw
    -----------           -----------   \\/   /\\
              | /       \\ |             s4
              |/         \\|             /\\

Parameters
----------
    L : horizontal arm length
    s1 : inner Δx separation
    s2 : inclined arm Δx
    s3 : inner Δy separation
    s4 : inclined arm Δy
    h : burial depth
    r : wire radius
    dw : separation width between the horizontal arms

Returns
-------
    electrode list
"""
function narrow(L, s1, s2, s3, s4, h, r, dw)
    x11 = s1 / 2.0
    x12 = s3 / 2.0
    x21 = x11 + s2
    x22 = x12 + s4
    x31 = x21 + L
    x32 = dw / 2.0
    wires = [
        new_electrode([x11, x12, h], [x21, x22, h], r),
        new_electrode([x21, x22, h], [x21, x32, h], r),
        new_electrode([x21, x32, h], [x31, x32, h], r),
        new_electrode([-x11, x12, h], [-x21, x22, h], r),
        new_electrode([-x21, x22, h], [-x21, x32, h], r),
        new_electrode([-x21, x32, h], [-x31, x32, h], r),
        new_electrode([-x11, -x12, h], [-x21, -x22, h], r),
        new_electrode([-x21, -x22, h], [-x21, -x32, h], r),
        new_electrode([-x21, -x32, h], [-x31, -x32, h], r),
        new_electrode([x11, -x12, h], [x21, -x22, h], r),
        new_electrode([x21, -x22, h], [x21, -x32, h], r),
        new_electrode([x21, -x32, h], [x31, -x32, h], r),
    ]
    return wires
end


"""
Build an electrode list of an unconventional counterpoise.

        L      s2   s1   s2     L
    |<------>|<-->|<-->|<-->|<------>|
             ----------------
             |\\            /|           /\\
             | \\          / |           s4
             |  \\        /  |           \\/   /\\
    ---------|              |---------       s3
             |  /        \\  |           /\\   \\/
             | /          \\ |           s4
             |/            \\|           \\/
             ----------------

Parameters
----------
    L : horizontal arm length
    s1 : inner Δx separation
    s2 : inclined arm Δx
    s3 : inner Δy separation
    s4 : inclined arm Δy
    h : burial depth
    r : wire radius

Returns
-------
    electrode list
"""
function unconventional(L, s1, s2, s3, s4, h, r)
    x11 = s1 / 2.0
    x12 = s3 / 2.0
    x21 = x11 + s2
    x22 = x12 + s4
    x31 = x21 + L
    wires = [
        new_electrode([x11, x12, h], [x21, x22, h], r),
        new_electrode([x21, x22, h], [x21, 0, h], r),
        new_electrode([x21, x22, h], [-x21, x22, h], r),
        new_electrode([-x11, x12, h], [-x21, x22, h], r),
        new_electrode([-x21, x22, h], [-x21, 0, h], r),
        new_electrode([x11, -x12, h], [x21, -x22, h], r),
        new_electrode([x21, -x22, h], [x21, 0, h], r),
        new_electrode([x21, -x22, h], [-x21, -x22, h], r),
        new_electrode([-x11, -x12, h], [-x21, -x22, h], r),
        new_electrode([-x21, -x22, h], [-x21, 0, h], r),
        new_electrode([x21, 0, h], [x31, 0, h], r),
        new_electrode([-x21, 0, h], [-x31, 0, h], r),
    ]
    return wires
end

