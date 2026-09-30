# -*- coding: utf-8 -*-
"""
Script to create a set of lines and break them at their intersection points.

Author: Pedro Henrique Nascimento Vieira
"""

using Plots
using LinearAlgebra

struct Point
	x::Real
	y::Real
end

struct Line
	start_point::Point
	end_point::Point
end

function crosses_point(line::Line, point::Point)
    """ Returns True if the point is upon the line. """
    if line.start_point.x <= line.end_point.x
        sx = line.start_point.x
        ex = line.end_point.x
    else
        ex = line.start_point.x
        sx = line.end_point.x
    end
    if line.start_point.y <= line.end_point.y
        sy = line.start_point.y
        ey = line.end_point.y
    else
        ey = line.start_point.y
        sy = line.end_point.y
    end
    xbound = (sx <= point.x <= ex)
    ybound = (sy <= point.y <= ey)
    return (xbound && ybound)
end

function intersection(line1::Line, line2::Line)
    """
    Returns the point at which the lines intersect, provided that they cross it
    Returns nothing otherwise or if there are infinite intersection points.
    """
    a1 = line1.end_point.y - line1.start_point.y
    b1 = line1.start_point.x - line1.end_point.x
    c1 = a1*line1.start_point.x + b1*line1.start_point.y
    a2 = line2.end_point.y - line2.start_point.y
    b2 = line2.start_point.x - line2.end_point.x
    c2 = a2*line2.start_point.x + b2*line2.start_point.y
    delta = a1*b2 - a2*b1
    x = (b2*c1 - b1*c2)
    y = (a1*c2 - a2*c1)
    point = Point(x/delta, y/delta)
    if crosses_point(line1, point) && crosses_point(line2, point)
        return point
    else
        return nothing
    end
end

function break_intersections(lines)
    """
    Breaks the lines in the set in every intersection point and return
    the new set.
    """
    function loop_set(lines)
        for li in lines  # TODO better search than linear?
            for lk in setdiff(lines, [li])
                intersect_point = intersection(li, lk)
                if intersect_point != nothing
                    old_length = new_length = length(lines)
                    for lm in [li, lk]
                        xp = intersect_point.x
						yp = intersect_point.y
                        x0 = lm.start_point.x
						y0 = lm.start_point.y
						x1 = lm.end_point.x
						y1 = lm.end_point.y
                        c1 = isapprox([xp, yp], [x0, y0])
                        c2 = isapprox([xp, yp], [x1, y1])
                        if !(c1 || c2)
                            x = lm.start_point.x
							y = lm.start_point.y
                            l1 = Line(Point(x, y), intersect_point)
							x = lm.end_point.x
							y = lm.end_point.y
                            l2 = Line(intersect_point, Point(x, y))
                            setdiff!(lines, [lm])
                            union!(lines, [l1, l2])
                            new_length = length(lines)
                        end
                    end
                    if new_length > old_length
                        return lines
                    end
                end
            end
        end
        return lines
    end
    old_length = new_length = length(lines)
    first_run = true
    while new_length > old_length || first_run
        first_run = false
        old_length = new_length
        lines = loop_set(lines)
        new_length = length(lines)
    end
    return lines
end

function plot_lines(lines, nodes=false)
    fig = plot(leg=false, aspect_ratio=1)#, border=:none)
    for l in lines
        x0 = l.start_point.x
		y0 = l.start_point.y
        x1 = l.end_point.x
		y1 = l.end_point.y
        plot!([x0, x1], [y0, y1], line=(1, :black, :solid))
        nodes && scatter!([x0, x1], [y0, y1], markercolor=:black, markersize=2)
    end
    return fig
end

function create_grid()
    lines = [
         Line(Point(0, 0), Point(250, 0)),
         Line(Point(0, 5), Point(250, 5)),
         Line(Point(0, 10), Point(250, 10)),
         Line(Point(0, 20), Point(250, 20)),
         Line(Point(0, 30), Point(400, 30)),
         Line(Point(240, 35), Point(400, 35)),
         Line(Point(240, 40), Point(400, 40)),
         Line(Point(0, 60), Point(400, 60)),
         Line(Point(0, 100), Point(400, 100)),
         Line(Point(0, 120), Point(400, 120)),
         Line(Point(0, 150), Point(400, 150)),
         Line(Point(0, 165), Point(400, 165)),
         Line(Point(0, 170), Point(130, 170)),
         Line(Point(300, 170), Point(400, 170)),
         Line(Point(0, 175), Point(130, 175)),
         Line(Point(300, 175), Point(400, 175)),
         Line(Point(120, 180), Point(310, 180)),
         Line(Point(120, 190), Point(310, 190)),
         Line(Point(120, 195), Point(310, 195)),
         Line(Point(120, 200), Point(310, 200)),
         Line(Point(0, 0), Point(0, 175)),
         Line(Point(5, 0), Point(5, 175)),
         Line(Point(10, 0), Point(10, 175)),
         Line(Point(20, 0), Point(20, 175)),
         Line(Point(30, 0), Point(30, 175)),
         Line(Point(40, 0), Point(40, 175)),
         Line(Point(80, 0), Point(80, 175)),
         Line(Point(120, 0), Point(120, 200)),
         Line(Point(160, 0), Point(160, 200)),
         Line(Point(200, 0), Point(200, 200)),
         Line(Point(225, 0), Point(225, 200)),
         Line(Point(250, 0), Point(250, 200)),
         Line(Point(280, 30), Point(280, 200)),
         Line(Point(310, 30), Point(310, 200)),
         Line(Point(350, 30), Point(350, 175)),
         Line(Point(370, 30), Point(370, 175)),
         Line(Point(380, 30), Point(380, 175)),
         Line(Point(390, 30), Point(390, 175)),
         Line(Point(395, 30), Point(395, 175)),
         Line(Point(400, 30), Point(400, 175)),
         Line(Point(240, 0), Point(240, 40)),
         Line(Point(245, 0), Point(245, 40)),
         Line(Point(125, 165), Point(125, 200)),
         Line(Point(130, 165), Point(130, 200)),
         Line(Point(300, 165), Point(300, 200)),
         Line(Point(305, 165), Point(305, 200)),
    ]
	return break_intersections(lines)
end

function segment_lines(lines, lmax)
    seg_lines = [Line(Point(0,0), Point(0,0))]
    for li in lines
        x0 = li.start_point.x
        y0 = li.start_point.y
        x1 = li.end_point.x
        y1 = li.end_point.y
        v = [x1 - x0, y1 - y0]
        L = norm(v)
        n = Int(cld(L, lmax))
        dl = v./n
        for k = 0:(n-1)
            p0 = [x0, y0] + k.*dl;
            p1 = p0 .+ dl;
            push!(seg_lines, Line(Point(p0[1], p0[2]), Point(p1[1], p1[2])))
        end
    end
    return seg_lines[2:end]
end

function main(nodes, lmax)
    lines = create_grid()
	seg_lines = segment_lines(lines, lmax)
	if nodes
    	fig = plot_lines(seg_lines, nodes)
	else
    	fig = plot_lines(lines, nodes)
	end
	savefig(fig, "grid.png")
    r = 10e-3  # radius
    z = 0.0
    println("number of lines = ", length(seg_lines))
	io = open("grid.txt", "w")
	for li in seg_lines
        x0 = li.start_point.x
		y0 = li.start_point.y
		x1 = li.end_point.x
		y1 = li.end_point.y
		write(io, join([string(x0), ", ", string(y0), ", ", string(z), ", ",
                        string(x1), ", ", string(y1), ", ", string(z), ", ",
                        string(r), "\n"]))
	end
	close(io)
    return lines, fig
end

# if true: 596.712559 seconds; else: 24.493915 seconds
@time lines, fig = main(false, 0.2);
