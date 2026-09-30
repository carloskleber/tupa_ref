# Composite rectangular grounding-grid element (`mElementMesh`, ADR 0020).
#
# `rows_x` bars parallel to X (each `length_x` long, spaced along Y) and
# `rows_y` bars parallel to Y, wired between a regular grid of main nodes
# named `<mesh id>-<row:02><col:02>` (0-based). Each bar is a `Line`
# `<mesh id>-<rrcc>-<rrcc>` with `segments` electrodes.

"Maximum rows per direction (two-digit node-ID mnemonic)."
const MAX_MESH_ROWS = 100

"A rectangular axis-aligned grounding grid."
struct MeshElement <: AbstractElement
    id::String
    "Corner position (z ≠ 0)"
    position::Vec3
    "Extent along X (m)"
    length_x::Float64
    "Extent along Y (m)"
    length_y::Float64
    "Bars parallel to X"
    rows_x::Int
    "Bars parallel to Y"
    rows_y::Int
    "Conductor radius (m)"
    radius::Float64
    "Segments per bar"
    segments::Int
    "Conductor material id"
    id_material::String
end

row_col_tag(row::Integer, col::Integer) = @sprintf("%02d%02d", row, col)
mesh_node_id(m::MeshElement, row, col) = "$(m.id)-$(row_col_tag(row, col))"
mesh_error(m::MeshElement, msg) = raise_error("tMeshElement '$(m.id)': $msg")

"Number of bars."
n_bars(m::MeshElement) = m.rows_x * max(m.rows_y - 1, 0) + m.rows_y * max(m.rows_x - 1, 0)

"""
Plant the main nodes, then wire every adjacent pair with a bar
(X-parallel bars first, then Y-parallel).
"""
function assemble!(m::MeshElement, s::Structure)
    (m.rows_x < 2 || m.rows_y < 2) && mesh_error(m, "rowsX and rowsY must each be >= 2")
    (m.rows_x > MAX_MESH_ROWS || m.rows_y > MAX_MESH_ROWS) &&
        mesh_error(m, "rowsX/rowsY must each be <= 100 (2-digit node ID mnemonic)")
    m.segments < 1 && mesh_error(m, "segments must be >= 1")
    (m.length_x <= 0.0 || m.length_y <= 0.0) && mesh_error(m, "lengthX and lengthY must be > 0")
    m.position[3] == 0.0 && mesh_error(m,
        "position z = 0 (exactly on the air-soil interface) is not supported — a segment " *
        "straddling the interface is not well-defined by the image-method formulation " *
        "(theory.md §2, §5)")
    find_material(s, m.id_material) === nothing &&
        mesh_error(m, "material '$(m.id_material)' not found")

    for row in 0:m.rows_x-1, col in 0:m.rows_y-1
        p = (m.position[1] + col * m.length_x / (m.rows_y - 1),
             m.position[2] + row * m.length_y / (m.rows_x - 1),
             m.position[3] + 0.0)
        add_node!(s, Node(mesh_node_id(m, row, col), p))
    end
    for row in 0:m.rows_x-1, col in 0:m.rows_y-2
        assemble_bar!(m, s, row, col, row, col + 1)
    end
    for col in 0:m.rows_y-1, row in 0:m.rows_x-2
        assemble_bar!(m, s, row, col, row + 1, col)
    end
    return s
end

function assemble_bar!(m::MeshElement, s::Structure, row1, col1, row2, col2)
    bar_id = "$(m.id)-$(row_col_tag(row1, col1))-$(row_col_tag(row2, col2))"
    bar = Line(bar_id, mesh_node_id(m, row1, col1), mesh_node_id(m, row2, col2),
               m.radius, m.segments, m.id_material)
    assemble!(bar, s)
    return nothing
end

function report(m::MeshElement)
    n_nodes = m.rows_x * m.rows_y + n_bars(m) * max(m.segments - 1, 0)
    return @sprintf("Element ID: %s, Material: %s, %dx%d rows, %.3fm x %.3fm, radius %.4fm\n",
                    m.id, m.id_material, m.rows_x, m.rows_y, m.length_x, m.length_y, m.radius) *
           @sprintf("  Nodes: %d (%dx%d main), Electrodes: %d\n",
                    n_nodes, m.rows_x, m.rows_y, n_bars(m) * m.segments)
end
