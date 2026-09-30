# Geometric elements that discretise into nodes and electrodes (`mElement`):
# straight `Line`, composite rectangular `MeshElement` (ADR 0020) and sagging
# `Catenary` (ADR 0023).
#
# Every element implements `assemble!(element, structure)` (resolve
# references, append nodes and electrodes) and `report(element)`.

"A geometric element of the structure."
abstract type AbstractElement end
