# CLI verbosity levels (`mVerbosity`).

"Suppress all informational output."
const VERB_QUIET = 0
"Default level."
const VERB_NORMAL = 1
"Progress output per assembly step and frequency."
const VERB_VERBOSE = 2

const VERBOSITY = Ref(VERB_NORMAL)

"Set the global verbosity level."
set_verbosity(level::Integer) = (VERBOSITY[] = level; nothing)

"Current global verbosity level."
verbosity_level() = VERBOSITY[]

"Print `msg` if the current level is at least `level`."
verbose(level::Integer, msg::AbstractString) =
    (verbosity_level() >= level && println(" ", msg); nothing)
