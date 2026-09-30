# Error type (`mError`). Every failure on user input is a `TupaError`;
# messages follow the Fortran `raiseError` texts, like the Rust port.

"A critical error with a human-readable message."
struct TupaError <: Exception
    msg::String
end

Base.showerror(io::IO, e::TupaError) = print(io, e.msg)

"Throw a `TupaError` (`raiseError`)."
raise_error(msg::AbstractString) = throw(TupaError(String(msg)))
