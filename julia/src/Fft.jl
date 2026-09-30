# In-repo radix-2 FFT (`mFft`, ADR 0014): iterative decimation with a
# bit-reversal permutation, ported so the operation order matches the Fortran
# code (general-purpose FFT libraries are not used on the reference path).
# Forward kernel e^{-jθ}, inverse e^{+jθ} scaled by 1/N.

"`n > 0` and a power of two."
is_power_of_two(n::Integer) = n > 0 && (n & (n - 1)) == 0

"Smallest power of two ≥ `n`."
next_power_of_two(n::Integer) = n <= 1 ? 1 : nextpow(2, n)

"In-place forward transform."
fft_forward!(x::AbstractVector{ComplexF64}) = fft_core!(x, -1.0)

"In-place inverse transform (scaled by 1/N)."
function fft_inverse!(x::AbstractVector{ComplexF64})
    fft_core!(x, 1.0)
    x ./= length(x)
    return x
end

function fft_core!(x::AbstractVector{ComplexF64}, sgn::Float64)
    n = length(x)
    n <= 1 && return x
    is_power_of_two(n) || raise_error("mFft: array length must be a power of two")

    # bit-reversal permutation
    j = 1
    @inbounds for i in 1:n
        if j > i
            x[j], x[i] = x[i], x[j]
        end
        m = n ÷ 2
        while m >= 2 && j > m
            j -= m
            m ÷= 2
        end
        j += m
    end

    mmax = 1
    @inbounds while mmax < n
        istep = 2 * mmax
        for m in 1:mmax
            theta = sgn * π * (m - 1) / mmax
            w = complex(cos(theta), sin(theta))
            for i in m:istep:n
                jj = i + mmax
                t = w * x[jj]
                x[jj] = x[i] - t
                x[i] += t
            end
        end
        mmax = istep
    end
    return x
end
