module mBranch
  !! Lumped series R-L-C branch between two nodes (theory.md §6, ROADMAP
  !! Phase 12 item 3 as pulled forward by ADR 0028).
  !!
  !! A branch is a circuit element with no geometry and no electromagnetic
  !! coupling to any segment: it adds the current `Y(s)·(u_a − u_b)` to the
  !! Kirchhoff balance of its nodes, nothing else. `nodeB = 0` connects node
  !! `a` to remote earth (the ground plane of ideal images), the usual
  !! matched termination of an overhead line.
  implicit none
  private

  type, public :: tBranch
    !! One lumped series branch, `Z(s) = R + sL + 1/(sC)`.
    character(len=256) :: id = ""
    !! User-assigned identifier
    integer(4) :: nodeA = 0
    !! 1-based index of the first node
    integer(4) :: nodeB = 0
    !! 1-based index of the second node; 0 = remote earth
    real(8) :: resistance = 0.0d0
    !! Series resistance R (Ω)
    real(8) :: inductance = 0.0d0
    !! Series inductance L (H)
    real(8) :: capacitance = 0.0d0
    !! Series capacitance C (F); 0 = no capacitor in the branch
  contains
    procedure :: admittance => branchAdmittance
  end type tBranch

contains

  complex(8) function branchAdmittance(this, s) result(y)
    !! Branch admittance `Y = 1/Z` at the complex frequency `s` (`jω` for a
    !! harmonic solve, `c + jω` for the Numerical Laplace Transform).
    !!
    !! With a capacitor the closed form `sC/(1 + sCR + s²LC)` is used, so a
    !! series C is finite (zero) at `s → 0` instead of a `1/0`. The
    !! constructor of the element guarantees `Z ≠ 0` for every `s` the
    !! solver can ask for (an inductor-only branch has `Z = 0` only at
    !! `s = 0`, which no sweep or transform bin uses).
    class(tBranch), intent(in) :: this
    complex(8), intent(in) :: s

    if (this%capacitance > 0.0d0) then
      y = s * this%capacitance / &
          (cmplx(1.0d0, 0.0d0, kind=8) + s * this%capacitance * this%resistance &
           + s * s * this%inductance * this%capacitance)
    else
      y = cmplx(1.0d0, 0.0d0, kind=8) / (cmplx(this%resistance, 0.0d0, kind=8) + s * this%inductance)
    end if
  end function branchAdmittance

end module mBranch
