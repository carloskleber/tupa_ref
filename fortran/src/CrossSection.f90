module mCrossSection
  !! Cross-section internal-impedance entries (ADR 0030, ROADMAP Phase 13
  !! item 2): a declared per-metre `Z_int(s)` for a conductor that is not a
  !! solid round wire (strip, angle, tube, stranded cable), used in place of
  !! the theory.md §4.3 cylinder.
  !!
  !! `tCrossSection` extends `tMaterial`, so an entry lives in the structure's
  !! material list under its id and elements reach it through the existing
  !! material lookup. `Z_int` is defined relative to the entry's equivalent
  !! (logarithmic-capacity) radius `r_eq`, which the thin-wire kernel uses as
  !! the segment radius (ADR 0030 §1); the loader sets the electrode radius.
  !!
  !! Two kinds:
  !! - `poleResidue`: `Z(s) = d + e·s + Σ c_k/(s − a_k)`, real poles `a_k < 0`;
  !!   exact at any complex `s` (harmonic and NLT paths);
  !! - `table`: `R(f)`, `X(f)` (Ω/m) on a frequency grid, log-linear
  !!   interpolation; `jω` axis only.
  !!
  !! Conductivity scaling (ADR 0030 §3): an entry stored at `sigmaRef` serves a
  !! conductor of conductivity σ as `Z(s; σ) = (σ0/σ) Z_ref(s σ/σ0)`.
  use mCtes, only: PI, EPSILON0, newl
  use mMaterial, only: tMaterial
  use mError, only: raiseError, errorRaised
  use mVerbosity, only: emitMessage, MSG_WARNING
  use mJsonParser
  implicit none
  private

  public :: newCrossSectionFromJson, withConductivity, loadCrossSectionFile
  public :: CS_POLE_RESIDUE, CS_TABLE

  integer, parameter :: CS_POLE_RESIDUE = 1
  integer, parameter :: CS_TABLE = 2

  type, extends(tMaterial), public :: tCrossSection
    !! One database entry, possibly rescaled to a segment's conductivity.
    integer :: kind = CS_POLE_RESIDUE
    real(8) :: equivalentRadius = 0.0d0
    !! r_eq (m): radius the thin-wire kernel uses for this section
    real(8) :: sigmaRef = 0.0d0
    !! Conductivity the entry was computed at (S/m); `mur` is the reference μr
    real(8) :: sigma = 0.0d0
    !! Conductivity of the conductor this instance represents (S/m)
    real(8) :: fMin = 0.0d0, fMax = 0.0d0
    !! Fitted / tabulated band at the reference conductivity (Hz)
    real(8) :: d = 0.0d0, e = 0.0d0
    !! poleResidue: constant term (Ω/m) and `s` coefficient (H/m)
    real(8), allocatable :: poles(:), residues(:)
    !! poleResidue: real poles `a_k < 0` (rad/s) and residues `c_k` (Ω/m·rad/s)
    real(8), allocatable :: tabF(:), tabR(:), tabX(:)
    !! table: frequency (Hz), resistance and reactance (Ω/m)
  contains
    procedure :: admittance => admittance_cs
    procedure :: admittanceLaplace => admittanceLaplace_cs
    procedure :: report => report_cs
    procedure :: perMetre
    procedure :: perMetreLaplace
    procedure :: effectiveBand
  end type tCrossSection

contains

  ! ------------------------------------------------------------------
  ! tMaterial interface (a conductor entry is never used as a medium)
  ! ------------------------------------------------------------------

  function admittance_cs(this, omega) result(w)
    class(tCrossSection), intent(in) :: this
    real(8), intent(in) :: omega
    complex(8) :: w
    w = cmplx(this%sigma, omega * EPSILON0 * 0.0d0, kind=8)
  end function admittance_cs

  function admittanceLaplace_cs(this, s) result(w)
    class(tCrossSection), intent(in) :: this
    complex(8), intent(in) :: s
    complex(8) :: w
    w = cmplx(this%sigma, 0.0d0, kind=8) + 0.0d0 * s
  end function admittanceLaplace_cs

  subroutine report_cs(this, str)
    class(tCrossSection), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    character(len=256) :: line

    write(line, '(A,A,A,ES10.3,A)') "cross-section '", trim(this%id), "' r_eq = ", this%equivalentRadius, " m"
    str = str // trim(line) // newl
  end subroutine report_cs

  ! ------------------------------------------------------------------
  ! Evaluation
  ! ------------------------------------------------------------------

  function perMetre(this, omega) result(z)
    !! Z_int per metre (Ω/m) at the real angular frequency `omega`.
    class(tCrossSection), intent(in) :: this
    real(8), intent(in) :: omega
    complex(8) :: z
    real(8) :: k

    k = this%sigma / this%sigmaRef
    select case (this%kind)
    case (CS_TABLE)
      z = tableAt(this, omega * k / (2.0d0 * PI)) / k
    case default
      z = poleResidueAt(this, cmplx(0.0d0, omega * k, kind=8)) / k
    end select
  end function perMetre

  function perMetreLaplace(this, s) result(z)
    !! Z_int per metre at the complex frequency `s = c + jω`. A `table` entry
    !! has no continuation off the `jω` axis: the study refuses it before the
    !! sweep, so reaching it here returns NaN rather than a wrong number.
    class(tCrossSection), intent(in) :: this
    complex(8), intent(in) :: s
    complex(8) :: z
    real(8) :: k, nan

    k = this%sigma / this%sigmaRef
    if (this%kind == CS_TABLE) then
      nan = 0.0d0
      nan = nan / nan
      z = cmplx(nan, nan, kind=8)
    else
      z = poleResidueAt(this, s * k) / k
    end if
  end function perMetreLaplace

  function poleResidueAt(this, s) result(z)
    class(tCrossSection), intent(in) :: this
    complex(8), intent(in) :: s
    complex(8) :: z
    integer :: n

    z = this%d + this%e * s
    do n = 1, size(this%poles)
      z = z + this%residues(n) / (s - this%poles(n))
    end do
  end function poleResidueAt

  function tableAt(this, f) result(z)
    !! Interpolated Z_ref(f) of a table entry. Below the first point the
    !! resistance is held and the inductance kept; above the last point the
    !! impedance continues as √f (skin-effect law).
    class(tCrossSection), intent(in) :: this
    real(8), intent(in) :: f
    complex(8) :: z
    integer :: n, lo, hi, mid
    real(8) :: t

    n = size(this%tabF)
    if (f <= 0.0d0) then
      z = cmplx(this%tabR(1), 0.0d0, kind=8)
    else if (f < this%tabF(1)) then
      z = cmplx(this%tabR(1), this%tabX(1) * f / this%tabF(1), kind=8)
    else if (f > this%tabF(n)) then
      z = cmplx(this%tabR(n), this%tabX(n), kind=8) * sqrt(f / this%tabF(n))
    else
      lo = 1
      hi = n
      do while (hi - lo > 1)
        mid = (lo + hi) / 2
        if (this%tabF(mid) <= f) then
          lo = mid
        else
          hi = mid
        end if
      end do
      t = log(f / this%tabF(lo)) / log(this%tabF(hi) / this%tabF(lo))
      z = cmplx(this%tabR(lo) + t * (this%tabR(hi) - this%tabR(lo)), &
                this%tabX(lo) + t * (this%tabX(hi) - this%tabX(lo)), kind=8)
    end if
  end function tableAt

  subroutine effectiveBand(this, fLow, fHigh)
    !! The band of *applied* frequencies this entry covers for its own σ:
    !! the reference band divided by σ/σ_ref.
    class(tCrossSection), intent(in) :: this
    real(8), intent(out) :: fLow, fHigh
    real(8) :: k

    k = this%sigma / this%sigmaRef
    fLow = this%fMin / k
    fHigh = this%fMax / k
  end subroutine effectiveBand

  ! ------------------------------------------------------------------
  ! Construction
  ! ------------------------------------------------------------------

  function withConductivity(cs, sigma, id) result(out)
    !! A copy of `cs` representing a conductor of conductivity `sigma`.
    type(tCrossSection), intent(in) :: cs
    real(8), intent(in) :: sigma
    character(len=*), intent(in) :: id
    type(tCrossSection) :: out

    out = cs
    out%id = id
    out%sigma = sigma
  end function withConductivity

  subroutine newCrossSectionFromJson(obj, cs)
    !! Build an entry from its JSON object (ADR 0030 §2). Errors are raised
    !! through `mError` (library mode: recorded, `cs` then unusable).
    type(tJsonValue), intent(in) :: obj
    type(tCrossSection), intent(out) :: cs
    type(tJsonValue), pointer :: ref, band, model
    character(len=256) :: kindStr, id
    logical :: ok
    integer :: n

    id = json_str(obj, "id")
    if (len_trim(id) == 0) then
      call raiseError("mCrossSection: an entry has no id")
      return
    end if
    cs%id = id
    if (json_has(obj, "schema")) then
      if (index(json_str(obj, "schema"), "tupa.crossSection/") /= 1) then
        call raiseError("mCrossSection: entry '" // trim(id) // "' has an unknown schema '" // &
                        trim(json_str(obj, "schema")) // "'")
        return
      end if
    end if

    cs%equivalentRadius = json_real(obj, "equivalentRadius")
    if (.not. (cs%equivalentRadius > 0.0d0)) then
      call raiseError("mCrossSection: entry '" // trim(id) // "' needs equivalentRadius > 0")
      return
    end if

    ref => json_child(obj, "reference")
    if (.not. associated(ref)) then
      call raiseError("mCrossSection: entry '" // trim(id) // "' has no reference block (sigma, mur)")
      return
    end if
    cs%sigmaRef = json_real(ref, "sigma")
    cs%mur = 1.0d0
    if (json_has(ref, "mur")) cs%mur = json_real(ref, "mur")
    if (.not. (cs%sigmaRef > 0.0d0) .or. .not. (cs%mur > 0.0d0)) then
      call raiseError("mCrossSection: entry '" // trim(id) // "' needs reference.sigma > 0 and mur > 0")
      return
    end if
    cs%sigma = cs%sigmaRef
    cs%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)

    band => json_child(obj, "band")
    if (.not. associated(band)) then
      call raiseError("mCrossSection: entry '" // trim(id) // "' has no band block (fMin, fMax)")
      return
    end if
    cs%fMin = json_real(band, "fMin")
    cs%fMax = json_real(band, "fMax")
    if (.not. (cs%fMin > 0.0d0 .and. cs%fMax > cs%fMin)) then
      call raiseError("mCrossSection: entry '" // trim(id) // "' needs 0 < band.fMin < band.fMax")
      return
    end if

    model => json_child(obj, "model")
    if (.not. associated(model)) then
      call raiseError("mCrossSection: entry '" // trim(id) // "' has no model block")
      return
    end if
    kindStr = json_str(obj, "kind")
    select case (trim(kindStr))
    case ("poleResidue")
      cs%kind = CS_POLE_RESIDUE
      cs%d = json_real(model, "d")
      cs%e = json_real(model, "e")
      if (cs%e < 0.0d0) then
        call raiseError("mCrossSection: entry '" // trim(id) // "' has a negative model.e")
        return
      end if
      call readRealArray(model, "poles", cs%poles, ok)
      if (.not. ok) then
        call raiseError("mCrossSection: entry '" // trim(id) // "' needs a model.poles array of numbers")
        return
      end if
      call readRealArray(model, "residues", cs%residues, ok)
      if (.not. ok .or. size(cs%residues) /= size(cs%poles)) then
        call raiseError("mCrossSection: entry '" // trim(id) // "' needs model.residues of the same length as poles")
        return
      end if
      do n = 1, size(cs%poles)
        if (.not. (cs%poles(n) < 0.0d0)) then
          call raiseError("mCrossSection: entry '" // trim(id) // "' has a pole that is not strictly negative (unstable)")
          return
        end if
      end do
      call checkPassive(cs)
    case ("table")
      cs%kind = CS_TABLE
      call readRealArray(model, "f", cs%tabF, ok)
      if (ok) call readRealArray(model, "R", cs%tabR, ok)
      if (ok) call readRealArray(model, "X", cs%tabX, ok)
      if (.not. ok .or. size(cs%tabF) < 2 .or. size(cs%tabR) /= size(cs%tabF) .or. size(cs%tabX) /= size(cs%tabF)) then
        call raiseError("mCrossSection: table entry '" // trim(id) // "' needs model.f, R, X of equal length >= 2")
        return
      end if
      do n = 1, size(cs%tabF)
        if (.not. (cs%tabF(n) > 0.0d0) .or. .not. (cs%tabR(n) > 0.0d0) .or. &
            (n > 1 .and. cs%tabF(max(n - 1, 1)) >= cs%tabF(n))) then
          call raiseError("mCrossSection: table entry '" // trim(id) // &
                          "' needs ascending positive f and positive R")
          return
        end if
      end do
    case default
      call raiseError("mCrossSection: entry '" // trim(id) // "' has unknown kind '" // trim(kindStr) // &
                      "' (poleResidue or table)")
    end select
  end subroutine newCrossSectionFromJson

  subroutine checkPassive(cs)
    !! Warn if Re Z(jω) < 0 anywhere on a log grid over 4 decades around the band.
    type(tCrossSection), intent(in) :: cs
    integer :: i
    real(8) :: w, wLo, wHi, worst
    complex(8) :: z

    wLo = 2.0d0 * PI * cs%fMin * 1.0d-2
    wHi = 2.0d0 * PI * cs%fMax * 1.0d2
    worst = huge(1.0d0)
    do i = 0, 200
      w = wLo * (wHi / wLo) ** (real(i, kind=8) / 200.0d0)
      z = poleResidueAt(cs, cmplx(0.0d0, w, kind=8))
      worst = min(worst, real(z) / max(abs(z), tiny(1.0d0)))
    end do
    if (worst < -1.0d-9) then
      call emitMessage(MSG_WARNING, "mCrossSection: entry '" // trim(cs%id) // &
                       "' is not passive (Re Z < 0 for some frequency)")
    end if
  end subroutine checkPassive

  subroutine readRealArray(parent, key, arr, ok)
    type(tJsonValue), intent(in) :: parent
    character(len=*), intent(in) :: key
    real(8), allocatable, intent(out) :: arr(:)
    logical, intent(out) :: ok
    type(tJsonValue), pointer :: child, item
    integer :: i, n

    ok = .false.
    if (.not. json_has(parent, key)) return
    child => json_child(parent, key)
    if (json_value_type(child) /= JSON_ARRAY) return
    n = json_size(child)
    allocate(arr(n))
    do i = 1, n
      item => json_item(child, i)
      if (json_value_type(item) /= JSON_NUMBER) return
      arr(i) = json_value_real(item)
    end do
    ok = .true.
  end subroutine readRealArray

  subroutine loadCrossSectionFile(path, entries)
    !! Read a database file: one entry object, or `{ "entries": [ ... ] }`.
    character(len=*), intent(in) :: path
    type(tCrossSection), allocatable, intent(out) :: entries(:)
    type(tJsonValue) :: root
    type(tJsonValue), pointer :: arr, item
    integer :: i, n

    call parseJsonFile(path, root)
    if (errorRaised()) return
    if (json_has(root, "entries")) then
      arr => json_child(root, "entries")
      n = json_size(arr)
      allocate(entries(n))
      do i = 1, n
        item => json_item(arr, i)
        call newCrossSectionFromJson(item, entries(i))
        if (errorRaised()) return
      end do
    else
      allocate(entries(1))
      call newCrossSectionFromJson(root, entries(1))
    end if
  end subroutine loadCrossSectionFile

end module mCrossSection
