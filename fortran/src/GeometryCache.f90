module mGeometryCache
  !! Memoisation table for quadrature-computed mutual geometry factors.
  !!
  !! The adaptive 2D quadrature (`mImpedance%geometryFactor2D`/`TWODQ`) is the
  !! dominant cost of `buildGeometryMatrices`. Real geometries are highly
  !! repetitive — consecutive segments of the same line, translated copies in
  !! a grid — so many segment pairs are congruent and share the same geometry
  !! factor g(a,b) = ∫∫ ds_a ds_b / r, which is invariant under rigid motions
  !! and reflections.
  !!
  !! **Key.** A pair of straight segments is determined up to congruence by
  !! six numbers: the two segment lengths (la, lb) and the four cross
  !! endpoint distances |a_i - b_j| (the six pairwise distances of the four
  !! endpoints; a Euclidean distance matrix fixes the point set up to
  !! isometry + reflection). The four cross distances alone are NOT enough —
  !! two pairs can share all four while differing in length, and hence in g
  !! (pinned by a test in test_geometry.f90). Each number is rounded to
  !! `SIG_DIGITS` significant digits so that floating-point noise from
  !! differently-computed but congruent endpoints still maps to the same key;
  !! the geometric perturbation this equates (~1e-9 relative) is far below
  !! the quadrature tolerance (~1e-6). The key is then canonicalised over the
  !! 8 labelling symmetries g is invariant to (swap a<->b, reverse either
  !! segment's endpoint order), maximising hits regardless of how the mesh
  !! happened to orient each segment.
  !!
  !! **Storage.** Open-addressing hash table (linear probing, power-of-two
  !! capacity, grows at ~70% load). Entries are only ever inserted, never
  !! evicted; `geomCacheClear` resets table and statistics (called once per
  !! `buildGeometryMatrices`, and when the quadrature tolerance changes,
  !! since cached values embed the tolerance they were computed at).
  !!
  !! **Not thread-safe**: lookups and inserts mutate shared module state,
  !! same caveat as mImpedance's integration state (ARCHITECTURE.md §7).
  implicit none
  private

  public :: geomCacheKey, geomCacheGet, geomCachePut, geomCacheClear
  public :: geomCacheSetEnabled, geomCacheIsEnabled, geomCacheStats

  integer, parameter :: KEYLEN = 6
  !! Key length: (la, lb, d11, d12, d21, d22), canonicalised
  integer, parameter :: INITIAL_CAPACITY = 1024
  !! Initial table capacity (must be a power of two for the mask in hashSlot)
  integer, parameter :: SIG_DIGITS = 10
  !! Significant digits kept when quantising key distances

  real(8), allocatable, save :: tblKeys(:,:)
  !! Stored keys, shape (KEYLEN, capacity)
  real(8), allocatable, save :: tblVals(:)
  !! Stored geometry factors, one per slot
  logical, allocatable, save :: tblUsed(:)
  !! Slot-occupied flags
  integer, save :: tblCount = 0
  !! Number of occupied slots
  integer(8), save :: nHits = 0, nMisses = 0
  !! Lookup statistics since the last geomCacheClear
  logical, save :: cacheEnabled = .true.
  !! Master switch (CLI --no-cache); disabled lookups always miss silently

contains

  ! =====================================================================
  ! Key construction
  ! =====================================================================

  pure function geomCacheKey(a1, a2, la, b1, b2, lb) result(key)
    !! Canonical congruence key for the segment pair (a1-a2, b1-b2):
    !! quantised (la, lb, |a1-b1|, |a1-b2|, |a2-b1|, |a2-b2|), minimised
    !! lexicographically over the 8 symmetries of g (swap segments, reverse
    !! either segment).
    real(8), intent(in) :: a1(:), a2(:), la
    !! Endpoints and length of segment a (m)
    real(8), intent(in) :: b1(:), b2(:), lb
    !! Endpoints and length of segment b (m)
    real(8) :: key(KEYLEN)
    real(8) :: d(2,2), laq, lbq, m(2,2), cand(KEYLEN), l1, l2
    integer :: sw, ra, rb, r1, r2, c1, c2
    logical :: first

    d(1,1) = quantize(norm2(b1 - a1))
    d(1,2) = quantize(norm2(b2 - a1))
    d(2,1) = quantize(norm2(b1 - a2))
    d(2,2) = quantize(norm2(b2 - a2))
    laq = quantize(la)
    lbq = quantize(lb)

    first = .true.
    key = 0.0d0
    do sw = 0, 1
      if (sw == 0) then
        m = d;            l1 = laq; l2 = lbq
      else
        m = transpose(d); l1 = lbq; l2 = laq
      end if
      do ra = 0, 1
        r1 = 1 + ra; r2 = 2 - ra
        do rb = 0, 1
          c1 = 1 + rb; c2 = 2 - rb
          cand = [l1, l2, m(r1,c1), m(r1,c2), m(r2,c1), m(r2,c2)]
          if (first .or. lexLess(cand, key)) key = cand
          first = .false.
        end do
      end do
    end do
  end function geomCacheKey

  elemental real(8) function quantize(v) result(q)
    !! Round v > 0 to SIG_DIGITS significant digits (values below 1e-30 m,
    !! including exact zeros from segments sharing a node, collapse to 0).
    real(8), intent(in) :: v
    real(8) :: s

    if (v < 1.0d-30) then
      q = 0.0d0
    else
      s = 10.0d0 ** (SIG_DIGITS - 1 - floor(log10(v)))
      q = anint(v * s) / s
    end if
  end function quantize

  pure logical function lexLess(x, y)
    !! Lexicographic x < y over KEYLEN reals.
    real(8), intent(in) :: x(KEYLEN), y(KEYLEN)
    integer :: i

    do i = 1, KEYLEN
      if (x(i) < y(i)) then
        lexLess = .true.
        return
      else if (x(i) > y(i)) then
        lexLess = .false.
        return
      end if
    end do
    lexLess = .false.
  end function lexLess

  ! =====================================================================
  ! Lookup / insert
  ! =====================================================================

  logical function geomCacheGet(key, g) result(found)
    !! Look up a key; on a hit, return the cached geometry factor in g.
    !! Updates hit/miss statistics. Always misses when the cache is disabled.
    real(8), intent(in)  :: key(KEYLEN)
    real(8), intent(out) :: g

    integer :: slot

    g = 0.0d0
    found = .false.
    if (.not. cacheEnabled) return
    if (.not. allocated(tblUsed)) call allocTable(INITIAL_CAPACITY)

    slot = findSlot(key)
    if (tblUsed(slot)) then
      g = tblVals(slot)
      found = .true.
      nHits = nHits + 1
    else
      nMisses = nMisses + 1
    end if
  end function geomCacheGet

  subroutine geomCachePut(key, g)
    !! Insert a computed geometry factor. A slot already holding the key is
    !! left untouched (first value wins; any later value is identical up to
    !! the quadrature tolerance by construction of the key). No-op when the
    !! cache is disabled.
    real(8), intent(in) :: key(KEYLEN)
    real(8), intent(in) :: g
    integer :: slot

    if (.not. cacheEnabled) return
    if (.not. allocated(tblUsed)) call allocTable(INITIAL_CAPACITY)
    ! Grow before probing so the load factor stays below ~70% and linear
    ! probing chains stay short.
    if (10 * (tblCount + 1) > 7 * size(tblUsed)) call growTable()

    slot = findSlot(key)
    if (.not. tblUsed(slot)) then
      tblUsed(slot)   = .true.
      tblKeys(:,slot) = key
      tblVals(slot)   = g
      tblCount        = tblCount + 1
    end if
  end subroutine geomCachePut

  ! =====================================================================
  ! Lifecycle / statistics
  ! =====================================================================

  subroutine geomCacheClear()
    !! Empty the table (keeping its allocation) and reset statistics.
    if (allocated(tblUsed)) tblUsed = .false.
    tblCount = 0
    nHits    = 0
    nMisses  = 0
  end subroutine geomCacheClear

  subroutine geomCacheSetEnabled(flag)
    !! Master switch (CLI --no-cache). Disabling does not clear the table;
    !! re-enabling resumes with the previous contents.
    logical, intent(in) :: flag

    cacheEnabled = flag
  end subroutine geomCacheSetEnabled

  logical function geomCacheIsEnabled()
    geomCacheIsEnabled = cacheEnabled
  end function geomCacheIsEnabled

  subroutine geomCacheStats(hits, misses, entries)
    !! Lookup statistics and entry count since the last geomCacheClear.
    integer(8), intent(out) :: hits, misses
    integer,    intent(out) :: entries

    hits    = nHits
    misses  = nMisses
    entries = tblCount
  end subroutine geomCacheStats

  ! =====================================================================
  ! Internal hash-table plumbing
  ! =====================================================================

  integer function findSlot(key) result(slot)
    !! Linear probe from the key's hash slot to the slot holding the key,
    !! or the first empty slot if absent. Table must be allocated and never
    !! full (growTable keeps load below ~70%).
    real(8), intent(in) :: key(KEYLEN)

    slot = hashSlot(key, size(tblUsed))
    do while (tblUsed(slot))
      if (all(tblKeys(:,slot) == key)) return
      slot = slot + 1
      if (slot > size(tblUsed)) slot = 1
    end do
  end function findSlot

  pure integer function hashSlot(key, cap) result(slot)
    !! FNV-1a over the key's 64-bit patterns, masked to [1, cap]
    !! (cap must be a power of two). Quantised keys are compared exactly,
    !! so hashing raw bit patterns is deterministic. The multiply is
    !! expected to wrap on overflow (two's-complement, as gfortran does).
    real(8), intent(in) :: key(KEYLEN)
    integer, intent(in) :: cap
    integer(8) :: h
    integer :: i

    h = -3750763034362895579_8   ! FNV-1a 64-bit offset basis
    do i = 1, KEYLEN
      h = ieor(h, transfer(key(i), 0_8))
      h = h * 1099511628211_8    ! FNV-1a 64-bit prime
    end do
    slot = int(iand(h, int(cap - 1, 8))) + 1
  end function hashSlot

  subroutine allocTable(cap)
    !! Allocate an empty table of the given capacity.
    integer, intent(in) :: cap

    allocate(tblKeys(KEYLEN, cap), tblVals(cap), tblUsed(cap))
    tblUsed  = .false.
    tblCount = 0
  end subroutine allocTable

  subroutine growTable()
    !! Double the capacity and re-insert every occupied slot (hash slots
    !! depend on the capacity mask).
    real(8), allocatable :: oldKeys(:,:), oldVals(:)
    logical, allocatable :: oldUsed(:)
    integer :: i, slot, oldCap

    oldCap = size(tblUsed)
    call move_alloc(tblKeys, oldKeys)
    call move_alloc(tblVals, oldVals)
    call move_alloc(tblUsed, oldUsed)
    call allocTable(2 * oldCap)

    do i = 1, oldCap
      if (oldUsed(i)) then
        slot = findSlot(oldKeys(:,i))
        tblUsed(slot)   = .true.
        tblKeys(:,slot) = oldKeys(:,i)
        tblVals(slot)   = oldVals(i)
        tblCount        = tblCount + 1
      end if
    end do
  end subroutine growTable

end module mGeometryCache
