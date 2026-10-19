program test_crossmedium
  !! Air-soil cross-medium coupling (ROADMAP Phase 14 item 1, theory.md §5.1,
  !! ADR 0029): kernel of a mixed pair, reciprocity, the homogeneous-medium
  !! limit, geometry bookkeeping and an end-to-end rod_air run.
  use mCtes
  use mMesh
  use mGeometry
  use mStudy
  use tupa, only: runStudyFromFile
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use check
  implicit none

  type(tMesh) :: m
  complex(8) :: Wa, Ws, expZt, expZl, zt, zl
  real(8) :: omega
  real(8) :: p1(2,3), p2(2,3), rad(2)
  real(8) :: G(2,2), Gi(2,2), Rb(2,2), Rbi(2,2), ct(2,2), cti(2,2)
  integer(4) :: pos(2)
  type(tStudy) :: sOn, sOff
  integer :: iTop, k
  real(8) :: rel(3)

  omega = 2.0d0 * PI * 1.0d5
  Wa = cmplx(0.0d0, omega * EPSILON0, kind=8)
  Ws = cmplx(0.01d0, omega * 10.0d0 * EPSILON0, kind=8)

  call test_init("cross-medium mixed-pair kernel (theory.md §5.1)")
  call initMesh(m, 2, 2)
  m%crossMedium = .true.
  call calcParamW(m, omega, MU0, Wa, MU0, Ws)
  ! air segment 1, soil segment 2: d = 6 m total, 4 m of it in air
  call calcZMutual(m, 1, 2, 1, 2, 6.0d0, 4.0d0, 2.0d0, 3.0d0, 1.5d0, 0.0d0, 0.5d0, 0.0d0)
  expZt = exp(-4.0d0 * m%propAir - 2.0d0 * m%propSoil) * 1.5d0 / (2.0d0 * PI * (Wa + Ws)) / 6.0d0
  expZl = exp(-4.0d0 * m%propAir - 2.0d0 * m%propSoil) * 1.5d0 * 0.5d0 * cmplx(0.0d0, omega * MU0 / FOUR_PI, kind=8)
  call test_ok("Ztrans = g e^{-g.R}/(2 pi (Wa+Ws) la lb)", abs(m%Ztrans(1,2) - expZt) < 1.0d-12 * abs(expZt), &
               "mixed Zt must carry 1/(2 pi (Wa+Ws)) and the piecewise retardation")
  call test_ok("Zlong = cM cosTheta g e^{-g.R}", abs(m%Zlong(1,2) - expZl) < 1.0d-12 * abs(expZl), &
               "mixed Zl is the direct term only")
  call test_ok("symmetric", m%Ztrans(1,2) == m%Ztrans(2,1) .and. m%Zlong(1,2) == m%Zlong(2,1), &
               "mixed block must be reciprocal")

  ! Reciprocity of the constant: 2 cEa cEs/(cEa+cEs) is symmetric in the media
  call test_ok("cEMixed = 2 cEa cEs/(cEa+cEs)", &
               abs(m%cEMixed - 2.0d0 * m%cEAir * m%cESoil / (m%cEAir + m%cESoil)) < 1.0d-12 * abs(m%cEMixed), &
               "mixed constant must be the harmonic-mean form")

  ! Homogeneous limit: equal media => Gamma = 0 and the mixed kernel equals
  ! the same-medium direct term cE e^{-gamma d} g/(la lb)
  call calcParamW(m, omega, MU0, Ws, MU0, Ws)
  call calcZMutual(m, 1, 2, 1, 2, 6.0d0, 4.0d0, 2.0d0, 3.0d0, 1.5d0, 0.0d0, 0.5d0, 0.0d0)
  expZt = m%cESoil * exp(-6.0d0 * m%propSoil) * 1.5d0 / 6.0d0
  call test_ok("equal media: mixed Zt equals the homogeneous direct term", &
               abs(m%Ztrans(1,2) - expZt) < 1.0d-12 * abs(expZt), "must reduce to the uniform-medium kernel")
  call test_ok("equal media: image coefficient vanishes", abs(m%gammaSoil) < 1.0d-14, "Gamma = 0 for identical media")

  ! Switch off: legacy behaviour
  m%crossMedium = .false.
  call calcZMutual(m, 1, 2, 1, 2, 6.0d0, 4.0d0, 2.0d0, 3.0d0, 1.5d0, 0.0d0, 0.5d0, 0.0d0)
  zt = m%Ztrans(1,2); zl = m%Zlong(1,2)
  call test_ok("switch off: mixed pair zero", abs(zt) == 0.0d0 .and. abs(zl) == 0.0d0, "default must neglect coupling")

  call test_init("cross-medium geometry bookkeeping")
  ! vertical collinear: air segment z 0..4 (mid 2), soil segment z 0..-2 (mid -1)
  p1(1,:) = [0.0d0, 0.0d0, 4.0d0]; p2(1,:) = [0.0d0, 0.0d0, 0.0d0]
  p1(2,:) = [0.0d0, 0.0d0, 0.0d0]; p2(2,:) = [0.0d0, 0.0d0, -2.0d0]
  rad = 0.01d0; pos = [1, 2]
  call buildGeometryMatrices(p1, p2, rad, 2, G, Gi, Rb, Rbi, ct, cti, pos=pos)
  call test_ok("off: mixed entries zero", G(1,2) == 0.0d0 .and. Rbi(1,2) == 0.0d0, "legacy path")
  call buildGeometryMatrices(p1, p2, rad, 2, G, Gi, Rb, Rbi, ct, cti, pos=pos, crossMedium=.true.)
  call test_ok("on: direct G is positive and symmetric", G(1,2) > 0.0d0 .and. G(1,2) == G(2,1), "g must be computed")
  call test_ok("on: Rbar = midpoint distance", abs(Rb(1,2) - 3.0d0) < 1.0d-12, "R = 2 - (-1)")
  call test_ok("on: Rbari = air part (2/3 of 3 m)", abs(Rbi(1,2) - 2.0d0) < 1.0d-12 .and. Rbi(2,1) == Rbi(1,2), &
               "ray splits at z = 0")
  call test_ok("on: no image terms", Gi(1,2) == 0.0d0 .and. cti(1,2) == 0.0d0, "transmitted field has no image")
  call test_ok("on: cosTheta of the collinear pair (+/-1 magnitude)", abs(abs(ct(1,2)) - 1.0d0) < 1.0d-12, "direction cosine")

  call test_init("channel above a buried rod: coupling grows with frequency (ROADMAP Phase 14 item 2)")
  call setVerbosity(VERB_QUIET)
  call runStudyFromFile("../common/channel_tower.json", sOff)
  call runStudyFromFile("../common/channel_tower_cross.json", sOn)
  iTop = sOn%structure%findNodeIndex("Ttop")
  do k = 1, 3
    rel(k) = abs(sOn%voltageResults%get(iTop, k) / sOff%voltageResults%get(iTop, k) - 1.0d0)
  end do
  call test_ok("finite results", all(rel == rel), "NaN in channel + cross-medium sweep")
  call test_ok("10 kHz: coupling effect < 1 %", rel(1) < 1.0d-2, "quasi-static limit should barely move")
  call test_ok("1 MHz: coupling effect > 1 % and < 50 %", rel(3) > 1.0d-2 .and. rel(3) < 0.5d0, &
               "channel-to-buried-rod coupling should be visible but moderate at 1 MHz")
  call test_ok("effect increases with frequency", rel(1) < rel(2) .and. rel(2) < rel(3), "monotone growth")

  call test_summary()
  call test_close()
end program test_crossmedium
