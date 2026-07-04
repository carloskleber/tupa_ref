program test_mesh
  !! Pins the sign and propagation conventions of mMesh against theory.md
  !! (ADR 0008, implementation-plan.md Phase 0).
  use mCtes
  use mMesh
  use check
  implicit none

  type(tMesh) :: mesh, meshMutua, meshTopo
  complex(8) :: gammaSolo, gammaAr, expectedSolo, expectedAr
  complex(8) :: fpropi, expZl, expZt
  real(8) :: omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo
  real(8) :: fAtD0, fAtD1, fAtD2
  integer(4) :: n1(1), n2(1)
  complex(8) :: sig(1)
  integer(4) :: pos(1)
  integer(4) :: info

  omega   = 2.0d0 * PI * 1.0d6
  epsAr   = EPSILON0
  muAr    = MU0
  sigmaAr = 0.0d0
  epsSolo = 10.0d0 * EPSILON0
  muSolo  = MU0
  sigmaSolo = 0.01d0

  ! ----------------------------------------------------------------
  ! Propagation constant: gamma = sqrt(j*omega*mu*(sigma+j*omega*eps))
  ! ----------------------------------------------------------------
  call test_init("calcParam propagation constant (theory.md §2)")

  call calcParam(mesh, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)

  gammaSolo = mesh%propSolo
  gammaAr   = mesh%propAr

  call test_ok("Re(gamma_soil) >= 0 (lossy medium attenuates)", &
               real(gammaSolo) >= 0.0d0, "propSolo has negative real part")

  call test_ok("Re(gamma_air) ~ 0 (ideal lossless air, sigma=0)", &
               abs(real(gammaAr)) < 1.0d-9 * abs(gammaAr), &
               "Ideal air should not attenuate")

  expectedSolo = cmplx(0.0d0, omega, kind=8) * muSolo * cmplx(sigmaSolo, omega * epsSolo, kind=8)
  call test_ok("gamma_soil**2 == j*omega*mu*(sigma+j*omega*eps)", &
               abs(gammaSolo**2 - expectedSolo) < 1.0d-12 * abs(expectedSolo), &
               "propSolo does not satisfy the defining equation")

  expectedAr = cmplx(0.0d0, omega, kind=8) * muAr * cmplx(sigmaAr, omega * epsAr, kind=8)
  call test_ok("gamma_air**2 == j*omega*mu*(sigma+j*omega*eps)", &
               abs(gammaAr**2 - expectedAr) < 1.0d-12 * max(1.0d0, abs(expectedAr)), &
               "propAr does not satisfy the defining equation")

  ! ----------------------------------------------------------------
  ! Propagation factor F(R) = exp(-gamma*R) decays with distance
  ! ----------------------------------------------------------------
  call test_init("Propagation factor decay (theory.md §2)")

  fAtD0 = abs(exp(-cmplx(0.0d0, 0.0d0, kind=8) * gammaSolo))
  fAtD1 = abs(exp(-cmplx(1.0d0, 0.0d0, kind=8) * gammaSolo))
  fAtD2 = abs(exp(-cmplx(5.0d0, 0.0d0, kind=8) * gammaSolo))

  call test_ok("|F(0)| == 1", abs(fAtD0 - 1.0d0) < 1.0d-12, "F(0) must be unity")
  call test_ok("|F(1)| < |F(0)|", fAtD1 < fAtD0, "Propagation factor must decay with distance")
  call test_ok("|F(5)| < |F(1)|", fAtD2 < fAtD1, "Propagation factor must keep decaying")

  ! ----------------------------------------------------------------
  ! Self impedance image sign: "-" in air, "+" in soil (theory.md §5)
  ! ----------------------------------------------------------------
  call test_init("calcZPropria image sign (theory.md §5)")

  call initMesh(mesh, 2, 1)
  call calcParam(mesh, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)

  call calcZPropria(mesh, 1, 1, 1.0d0, ZERO_CPLX, 2.0d0, 2.0d0, 3.0d0, 3.0d0)
  fpropi = exp(-cmplx(1.0d0, 0.0d0, kind=8) * mesh%propAr)
  expZl = mesh%cteMagAr * (cmplx(2.0d0, 0.0d0, kind=8) - fpropi * cmplx(2.0d0, 0.0d0, kind=8))
  expZt = mesh%cteEletAr * (cmplx(3.0d0, 0.0d0, kind=8) - fpropi * cmplx(3.0d0, 0.0d0, kind=8))
  call test_ok("Zlong self (air) uses '-' image sign", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "Air longitudinal self impedance must subtract the image term")
  call test_ok("Ztrans self (air) uses '-' image sign", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "Air transversal self impedance must subtract the image term")

  call calcZPropria(mesh, 1, 2, 1.0d0, ZERO_CPLX, 2.0d0, 2.0d0, 3.0d0, 3.0d0)
  fpropi = exp(-cmplx(1.0d0, 0.0d0, kind=8) * mesh%propSolo)
  expZl = mesh%cteMagSolo * (cmplx(2.0d0, 0.0d0, kind=8) + fpropi * cmplx(2.0d0, 0.0d0, kind=8))
  expZt = mesh%cteEletSolo * (cmplx(3.0d0, 0.0d0, kind=8) + fpropi * cmplx(3.0d0, 0.0d0, kind=8))
  call test_ok("Zlong self (soil) uses '+' image sign", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "Soil longitudinal self impedance must add the image term")
  call test_ok("Ztrans self (soil) uses '+' image sign", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "Soil transversal self impedance must add the image term")

  ! ----------------------------------------------------------------
  ! calcZMutua symmetry
  ! ----------------------------------------------------------------
  call test_init("calcZMutua symmetry")

  call initMesh(meshMutua, 2, 2)
  call calcParam(meshMutua, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)
  call calcZMutua(meshMutua, 1, 2, 2, 2, 3.0d0, 4.0d0, 1.5d0, 1.2d0, 0.8d0, 0.6d0)
  call test_ok("Ztrans(i,j) == Ztrans(j,i)", &
               meshMutua%Ztrans(1,2) == meshMutua%Ztrans(2,1), "Transversal impedance must be symmetric")
  call test_ok("Zlong(i,j) == Zlong(j,i)", &
               meshMutua%Zlong(1,2) == meshMutua%Zlong(2,1), "Longitudinal impedance must be symmetric")

  ! ----------------------------------------------------------------
  ! Topology matrices A, B, C, D (theory.md §6)
  ! ----------------------------------------------------------------
  call test_init("calcTopologia matrix entries (theory.md §6)")

  call initMesh(meshTopo, 2, 1)
  n1 = [1]
  n2 = [2]
  call calcTopologia(meshTopo, 1, n1, n2)

  call test_ok("A(1,n1)=-1, A(1,n2)=+1", &
               meshTopo%A(1,1) == cmplx(-1.0d0,0.0d0,kind=8) .and. meshTopo%A(1,2) == ONE_CPLX, &
               "A row does not match theory.md §6")
  call test_ok("B(1,n1)=B(1,n2)=-1/2", &
               meshTopo%B(1,1) == cmplx(-0.5d0,0.0d0,kind=8) .and. meshTopo%B(1,2) == cmplx(-0.5d0,0.0d0,kind=8), &
               "B row does not match theory.md §6")
  call test_ok("C(n1,1)=+1, C(n2,1)=0", &
               meshTopo%C(1,1) == ONE_CPLX .and. meshTopo%C(2,1) == ZERO_CPLX, &
               "C column does not match theory.md §6")
  call test_ok("D(n2,1)=+1, D(n1,1)=0", &
               meshTopo%D(2,1) == ONE_CPLX .and. meshTopo%D(1,1) == ZERO_CPLX, &
               "D column does not match theory.md §6")

  ! ----------------------------------------------------------------
  ! End-to-end solve on a trivial 2-node, 1-segment circuit, hand-solved
  ! independently: Zlong = Ztrans = 2+0j, 1 A injected at node 1.
  ! Expected (Cramer's rule on the 4x4 Zeq system): u=[2.5,1.5], i1=[1], i2=[0].
  ! ----------------------------------------------------------------
  call test_init("calcFreq2 + injetaSinalF end-to-end solve")

  call setZ(meshTopo, 1, 1, cmplx(2.0d0,0.0d0,kind=8), cmplx(2.0d0,0.0d0,kind=8))
  call calcFreq2(meshTopo)

  pos = [1]
  sig = [cmplx(1.0d0, 0.0d0, kind=8)]
  info = injetaSinalF(meshTopo, 1, pos, sig)

  call test_ok("injetaSinalF returns success", info == 0, "ZGESV reported an error")
  call test_ok("u(1) == 2.5", abs(meshTopo%tensao(1) - cmplx(2.5d0,0.0d0,kind=8)) < 1.0d-10, &
               "Node 1 voltage does not match the hand-solved system")
  call test_ok("u(2) == 1.5", abs(meshTopo%tensao(2) - cmplx(1.5d0,0.0d0,kind=8)) < 1.0d-10, &
               "Node 2 voltage does not match the hand-solved system")
  call test_ok("i1(1) == 1.0", abs(meshTopo%corrente1(1) - cmplx(1.0d0,0.0d0,kind=8)) < 1.0d-10, &
               "Longitudinal current does not match the hand-solved system")
  call test_ok("i2(1) == 0.0", abs(meshTopo%corrente2(1)) < 1.0d-10, &
               "Transversal current does not match the hand-solved system")

  call test_summary()

end program test_mesh
