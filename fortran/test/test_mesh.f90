program test_mesh
  !! Pins the sign and propagation conventions of mMesh against theory.md
  !! (ADR 0008, ROADMAP Phase 0) and the ADR 0009 impedance-fill interface.
  use mCtes
  use mMesh
  use mMaterial
  use check
  implicit none

  type(tMesh) :: mesh, meshMutual, meshTopo
  type(tLinear) :: matAir, matSoil
  complex(8) :: gammaSoil, gammaAir, expectedSoil, expectedAir
  complex(8) :: fprop, fpropi, expZl, expZt
  real(8) :: omega, epsAir, muAir, sigmaAir, epsSoil, muSoil, sigmaSoil
  real(8) :: fAtD0, fAtD1, fAtD2
  integer(4) :: n1(1), n2(1)
  complex(8) :: sig(1)
  integer(4) :: pos(1)
  integer(4) :: info

  omega   = 2.0d0 * PI * 1.0d6
  epsAir   = EPSILON0
  muAir    = MU0
  sigmaAir = 0.0d0
  epsSoil = 10.0d0 * EPSILON0
  muSoil  = MU0
  sigmaSoil = 0.01d0

  ! ----------------------------------------------------------------
  ! Propagation constant: gamma = sqrt(j*omega*mu*(sigma+j*omega*eps))
  ! ----------------------------------------------------------------
  call test_init("calcParam propagation constant (theory.md §2)")

  call calcParam(mesh, omega, epsAir, muAir, sigmaAir, epsSoil, muSoil, sigmaSoil)

  gammaSoil = mesh%propSoil
  gammaAir   = mesh%propAir

  call test_ok("Re(gamma_soil) >= 0 (lossy medium attenuates)", &
               real(gammaSoil) >= 0.0d0, "propSoil has negative real part")

  call test_ok("Re(gamma_air) ~ 0 (ideal lossless air, sigma=0)", &
               abs(real(gammaAir)) < 1.0d-9 * abs(gammaAir), &
               "Ideal air should not attenuate")

  expectedSoil = cmplx(0.0d0, omega, kind=8) * muSoil * cmplx(sigmaSoil, omega * epsSoil, kind=8)
  call test_ok("gamma_soil**2 == j*omega*mu*(sigma+j*omega*eps)", &
               abs(gammaSoil**2 - expectedSoil) < 1.0d-12 * abs(expectedSoil), &
               "propSoil does not satisfy the defining equation")

  expectedAir = cmplx(0.0d0, omega, kind=8) * muAir * cmplx(sigmaAir, omega * epsAir, kind=8)
  call test_ok("gamma_air**2 == j*omega*mu*(sigma+j*omega*eps)", &
               abs(gammaAir**2 - expectedAir) < 1.0d-12 * max(1.0d0, abs(expectedAir)), &
               "propAir does not satisfy the defining equation")

  ! ----------------------------------------------------------------
  ! Propagation factor F(R) = exp(-gamma*R) decays with distance
  ! ----------------------------------------------------------------
  call test_init("Propagation factor decay (theory.md §2)")

  fAtD0 = abs(exp(-cmplx(0.0d0, 0.0d0, kind=8) * gammaSoil))
  fAtD1 = abs(exp(-cmplx(1.0d0, 0.0d0, kind=8) * gammaSoil))
  fAtD2 = abs(exp(-cmplx(5.0d0, 0.0d0, kind=8) * gammaSoil))

  call test_ok("|F(0)| == 1", abs(fAtD0 - 1.0d0) < 1.0d-12, "F(0) must be unity")
  call test_ok("|F(1)| < |F(0)|", fAtD1 < fAtD0, "Propagation factor must decay with distance")
  call test_ok("|F(5)| < |F(1)|", fAtD2 < fAtD1, "Propagation factor must keep decaying")

  ! ----------------------------------------------------------------
  ! tLinear propagation constant follows the engineering convention
  ! (theory.md §2): gamma² = -omega²*mu*eps + j*omega*mu*sigma, Re >= 0.
  ! Must agree with calcParam for the same parameters.
  ! ----------------------------------------------------------------
  call test_init("tLinear%calcPropagationConstant convention (theory.md §2)")

  matAir = newMaterialLinear("air", 1.0d0, 1.0d0, 0.0d0)
  call matAir%calcPropagationConstant(omega)
  call test_ok("lossless medium: gamma purely imaginary (phase delay only)", &
               abs(real(matAir%propagationConstant)) &
                 < 1.0d-9 * abs(matAir%propagationConstant), &
               "a lossless medium must not attenuate")
  call test_ok("lossless medium: Im(gamma) == omega*sqrt(mu*eps)", &
               abs(aimag(matAir%propagationConstant) - omega * sqrt(MU0 * EPSILON0)) &
                 < 1.0d-12 * omega * sqrt(MU0 * EPSILON0), &
               "phase constant must be omega/c in air")

  matSoil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
  call matSoil%calcPropagationConstant(omega)
  call test_ok("tLinear gamma matches calcParam gamma for identical media", &
               abs(matSoil%propagationConstant - gammaSoil) < 1.0d-12 * abs(gammaSoil), &
               "mMaterial and mMesh disagree on the propagation constant")

  ! ----------------------------------------------------------------
  ! Self impedance (ADR 0009 interface): theory factors applied inside;
  ! image sign "-" in air, "+" in soil (theory.md §4.3, §5)
  ! ----------------------------------------------------------------
  call test_init("calcZSelf image sign and factors (theory.md §4.3, §5)")

  call initMesh(mesh, 2, 1)
  call calcParam(mesh, omega, epsAir, muAir, sigmaAir, epsSoil, muSoil, sigmaSoil)

  ! air: d=r0=0.01, di=1.0, l=2.0, g=2.0, gi=2.0, cosThetaI=+1
  call calcZSelf(mesh, 1, 1, 0.01d0, 1.0d0, 2.0d0, ZERO_CPLX, 2.0d0, 2.0d0, 1.0d0)
  fprop  = exp(-cmplx(0.01d0, 0.0d0, kind=8) * mesh%propAir)
  fpropi = exp(-cmplx(1.0d0, 0.0d0, kind=8) * mesh%propAir)
  expZl = mesh%cMAir * (fprop * 2.0d0 - fpropi * 2.0d0)
  expZt = mesh%cEAir * (fprop * 2.0d0 - fpropi * 2.0d0) / 4.0d0
  call test_ok("Zlong self (air) uses '-' image sign", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "Air longitudinal self impedance must subtract the image term")
  call test_ok("Ztrans self (air) uses '-' image sign and 1/l^2", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "Air transversal self impedance must subtract the image term")

  ! soil, horizontal (cosThetaI=+1)
  call calcZSelf(mesh, 1, 2, 0.01d0, 1.0d0, 2.0d0, ZERO_CPLX, 2.0d0, 2.0d0, 1.0d0)
  fprop  = exp(-cmplx(0.01d0, 0.0d0, kind=8) * mesh%propSoil)
  fpropi = exp(-cmplx(1.0d0, 0.0d0, kind=8) * mesh%propSoil)
  expZl = mesh%cMSoil * (fprop * 2.0d0 + fpropi * 2.0d0)
  expZt = mesh%cESoil * (fprop * 2.0d0 + fpropi * 2.0d0) / 4.0d0
  call test_ok("Zlong self (soil) uses '+' image sign", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "Soil longitudinal self impedance must add the image term")
  call test_ok("Ztrans self (soil) uses '+' image sign and 1/l^2", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "Soil transversal self impedance must add the image term")

  ! soil, vertical segment (cosThetaI=-1): the image direction cosine flips
  ! the longitudinal image parcel; the transversal one is unaffected
  call calcZSelf(mesh, 1, 2, 0.01d0, 1.0d0, 2.0d0, ZERO_CPLX, 2.0d0, 2.0d0, -1.0d0)
  expZl = mesh%cMSoil * (fprop * 2.0d0 - fpropi * 2.0d0)
  call test_ok("Zlong self (soil, vertical) carries cosThetaI=-1", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "vertical segment's longitudinal image parcel must flip sign")
  call test_ok("Ztrans self (soil, vertical) ignores cosThetaI", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "transversal self impedance must not depend on cosThetaI")

  ! ----------------------------------------------------------------
  ! calcZMutual: hand-evaluated value and symmetry (ADR 0009 interface)
  ! ----------------------------------------------------------------
  call test_init("calcZMutual value and symmetry")

  call initMesh(meshMutual, 2, 2)
  call calcParam(meshMutual, omega, epsAir, muAir, sigmaAir, epsSoil, muSoil, sigmaSoil)
  ! both soil: d=3, di=4, la=2, lb=1, g=1.5, gi=1.2, cosTheta=0.8, cosThetaI=0.6
  call calcZMutual(meshMutual, 1, 2, 2, 2, 3.0d0, 4.0d0, 2.0d0, 1.0d0, &
                  1.5d0, 1.2d0, 0.8d0, 0.6d0)
  fprop  = exp(-cmplx(3.0d0, 0.0d0, kind=8) * meshMutual%propSoil)
  fpropi = exp(-cmplx(4.0d0, 0.0d0, kind=8) * meshMutual%propSoil)
  expZt = meshMutual%cESoil * (fprop * 1.5d0 + fpropi * 1.2d0) / 2.0d0
  expZl = meshMutual%cMSoil * (0.8d0 * fprop * 1.5d0 + 0.6d0 * fpropi * 1.2d0)
  call test_ok("Ztrans(1,2) matches the hand-evaluated theory expression", &
               abs(meshMutual%Ztrans(1,2) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "transversal mutual impedance must apply e^{-gamma d}, +image, 1/(la*lb)")
  call test_ok("Zlong(1,2) matches the hand-evaluated theory expression", &
               abs(meshMutual%Zlong(1,2) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "longitudinal mutual impedance must apply cosTheta and cosThetaI")
  call test_ok("Ztrans(i,j) == Ztrans(j,i)", &
               meshMutual%Ztrans(1,2) == meshMutual%Ztrans(2,1), "Transversal impedance must be symmetric")
  call test_ok("Zlong(i,j) == Zlong(j,i)", &
               meshMutual%Zlong(1,2) == meshMutual%Zlong(2,1), "Longitudinal impedance must be symmetric")

  ! mixed media: coupling neglected (theory.md §5 / ADR 0005)
  call calcZMutual(meshMutual, 1, 2, 1, 2, 3.0d0, 4.0d0, 2.0d0, 1.0d0, &
                  1.5d0, 1.2d0, 0.8d0, 0.6d0)
  call test_ok("mixed-media pair is zeroed", &
               abs(meshMutual%Ztrans(1,2)) == 0.0d0 .and. abs(meshMutual%Zlong(1,2)) == 0.0d0, &
               "air-soil segment pairs must have neglected (zero) coupling")

  ! ----------------------------------------------------------------
  ! Topology matrices A, B, C, D (theory.md §6)
  ! ----------------------------------------------------------------
  call test_init("calcTopology matrix entries (theory.md §6)")

  call initMesh(meshTopo, 2, 1)
  n1 = [1]
  n2 = [2]
  call calcTopology(meshTopo, 1, n1, n2)

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
  call test_init("calcFreq2 + injectSignal end-to-end solve")

  call setZ(meshTopo, 1, 1, cmplx(2.0d0,0.0d0,kind=8), cmplx(2.0d0,0.0d0,kind=8))
  call calcFreq2(meshTopo)

  pos = [1]
  sig = [cmplx(1.0d0, 0.0d0, kind=8)]
  info = injectSignal(meshTopo, 1, pos, sig)

  call test_ok("injectSignal returns success", info == 0, "ZGESV reported an error")
  call test_ok("u(1) == 2.5", abs(meshTopo%voltage(1) - cmplx(2.5d0,0.0d0,kind=8)) < 1.0d-10, &
               "Node 1 voltage does not match the hand-solved system")
  call test_ok("u(2) == 1.5", abs(meshTopo%voltage(2) - cmplx(1.5d0,0.0d0,kind=8)) < 1.0d-10, &
               "Node 2 voltage does not match the hand-solved system")
  call test_ok("i1(1) == 1.0", abs(meshTopo%current1(1) - cmplx(1.0d0,0.0d0,kind=8)) < 1.0d-10, &
               "Longitudinal current does not match the hand-solved system")
  call test_ok("i2(1) == 0.0", abs(meshTopo%current2(1)) < 1.0d-10, &
               "Transversal current does not match the hand-solved system")

  call test_summary()

end program test_mesh
