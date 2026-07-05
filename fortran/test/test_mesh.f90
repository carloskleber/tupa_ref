program test_mesh
  !! Pins the sign and propagation conventions of mMesh against theory.md
  !! (ADR 0008, ROADMAP Phase 0) and the ADR 0009 impedance-fill interface.
  use mCtes
  use mMesh
  use mMaterial
  use check
  implicit none

  type(tMesh) :: mesh, meshMutua, meshTopo
  type(tLinear) :: matAir, matSoil
  complex(8) :: gammaSolo, gammaAr, expectedSolo, expectedAr
  complex(8) :: fprop, fpropi, expZl, expZt
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
               abs(matSoil%propagationConstant - gammaSolo) < 1.0d-12 * abs(gammaSolo), &
               "mMaterial and mMesh disagree on the propagation constant")

  ! ----------------------------------------------------------------
  ! Self impedance (ADR 0009 interface): theory factors applied inside;
  ! image sign "-" in air, "+" in soil (theory.md §4.3, §5)
  ! ----------------------------------------------------------------
  call test_init("calcZPropria image sign and factors (theory.md §4.3, §5)")

  call initMesh(mesh, 2, 1)
  call calcParam(mesh, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)

  ! air: d=r0=0.01, di=1.0, l=2.0, g=2.0, gi=2.0, cosThetaI=+1
  call calcZPropria(mesh, 1, 1, 0.01d0, 1.0d0, 2.0d0, ZERO_CPLX, 2.0d0, 2.0d0, 1.0d0)
  fprop  = exp(-cmplx(0.01d0, 0.0d0, kind=8) * mesh%propAr)
  fpropi = exp(-cmplx(1.0d0, 0.0d0, kind=8) * mesh%propAr)
  expZl = mesh%cteMagAr * (fprop * 2.0d0 - fpropi * 2.0d0)
  expZt = mesh%cteEletAr * (fprop * 2.0d0 - fpropi * 2.0d0) / 4.0d0
  call test_ok("Zlong self (air) uses '-' image sign", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "Air longitudinal self impedance must subtract the image term")
  call test_ok("Ztrans self (air) uses '-' image sign and 1/l^2", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "Air transversal self impedance must subtract the image term")

  ! soil, horizontal (cosThetaI=+1)
  call calcZPropria(mesh, 1, 2, 0.01d0, 1.0d0, 2.0d0, ZERO_CPLX, 2.0d0, 2.0d0, 1.0d0)
  fprop  = exp(-cmplx(0.01d0, 0.0d0, kind=8) * mesh%propSolo)
  fpropi = exp(-cmplx(1.0d0, 0.0d0, kind=8) * mesh%propSolo)
  expZl = mesh%cteMagSolo * (fprop * 2.0d0 + fpropi * 2.0d0)
  expZt = mesh%cteEletSolo * (fprop * 2.0d0 + fpropi * 2.0d0) / 4.0d0
  call test_ok("Zlong self (soil) uses '+' image sign", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "Soil longitudinal self impedance must add the image term")
  call test_ok("Ztrans self (soil) uses '+' image sign and 1/l^2", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "Soil transversal self impedance must add the image term")

  ! soil, vertical segment (cosThetaI=-1): the image direction cosine flips
  ! the longitudinal image parcel; the transversal one is unaffected
  call calcZPropria(mesh, 1, 2, 0.01d0, 1.0d0, 2.0d0, ZERO_CPLX, 2.0d0, 2.0d0, -1.0d0)
  expZl = mesh%cteMagSolo * (fprop * 2.0d0 - fpropi * 2.0d0)
  call test_ok("Zlong self (soil, vertical) carries cosThetaI=-1", &
               abs(mesh%Zlong(1,1) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "vertical segment's longitudinal image parcel must flip sign")
  call test_ok("Ztrans self (soil, vertical) ignores cosThetaI", &
               abs(mesh%Ztrans(1,1) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "transversal self impedance must not depend on cosThetaI")

  ! ----------------------------------------------------------------
  ! calcZMutua: hand-evaluated value and symmetry (ADR 0009 interface)
  ! ----------------------------------------------------------------
  call test_init("calcZMutua value and symmetry")

  call initMesh(meshMutua, 2, 2)
  call calcParam(meshMutua, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)
  ! both soil: d=3, di=4, la=2, lb=1, g=1.5, gi=1.2, cosTheta=0.8, cosThetaI=0.6
  call calcZMutua(meshMutua, 1, 2, 2, 2, 3.0d0, 4.0d0, 2.0d0, 1.0d0, &
                  1.5d0, 1.2d0, 0.8d0, 0.6d0)
  fprop  = exp(-cmplx(3.0d0, 0.0d0, kind=8) * meshMutua%propSolo)
  fpropi = exp(-cmplx(4.0d0, 0.0d0, kind=8) * meshMutua%propSolo)
  expZt = meshMutua%cteEletSolo * (fprop * 1.5d0 + fpropi * 1.2d0) / 2.0d0
  expZl = meshMutua%cteMagSolo * (0.8d0 * fprop * 1.5d0 + 0.6d0 * fpropi * 1.2d0)
  call test_ok("Ztrans(1,2) matches the hand-evaluated theory expression", &
               abs(meshMutua%Ztrans(1,2) - expZt) < 1.0d-12 * max(1.0d0, abs(expZt)), &
               "transversal mutual impedance must apply e^{-gamma d}, +image, 1/(la*lb)")
  call test_ok("Zlong(1,2) matches the hand-evaluated theory expression", &
               abs(meshMutua%Zlong(1,2) - expZl) < 1.0d-12 * max(1.0d0, abs(expZl)), &
               "longitudinal mutual impedance must apply cosTheta and cosThetaI")
  call test_ok("Ztrans(i,j) == Ztrans(j,i)", &
               meshMutua%Ztrans(1,2) == meshMutua%Ztrans(2,1), "Transversal impedance must be symmetric")
  call test_ok("Zlong(i,j) == Zlong(j,i)", &
               meshMutua%Zlong(1,2) == meshMutua%Zlong(2,1), "Longitudinal impedance must be symmetric")

  ! mixed media: coupling neglected (theory.md §5 / ADR 0005)
  call calcZMutua(meshMutua, 1, 2, 1, 2, 3.0d0, 4.0d0, 2.0d0, 1.0d0, &
                  1.5d0, 1.2d0, 0.8d0, 0.6d0)
  call test_ok("mixed-media pair is zeroed", &
               abs(meshMutua%Ztrans(1,2)) == 0.0d0 .and. abs(meshMutua%Zlong(1,2)) == 0.0d0, &
               "air-soil segment pairs must have neglected (zero) coupling")

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
