program test_marx
  !! Marx generator stage as a charged capacitor discharging into a load
  !! (docs/marx_generator.md). Reference: series R-L-C discharge of C
  !! charged to V0 into R_total = R_branch + R_load, whose load voltage is
  !! V0·R_load·h(t) with h = (1/L)·e^{-at}·sinh(wd t)/wd (overdamped for these values).
  !! The study holds the charged capacitor as its Norton equivalent: the
  !! branch (R, L, C) to remote earth plus the current source V0·h_branch(t).
  use mCtes, only: dp
  use mStudy
  use mSignal
  use mTransient
  use mNode
  use mMaterial
  use mElementLine
  use mElementBranch
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use check
  implicit none

  real(dp), parameter :: v0 = 100.0d3, rBr = 2.0_dp, lBr = 0.5d-6, cBr = 1.0d-9, rLoad = 50.0_dp
  integer(4), parameter :: nSamples = 4096
  type(tStudy) :: study
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  type(tBranchNortonSignal) :: norton
  type(tStepSignal) :: step
  type(tTransientOptions) :: opts
  real(dp), allocatable :: t(:), injected(:), resp(:,:), ref(:), tt(:)
  real(dp), allocatable :: brResp(:,:,:), nodeResp3(:,:,:), inj2(:,:)
  type(tSignalSlot) :: slots(1)
  real(dp) :: a, wd, peak, nyquistHz
  integer(4) :: k, nCmp

  call setVerbosity(VERB_QUIET)

  call test_init("Step and Norton waveforms")
  step = newStepSignal(3.0_dp)
  tt = [-1.0d-6, 0.0_dp, 1.0d-6]
  call test_ok("step is zero before t = 0 and imax after", &
               all(abs(step%waveform(tt) - [0.0_dp, 3.0_dp, 3.0_dp]) < 1.0d-15), "step")
  norton = newBranchNortonSignal(1.0_dp, 10.0_dp, 0.0_dp, 1.0d-9)
  call test_ok("L = 0: V0/R exp(-t/RC)", &
               all(abs(norton%waveform([5.0d-9])-exp(-0.5_dp)/10.0_dp) < 1.0d-15), "first order")
  norton = newBranchNortonSignal(1.0_dp, 2.0_dp, 1.0d-6, 1.0d-9)
  a = 1.0d6; wd = sqrt(1.0d15 - a * a)
  call test_ok("underdamped RLC: V0 e^{-at} sin(wd t)/(L wd)", &
               all(abs(norton%waveform([3.0d-8]) - exp(-a*3.0d-8)*sin(wd*3.0d-8)/(1.0d-6*wd)) < 1.0d-9), "underdamped")
  norton = newBranchNortonSignal(1.0_dp, 200.0_dp, 1.0d-6, 1.0d-9)
  a = 1.0d8; wd = sqrt(a*a - 1.0d15)
  call test_ok("overdamped RLC against the closed form", &
               all(abs(norton%waveform([3.0d-8]) - exp(-a*3.0d-8)*sinh(wd*3.0d-8)/(1.0d-6*wd)) < 1.0d-8), "overdamped")

  call test_init("Marx stage: charged C into R load (NLT), Norton form")
  study%title = "Marx single stage"
  call study%structure%addNode(newNode("Cap",    [0.0d0, 0.0d0, 5.0d0]))
  call study%structure%addNode(newNode("Node_1", [20.0d0, 0.0d0, -0.5d0]))
  call study%structure%addNode(newNode("Node_2", [25.0d0, 0.0d0, -0.5d0]))
  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
  elem = newElementLine("Rod", "Node_1", "Node_2", 0.01d0, 2, "copper")
  call study%structure%addElement(elem)
  elem = newElementBranch("Marx", "Cap", "", rBr, lBr, cBr)
  call study%structure%addElement(elem)
  elem = newElementBranch("Load", "Cap", "", rLoad, 0.0d0, 0.0d0)
  call study%structure%addElement(elem)

  norton = newBranchNortonSignal(v0, rBr, lBr, cBr)
  nyquistHz = 1.0d8
  opts%transform = "nlt"
  call transientResponse(study, norton, "Cap", ["Cap"], nyquistHz, nSamples, 0.0_dp, t, injected, resp, options=opts)

  a = 0.5_dp * (rBr + rLoad) / lBr
  wd = sqrt(a * a - 1.0_dp / (lBr * cBr))   ! overdamped: zeta = 1.16
  nCmp = 200
  allocate(ref(nCmp))
  do k = 1, nCmp
    ref(k) = v0 * rLoad * exp(-a * t(k)) * sinh(wd * t(k)) / (lBr * wd)
  end do
  peak = maxval(abs(ref))
  ! The 100 MHz band limit (5 ns samples against a ~20 ns front) rings
  ! within the first few samples; beyond them the match is much tighter
  call test_ok("load voltage within 3% of peak of the analytic RLC discharge over the first 200 samples", &
               maxval(abs(resp(1, 1:nCmp) - ref)) < 3.0d-2 * peak, "marx")
  call test_ok("and within 0.5% of peak from sample 20 on", &
               maxval(abs(resp(1, 20:nCmp) - ref(20:nCmp))) < 5.0d-3 * peak, "marx tail")
  call test_ok("peak load voltage is positive and below V0", peak > 0.0_dp .and. peak < v0, "peak")

  call test_init("Branch current output (Y(s)·(u_a - u_b))")
  ! Load branch: i = u/R_load. Marx branch: i_N - i_load = i_N - u/R_load
  ! (Norton node KCL), i.e. the capacitor discharge current V0·h_{R_total}(t)
  allocate(slots(1)%sig, source=norton)
  call transientResponseSignals(study, slots, ["Cap"], ["Cap"], nyquistHz, nSamples, 0.0_dp, t, inj2, nodeResp3, &
                                options=opts, observeBranchIds=["Load", "Marx"], branchResponses=brResp)
  call test_ok("branch responses shaped (branches, samples, 1)", all(shape(brResp) == [2, nSamples, 1]), "shape")
  call test_ok("load branch current = u/R_load", &
               maxval(abs(brResp(1, :, 1) - nodeResp3(1, :, 1) / rLoad)) < 1.0d-6 * peak / rLoad, "load")
  ! Norton form: the source splits as i_N = Y·u (this branch) + u/R_load, and the
  ! physical series discharge current is i_N - Y·u = u/R_load
  call test_ok("KCL at the node: Marx-branch + load-branch currents = the Norton current", &
               maxval(abs(brResp(1, :, 1) + brResp(2, :, 1) - inj2(1, :))) < 1.0d-4 * maxval(abs(inj2)), "kcl")
  call test_ok("load-branch current matches the analytic discharge current from sample 20 on", &
               maxval(abs(brResp(1, 20:nCmp, 1) - ref(20:nCmp) / rLoad)) < 5.0d-3 * peak / rLoad, "i load")

  call test_summary()
end program test_marx
