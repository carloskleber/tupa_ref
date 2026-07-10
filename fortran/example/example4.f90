program example4
  !! ROADMAP.md Phase 3 milestone: frequency sweep, result storage, and the
  !! CSV/JSON writers, on the same Portela-1997-parameter buried conductor
  !! as example3 (10 m, r0 = 7 mm, 0.5 m depth, soil sigma = 0.01 S/m,
  !! epsilon_r = 10).
  !!
  !! Unlike example3 (a hand-rolled loop over `run` that only prints Zin),
  !! this drives `tStudy%runSweep` end to end: a log-spaced default
  !! frequency axis (`logFrequencyAxis`), the stored voltage/current
  !! results, the `inputImpedance`/`maxVoltageMagnitude` convenience
  !! queries, and both output writers (`mResultsWriter`), against the
  !! ADR 0012 v0 JSON schema.
  use mCtes
  use mStudy
  use mResultsWriter
  use mNode
  use mMaterial
  use mElementLine
  implicit none

  type(tStudy) :: study
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  real(8), parameter :: length = 10.0d0, r0 = 0.007d0, depth = 0.5d0
  real(8), parameter :: sigmaSoil = 0.01d0, epsrSoil = 10.0d0
  real(8), allocatable :: freqHz(:), vmax(:)
  complex(8), allocatable :: zin(:)
  integer :: k

  print *, color_green, "Starting Example 4: frequency sweep + results writers (ROADMAP Phase 3)", color_reset
  study%title = "Example 4 - Portela 1997 buried conductor, full sweep"

  call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -depth]))
  call study%structure%addNode(newNode("Node_2", [length, 0.0d0, -depth]))

  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("soil", epsrSoil, 1.0d0, sigmaSoil)

  elem = newElementLine("Line_1", "Node_1", "Node_2", r0, 10, "copper")
  call study%structure%addElement(elem)

  freqHz = logFrequencyAxis(1.0d2, 1.0d6, 9)
  call study%runSweep(freqHz, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])

  zin = study%inputImpedance("Node_1")
  vmax = study%maxVoltageMagnitude()

  print *, ""
  print '(A14,A16,A16,A16)', "f (Hz)", "|Zin| (ohm)", "phase (deg)", "max|V| (V)"
  do k = 1, size(freqHz)
    print '(F14.1,F16.4,F16.4,F16.4)', freqHz(k), abs(zin(k)), &
      atan2(aimag(zin(k)), real(zin(k))) * 180.0d0 / PI, vmax(k)
  end do

  call writeResultsCsv(study, "example4_results.csv")
  call writeResultsJson(study, "example4_results.json")

  print *, ""
  print *, "Wrote example4_results.csv and example4_results.json"
  print *, color_green, "Example 4 completed.", color_reset
end program example4
