program test_dc
  !! Quasi-stationary (DC) point of the `frequencies` block
  !! (`"dc": true`, `dcFrequency`): axis construction in `loadStudy` and the
  !! solved DC limit of `common/rod_dc.json` (a 3 m, 1 cm rod in 10 mS/m soil).
  use mCtes, only: PI
  use tupa
  use mStudy
  use check
  implicit none

  type(tStudy) :: study
  real(8), allocatable :: freqHz(:)
  complex(8), allocatable :: srcI(:)
  character(len=256), allocatable :: srcNode(:)
  complex(8), allocatable :: zin(:)
  complex(8) :: zDc, zLow
  real(8) :: rSunde

  call test_init("frequencies.dc: axis and quasi-static solve")

  call loadStudy("../common/rod_dc.json", study, sourceNodeIds=srcNode, sourceCurrents=srcI, freqHz=freqHz)
  call test_ok("axis has DC point + 3 log points", size(freqHz) == 4, "wrong axis length")
  call test_ok("DC point is dcFrequency default 1 mHz, first", abs(freqHz(1) - 1.0d-3) < 1.0d-15, "wrong DC frequency")
  call test_ok("log axis follows", abs(freqHz(2) - 10.0d0) < 1.0d-9 .and. abs(freqHz(4) - 1000.0d0) < 1.0d-6, &
               "wrong log axis")

  call study%runSweep(freqHz, srcNode, srcI)
  zin = study%inputImpedance("Node_1")
  zDc = zin(1)
  zLow = zin(2)
  rSunde = 100.0d0 / (2.0d0 * PI * 3.0d0) * (log(4.0d0 * 3.0d0 / 0.01d0) - 1.0d0)

  call test_ok("DC impedance is real", abs(aimag(zDc)) < 1.0d-4 * real(zDc), "imaginary part not negligible")
  call test_ok("DC resistance within 8% of Sunde's rod formula", abs(real(zDc) - rSunde) < 0.08d0 * rSunde, &
               "R_dc far from the analytic rod resistance")
  call test_ok("DC equals the 10 Hz value to 0.5%", abs(zDc - zLow) < 5.0d-3 * abs(zLow), "DC and 10 Hz disagree")

  call test_summary()
end program test_dc
