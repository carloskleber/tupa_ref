subroutine solMalha(zLong, zTrans, topA, topB, topDmenosC, topDmaisC, nSeg, nNos, &
                    facZ, pivot, S1, S2, Yg, invZlA, invZtB)
  !! Legacy placeholder for direct mesh solution using LU factorization.
  !!
  !! **Status**: Not implemented. This routine is a historical artifact from an earlier
  !! version of the solver. Modern code uses `Mesh%calcFreq2()` and LAPACK's ZGESV
  !! to solve the augmented system Zeq·x = b instead.
  !!
  !! The commented-out code below shows the intended algorithm:
  !! 1. Compute invZlA = topA · Z_long^(-1)
  !! 2. Compute invZtB = topB · Z_trans^(-1)
  !! 3. Assemble admittance matrix Yg = (D+C) · invZlA - (D-C) · invZtB
  !! 4. Factor Yg = LU (via DLFTCG) and store pivot information
  !! 5. Compute source coupling matrices S1 and S2
  !!
  !! This direct approach has been superseded by the method in `Mesh%calcFreq2()`,
  !! which assembles the full impedance matrix Zeq and uses standard LAPACK solving.
  implicit none

  integer, intent(in) :: nSeg
  !! Number of electrode segments
  integer, intent(in) :: nNos
  !! Number of nodes
  complex(8), intent(in) :: topA(nSeg, nNos)
  !! Topology matrix A (segment-to-node incidence)
  complex(8), intent(in) :: topB(nSeg, nNos)
  !! Topology matrix B (segment-to-node incidence for transverse coupling)
  complex(8), intent(in) :: topDmenosC(nNos, nSeg)
  !! Topology matrix (D-C)
  complex(8), intent(in) :: topDmaisC(nNos, nSeg)
  !! Topology matrix (D+C)
  complex(8), intent(in) :: zLong(nSeg, nSeg)
  !! Longitudinal impedance matrix (self and mutual impedances along conductors)
  complex(8), intent(in) :: zTrans(nSeg, nSeg)
  !! Transverse impedance matrix (coupling to earth)
  complex(8), intent(out) :: facZ(nSeg, nSeg)
  !! Factored system matrix (not populated — historical output)
  integer, intent(out) :: pivot(nNos)
  !! Pivot indices from LU factorization (not populated — historical output)
  complex(8), intent(out) :: S1(nNos, nSeg)
  !! Source coupling matrix 1 (not populated — historical output)
  complex(8), intent(out) :: S2(nNos, nSeg)
  !! Source coupling matrix 2 (not populated — historical output)
  complex(8), intent(out) :: Yg(nNos, nSeg)
  !! Admittance matrix (not populated — historical output)
  complex(8), intent(out) :: invZlA(nSeg, nNos)
  !! Inverse impedance times topology (not populated — historical output)
  complex(8), intent(out) :: invZtB(nSeg, nNos)
  !! Inverse impedance times topology (not populated — historical output)

  ! Historical commented-out implementation:
  ! invZlA = topA .xi. zLong
  ! invZtB = topB .xi. zTrans
  ! call ZSCAL(0.5, invZtB)
  ! Yg = topDmaisC .x. invZlA
  ! call ZGEMM('N', 'N', 1.0, topDmenosC, invZtB, -1.0, Yg)
  ! call DLFTCG(Yg, facZ, pivot)
  ! S1 = .t.(Yg .xi. -(invZlA + invZtB))
  ! S2 = .t.(Yg .xi. (invZtB - invZlA))

end subroutine solMalha
