!  solMalha.f90 
!
!  Rotina principal de solução de malhas
!  Retorna as matrizes facZ, pivot, S1 e S2 para uso externo na obtenção das tensões e correntes

subroutine solMalha (zLong, zTrans, topA, topB, topDmenosC, topDmaisC, nSeg, nNos, facZ, pivot, S1, S2, Yg, invZlA, invZtB)
  implicit none

  ! Variables
  integer nSeg, nNos
  integer pivot(nNos)
  double complex topA(nSeg, nNos), topB(nSeg, nNos), topDmaisC(nNos, nSeg), topDmenosC(nNos, nSeg)
  double complex zLong(nSeg, nSeg), zTrans(nSeg, nSeg), facZ(nSeg, nSeg), S1(nNos, nSeg), S2(nNos, nSeg)
  double complex Yg(nNos, nSeg), invZlA(nSeg, nNos), invZtB(nSeg, nNos)

  ! Body of solMalha
  !invZlA = topA .xi. zLong
  !invZtB = topB .xi. zTrans
  !call ZSCAL(0.5, invZtB)
  !Yg = topDmaisC .x. invZlA
  !call ZGEMM('N', 'N', 1.0, topDmenosC, invZtB, -1.0, Yg)
  !call DLFTCG(Yg, facZ, pivot)
  !S1 = .t.(Yg .xi. -(invZlA + invZtB))
  !S2 = .t.(Yg .xi. (invZtB - invZlA))
end subroutine solMalha

