module mMesh
  !! Mesh configuration: topology and impedances matrix
  implicit none

  ! Explicit interface for LAPACK ZGESV (double complex linear solve)
  interface
    subroutine zgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer, intent(in) :: n
      integer, intent(in) :: nrhs
      integer, intent(in) :: lda
      integer, intent(in) :: ldb
      integer, intent(out) :: ipiv(*)
      integer, intent(out) :: info
      complex(8), intent(inout) :: a(lda, *)
      complex(8), intent(inout) :: b(ldb, *)
    end subroutine zgesv
  end interface
  real(8), parameter :: pi = acos(-1.)
  real(8), parameter :: quatropi =  4. * acos(-1.)
  
  type :: tMesh
    complex(8), allocatable :: tensao(:)
    complex(8), allocatable :: corrente1(:)
    complex(8), allocatable :: corrente2(:)
    complex(8), allocatable :: A(:,:)
    complex(8), allocatable :: B(:,:)
    complex(8), allocatable :: C(:,:)
    complex(8), allocatable :: D(:,:)
    complex(8), allocatable :: Ztrans(:,:)
    complex(8), allocatable :: Zlong(:,:)
    complex(8), allocatable :: Zeq(:,:)
    !< Parametros do meio para armazenar a cada loop de frequencia
    complex(8) cteEletAr, cteEletSolo, cteMagAr, cteMagSolo, propAr, propSolo
    !< Dimensoes do problema
    integer(4) :: nno, nseg
  end type
contains

  function alocaMalha(nn, ns) result(malha)
    !! Alocação de memória
    integer(4), value :: nn, ns
    type(tMesh), pointer :: malha
    
    allocate(malha)
    malha%nno = nn
    malha%nseg = ns
    allocate(malha%tensao(nn))
    allocate(malha%corrente1(ns))
    allocate(malha%corrente2(ns))
    !allocate(malha%S1(ns, nn))
    !allocate(malha%S2(ns, nn))
    allocate(malha%A(ns, nn))
    allocate(malha%B(ns, nn))
    allocate(malha%C(nn, ns))
    allocate(malha%D(nn, ns))
    allocate(malha%Ztrans(ns, ns))
    allocate(malha%Zlong(ns, ns))
    !allocate(malha%Yg(nn, nn))
    allocate(malha%Zeq(nn+2*ns, nn+2*ns))
    malha%A(:,:) = cmplx(0.,0.,kind=8)
    malha%B(:,:) = cmplx(0.,0.,kind=8)
    malha%C(:,:) = cmplx(0.,0.,kind=8)
    malha%D(:,:) = cmplx(0.,0.,kind=8)
  end function
  

  subroutine calcTopologia(malha, nn, ns, n1, n2)
    !! Gera as matrizes de topologia A, B, C, D a partir dos vetores de conexões
    integer(4), value :: nn, ns
    integer(4), intent(in) :: n1(ns), n2(ns)
    type(tMesh), pointer :: malha
    integer(4) i1
    
    do i1 = 1,ns
      malha%A(i1, n1(i1)+1) = cmplx(-1.,0.,kind=8)
      malha%A(i1, n2(i1)+1) = cmplx(1.,0.,kind=8)
      malha%B(i1, n1(i1)+1) = cmplx(-0.5,0.,kind=8)
      malha%B(i1, n2(i1)+1) = cmplx(-0.5,0.,kind=8)
      malha%C(n1(i1)+1, i1) = cmplx(1.,0.,kind=8)
      malha%D(n2(i1)+1, i1) = cmplx(1.,0.,kind=8) ! Na verdade esta fazendo -D, ja para economizar conta
    enddo
  end subroutine
  

  subroutine calcBase(malha)
    !! Cálculo das matrizes de base
    !! Obtem os dados da estrutura (ou a estrutura cria a matriz de geometria)
    type(tMesh), pointer :: malha
    
  end subroutine
  
  subroutine calcParam(malha, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)
    !! Cálculo dos parâmetros básicos para uma frequência (parâmetros do ar e solo)
    real(8), intent(in), value :: omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo
    type(tMesh), pointer :: malha

    malha%cteEletAr = 1. / (quatropi * cmplx(sigmaAr, omega * epsAr,kind=8))
    malha%cteEletSolo = 1. / (quatropi * cmplx(sigmaSolo, omega * epsSolo,kind=8))
    malha%cteMagAr = cmplx(0., omega * muAr / quatropi, kind=8)
    malha%cteMagSolo = cmplx(0., omega * muSolo / quatropi, kind=8)
    malha%propAr = sqrt(cmplx(muAr * epsAr * omega * omega, -muAr * sigmaAr * omega,kind=8));
    malha%propSolo = sqrt(cmplx(muSolo * epsSolo * omega * omega, -muSolo * sigmaSolo * omega,kind=8));
  end subroutine

  !> Seta um elemento das matrizes Zlong e Ztrans
  subroutine setZ(malha, i, j, zl, zt)
    integer(4), intent(in), value :: i, j
    type(tMesh), pointer :: malha
    complex(8), intent(in), value :: zl, zt

    malha%Zlong(i, j) = zl
    malha%Ztrans(i, j) = zt

    if (i .ne. j) then
      malha%Zlong(j, i) = zl
      malha%Ztrans(j, i) = zt
    endif
  end subroutine

  subroutine calcZPropria(malha, i, pos, h, zint, fgl, fgli, fgt, fgti)
    !! Calcula a impedância transversal e longitudinal própria de um segmento cilíndrico
    !! @param[in] pos Posição relativa do segmento (1 = ar, 2 = solo, 0 = fronteira)
    !! @param[in] h Dobro da altura do segmento (distância para imagem)
    !! @param[in] zint Impedância interna do segmento
    integer(4), intent(in) :: i, pos
    real(8), intent(in) :: h, fgl, fgli, fgt, fgti
    complex(8), intent(in) :: zint
    type(tMesh), pointer :: malha
    complex(8) fpropi

    if (pos .eq. 1) then
      fpropi = exp(cmplx(0., h, kind=8) * malha%propAr); ! convencao exp(-j k R)
      malha%zTrans(i+1, i+1) = malha%cteEletAr * (cmplx(fgt,0.,kind=8) - fpropi * cmplx(fgti,0.,kind=8))
      malha%zLong(i+1, i+1) = malha%cteMagAr * (cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8)) + zint
    else
      fpropi = exp(cmplx(0., h, kind=8) * malha%propSolo);
      malha%zTrans(i+1, i+1) = malha%cteEletSolo * (cmplx(fgt,0.,kind=8) + fpropi * cmplx(fgti,0.,kind=8))
      malha%zLong(i+1, i+1) = malha%cteMagSolo * (cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8)) + zint
    endif
  end subroutine

  subroutine calcZMutua(malha, i, j, pos1, pos2, d, di, fgl, fgli, fgt, fgti)
    !! Calcula a impedância transversal e longitudinal entre dois segmentos
    !!
    !! @param[in] pos1 Posicao relativa do primeiro segmento (1 = ar, 2 = solo)
    !! @param[in] pos2 Posicao relativa do segundo segmento
    !! @param[in] d Distancia direta entre primeiro e segundo segmento
    !! @param[in] di Distancia entre o primeiro segmento e imagem do segundo segmento
    integer(4), intent(in), value :: i, j, pos1, pos2
    real(8), intent(in), value :: d, di, fgl, fgli, fgt, fgti
    type(tMesh), pointer :: malha
    complex(8) fprop, fpropi, zt, zl

    if (pos1 .eq. 1 .and. pos2 .eq. 1) then
      fprop = exp(cmplx(0., d, kind=8) * malha%propAr);
      fpropi = exp(cmplx(0., di, kind=8) * malha%propAr);
      zt = malha%cteEletAr * (fprop * cmplx(fgt,0.,kind=8) - fpropi * cmplx(fgti,0.,kind=8))
      zl = malha%cteMagAr * (fprop * cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8))
    else if (pos1 .eq. 2 .and. pos2 .eq. 2) then
      fprop = exp(cmplx(0., d, kind=8) * malha%propSolo);
      fpropi = exp(cmplx(0., di, kind=8) * malha%propSolo);
      zt = malha%cteEletSolo * (fprop * cmplx(fgt,0.,kind=8) + fpropi * cmplx(fgti,0.,kind=8))
      zl = malha%cteMagSolo * (fprop * cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8))
    else
      zt = cmplx(0.,0.,kind=8)
      zl = cmplx(0.,0.,kind=8)
    endif
    malha%zTrans(i+1,j+1) = zt
    malha%zTrans(j+1,i+1) = zt
    malha%zLong(i+1,j+1) = zl
    malha%zLong(j+1,i+1) = zl
  end subroutine

  integer(4) function calcFreqF(malha)
    !! Cálculo na frequencia
    !! @returns INFO das rotinas ZGESV
    type(tMesh), pointer :: malha
    integer INFO
    integer, allocatable :: IPIV(:)

    allocate(IPIV(malha%nseg))
    call zgesv(malha%nseg, malha%nno, malha%Zlong, malha%nseg, IPIV, malha%A, malha%nseg, INFO) ! Zlong torna-se invZlA
    if (INFO .ne. 0) then
      calcFreqF = INFO
      return
    endif
    call zgesv(malha%nseg, malha%nno, malha%Ztrans, malha%nseg, IPIV, malha%B, malha%nseg, INFO) ! Ztrans torna-se invZtB
    if (INFO .ne. 0) then
      calcFreqF = INFO
      return
    endif

    error stop ! testar na rotina calcFreq2
    !malha%Ztrans = malha%Ztrans * 0.5
    !malha%Yg = matmul(malha%D-malha%C, malha%Ztrans) - matmul(malha%D+malha%C, malha%Zlong) ! TODO otimizar D+C, D-C
    !malha%S1 = -(malha%Ztrans + malha%Zlong)
    !malha%S2 = malha%Zlong - malha%Ztrans
    calcFreqF = 0
  end function calcFreqF
  
  subroutine calcFreq2(malha)
    !! Cálculo na frequência - versão cheia
    !!
    !! Basicamente faz a montagem de Zeq
    type(tMesh), pointer :: malha
    integer INFO, nn, ns, n
    integer, allocatable :: IPIV(:)

    nn = malha%nno
    ns = malha%nseg
    n = nn + 2 * ns
    malha%Zeq(1:ns,          1:nn          ) = malha%A
    malha%Zeq(1:ns,          (nn+1):(nn+ns)) = malha%Zlong * 0.5
    malha%Zeq(1:ns,          (nn+ns+1):n   ) = malha%Zlong * (-0.5)
    malha%Zeq((ns+1):(2*ns), 1:nn          ) = malha%B
    malha%Zeq((ns+1):(2*ns), (nn+1):(nn+ns)) = malha%Ztrans
    malha%Zeq((ns+1):(2*ns), (nn+ns+1):n   ) = malha%Ztrans
    malha%Zeq((2*ns+1):n,    1:nn          ) = cmplx(0.,0.,kind=8)
    malha%Zeq((2*ns+1):n,    (nn+1):(nn+ns)) = malha%C
    malha%Zeq((2*ns+1):n,    (nn+ns+1):n   ) = malha%D
  end subroutine calcFreq2

  integer(4) function injetaSinalF(malha, nsig, pos, sig)
    !! Injeta o sinal
    integer(4), intent(in), value :: nsig
    integer(4), intent(in) :: pos(nsig)
    complex(8), intent(in) :: sig(nsig)
    complex(8), allocatable :: y(:)
    type(tMesh), pointer :: malha
    integer INFO, nn, ns, n
    integer, allocatable :: IPIV(:)
    
    nn = malha%nno
    ns = malha%nseg
    n = 2*ns + nn
    allocate(IPIV(n))
    allocate(y(n))
    y(:) = cmplx(0.,0.,kind=8)
    y(2*ns+pos) = sig
    call zgesv(n, 1, malha%Zeq, n, IPIV, y, n, INFO)
    if (INFO .ne. 0) then
      injetaSinalF = INFO
      return
    endif
    malha%tensao    = y(1:nn)
    malha%corrente1 = y((nn+1):(nn+ns))
    malha%corrente2 = y((nn+ns+1):n)
    injetaSinalF = 0
  end function injetaSinalF

  subroutine getSaidas(malha, nn, ns, v, i1, i2)
    !! Retorna o ponteiro das saídas
    integer(4), intent(in), value :: nn, ns
    complex(8), intent(out) :: v(nn), i1(ns), i2(ns)
    type(tMesh), pointer :: malha

    v = malha%tensao
    i1 = malha%corrente1
    i2 = malha%corrente2
  end subroutine

  subroutine printM(desc, m, n, a)
    !! Rotina auxiliar para impressão de matrizes
    character*(*) desc
    integer m,n,lda
    complex(8) a (m,n)
    integer i,j
    write (*,*)
    write (*,*) desc
    do i = 1,m
      write( * , "(1x,*(g10.3))") ((real(a(i,j))," ",j=1,n), j=1,n)
      write( * , "(1x,*(g10.3))") ((aimag(a(i,j))," ",j=1,n), j=1,n)
    enddo
  end subroutine printM
end module
