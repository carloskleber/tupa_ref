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

  function alocaMalha(nn, ns) result(mesh)
    !! Alocação de memória
    integer(4), value :: nn, ns
    type(tMesh), pointer :: mesh
    
    allocate(mesh)
    mesh%nno = nn
    mesh%nseg = ns
    allocate(mesh%tensao(nn))
    allocate(mesh%corrente1(ns))
    allocate(mesh%corrente2(ns))
    !allocate(mesh%S1(ns, nn))
    !allocate(mesh%S2(ns, nn))
    allocate(mesh%A(ns, nn))
    allocate(mesh%B(ns, nn))
    allocate(mesh%C(nn, ns))
    allocate(mesh%D(nn, ns))
    allocate(mesh%Ztrans(ns, ns))
    allocate(mesh%Zlong(ns, ns))
    !allocate(mesh%Yg(nn, nn))
    allocate(mesh%Zeq(nn+2*ns, nn+2*ns))
    mesh%A(:,:) = cmplx(0.,0.,kind=8)
    mesh%B(:,:) = cmplx(0.,0.,kind=8)
    mesh%C(:,:) = cmplx(0.,0.,kind=8)
    mesh%D(:,:) = cmplx(0.,0.,kind=8)
  end function
  

  subroutine calcTopologia(mesh, nn, ns, n1, n2)
    !! Gera as matrizes de topologia A, B, C, D a partir dos vetores de conexões
    integer(4), value :: nn, ns
    integer(4), intent(in) :: n1(ns), n2(ns)
    type(tMesh), pointer :: mesh
    integer(4) i1
    
    do i1 = 1,ns
      mesh%A(i1, n1(i1)+1) = cmplx(-1.,0.,kind=8)
      mesh%A(i1, n2(i1)+1) = cmplx(1.,0.,kind=8)
      mesh%B(i1, n1(i1)+1) = cmplx(-0.5,0.,kind=8)
      mesh%B(i1, n2(i1)+1) = cmplx(-0.5,0.,kind=8)
      mesh%C(n1(i1)+1, i1) = cmplx(1.,0.,kind=8)
      mesh%D(n2(i1)+1, i1) = cmplx(1.,0.,kind=8) ! Na verdade esta fazendo -D, ja para economizar conta
    enddo
  end subroutine
  

  subroutine calcBase(mesh)
    !! Cálculo das matrizes de base
    !! Obtem os dados da estrutura (ou a estrutura cria a matriz de geometria)
    type(tMesh), pointer :: mesh
    
  end subroutine
  
  subroutine calcParam(mesh, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)
    !! Cálculo dos parâmetros básicos para uma frequência (parâmetros do ar e solo)
    real(8), intent(in), value :: omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo
    type(tMesh), pointer :: mesh

    mesh%cteEletAr = 1. / (quatropi * cmplx(sigmaAr, omega * epsAr,kind=8))
    mesh%cteEletSolo = 1. / (quatropi * cmplx(sigmaSolo, omega * epsSolo,kind=8))
    mesh%cteMagAr = cmplx(0., omega * muAr / quatropi, kind=8)
    mesh%cteMagSolo = cmplx(0., omega * muSolo / quatropi, kind=8)
    mesh%propAr = sqrt(cmplx(muAr * epsAr * omega * omega, -muAr * sigmaAr * omega,kind=8));
    mesh%propSolo = sqrt(cmplx(muSolo * epsSolo * omega * omega, -muSolo * sigmaSolo * omega,kind=8));
  end subroutine

  !> Seta um elemento das matrizes Zlong e Ztrans
  subroutine setZ(mesh, i, j, zl, zt)
    integer(4), intent(in), value :: i, j
    type(tMesh), pointer :: mesh
    complex(8), intent(in), value :: zl, zt

    mesh%Zlong(i, j) = zl
    mesh%Ztrans(i, j) = zt

    if (i .ne. j) then
      mesh%Zlong(j, i) = zl
      mesh%Ztrans(j, i) = zt
    endif
  end subroutine

  subroutine calcZPropria(mesh, i, pos, h, zint, fgl, fgli, fgt, fgti)
    !! Calcula a impedância transversal e longitudinal própria de um segmento cilíndrico
    !! @param[in] pos Posição relativa do segmento (1 = ar, 2 = solo, 0 = fronteira)
    !! @param[in] h Dobro da altura do segmento (distância para imagem)
    !! @param[in] zint Impedância interna do segmento
    integer(4), intent(in) :: i, pos
    real(8), intent(in) :: h, fgl, fgli, fgt, fgti
    complex(8), intent(in) :: zint
    type(tMesh), pointer :: mesh
    complex(8) fpropi

    if (pos .eq. 1) then
      fpropi = exp(cmplx(0., h, kind=8) * mesh%propAr); ! convencao exp(-j k R)
      mesh%zTrans(i+1, i+1) = mesh%cteEletAr * (cmplx(fgt,0.,kind=8) - fpropi * cmplx(fgti,0.,kind=8))
      mesh%zLong(i+1, i+1) = mesh%cteMagAr * (cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8)) + zint
    else
      fpropi = exp(cmplx(0., h, kind=8) * mesh%propSolo);
      mesh%zTrans(i+1, i+1) = mesh%cteEletSolo * (cmplx(fgt,0.,kind=8) + fpropi * cmplx(fgti,0.,kind=8))
      mesh%zLong(i+1, i+1) = mesh%cteMagSolo * (cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8)) + zint
    endif
  end subroutine

  subroutine calcZMutua(mesh, i, j, pos1, pos2, d, di, fgl, fgli, fgt, fgti)
    !! Calcula a impedância transversal e longitudinal entre dois segmentos
    !!
    !! @param[in] pos1 Posicao relativa do primeiro segmento (1 = ar, 2 = solo)
    !! @param[in] pos2 Posicao relativa do segundo segmento
    !! @param[in] d Distancia direta entre primeiro e segundo segmento
    !! @param[in] di Distancia entre o primeiro segmento e imagem do segundo segmento
    integer(4), intent(in), value :: i, j, pos1, pos2
    real(8), intent(in), value :: d, di, fgl, fgli, fgt, fgti
    type(tMesh), pointer :: mesh
    complex(8) fprop, fpropi, zt, zl

    if (pos1 .eq. 1 .and. pos2 .eq. 1) then
      fprop = exp(cmplx(0., d, kind=8) * mesh%propAr);
      fpropi = exp(cmplx(0., di, kind=8) * mesh%propAr);
      zt = mesh%cteEletAr * (fprop * cmplx(fgt,0.,kind=8) - fpropi * cmplx(fgti,0.,kind=8))
      zl = mesh%cteMagAr * (fprop * cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8))
    else if (pos1 .eq. 2 .and. pos2 .eq. 2) then
      fprop = exp(cmplx(0., d, kind=8) * mesh%propSolo);
      fpropi = exp(cmplx(0., di, kind=8) * mesh%propSolo);
      zt = mesh%cteEletSolo * (fprop * cmplx(fgt,0.,kind=8) + fpropi * cmplx(fgti,0.,kind=8))
      zl = mesh%cteMagSolo * (fprop * cmplx(fgl,0.,kind=8) + fpropi * cmplx(fgli,0.,kind=8))
    else
      zt = cmplx(0.,0.,kind=8)
      zl = cmplx(0.,0.,kind=8)
    endif
    mesh%zTrans(i+1,j+1) = zt
    mesh%zTrans(j+1,i+1) = zt
    mesh%zLong(i+1,j+1) = zl
    mesh%zLong(j+1,i+1) = zl
  end subroutine

  integer(4) function calcFreqF(mesh)
    !! Cálculo na frequencia
    !! @returns INFO das rotinas ZGESV
    type(tMesh), pointer :: mesh
    integer INFO
    integer, allocatable :: IPIV(:)

    allocate(IPIV(mesh%nseg))
    call zgesv(mesh%nseg, mesh%nno, mesh%Zlong, mesh%nseg, IPIV, mesh%A, mesh%nseg, INFO) ! Zlong torna-se invZlA
    if (INFO .ne. 0) then
      calcFreqF = INFO
      return
    endif
    call zgesv(mesh%nseg, mesh%nno, mesh%Ztrans, mesh%nseg, IPIV, mesh%B, mesh%nseg, INFO) ! Ztrans torna-se invZtB
    if (INFO .ne. 0) then
      calcFreqF = INFO
      return
    endif

    error stop ! testar na rotina calcFreq2
    !mesh%Ztrans = mesh%Ztrans * 0.5
    !mesh%Yg = matmul(mesh%D-mesh%C, mesh%Ztrans) - matmul(mesh%D+mesh%C, mesh%Zlong) ! TODO otimizar D+C, D-C
    !mesh%S1 = -(mesh%Ztrans + mesh%Zlong)
    !mesh%S2 = mesh%Zlong - mesh%Ztrans
    calcFreqF = 0
  end function calcFreqF
  
  subroutine calcFreq2(mesh)
    !! Cálculo na frequência - versão cheia
    !!
    !! Basicamente faz a montagem de Zeq
    type(tMesh), pointer :: mesh
    integer INFO, nn, ns, n
    integer, allocatable :: IPIV(:)

    nn = mesh%nno
    ns = mesh%nseg
    n = nn + 2 * ns
    mesh%Zeq(1:ns,          1:nn          ) = mesh%A
    mesh%Zeq(1:ns,          (nn+1):(nn+ns)) = mesh%Zlong * 0.5
    mesh%Zeq(1:ns,          (nn+ns+1):n   ) = mesh%Zlong * (-0.5)
    mesh%Zeq((ns+1):(2*ns), 1:nn          ) = mesh%B
    mesh%Zeq((ns+1):(2*ns), (nn+1):(nn+ns)) = mesh%Ztrans
    mesh%Zeq((ns+1):(2*ns), (nn+ns+1):n   ) = mesh%Ztrans
    mesh%Zeq((2*ns+1):n,    1:nn          ) = cmplx(0.,0.,kind=8)
    mesh%Zeq((2*ns+1):n,    (nn+1):(nn+ns)) = mesh%C
    mesh%Zeq((2*ns+1):n,    (nn+ns+1):n   ) = mesh%D
  end subroutine calcFreq2

  integer(4) function injetaSinalF(mesh, nsig, pos, sig)
    !! Injeta o sinal
    integer(4), intent(in), value :: nsig
    integer(4), intent(in) :: pos(nsig)
    complex(8), intent(in) :: sig(nsig)
    complex(8), allocatable :: y(:)
    type(tMesh), pointer :: mesh
    integer INFO, nn, ns, n
    integer, allocatable :: IPIV(:)
    
    nn = mesh%nno
    ns = mesh%nseg
    n = 2*ns + nn
    allocate(IPIV(n))
    allocate(y(n))
    y(:) = cmplx(0.,0.,kind=8)
    y(2*ns+pos) = sig
    call zgesv(n, 1, mesh%Zeq, n, IPIV, y, n, INFO)
    if (INFO .ne. 0) then
      injetaSinalF = INFO
      return
    endif
    mesh%tensao    = y(1:nn)
    mesh%corrente1 = y((nn+1):(nn+ns))
    mesh%corrente2 = y((nn+ns+1):n)
    injetaSinalF = 0
  end function injetaSinalF

  subroutine getSaidas(mesh, nn, ns, v, i1, i2)
    !! Retorna o ponteiro das saídas
    integer(4), intent(in), value :: nn, ns
    complex(8), intent(out) :: v(nn), i1(ns), i2(ns)
    type(tMesh), pointer :: mesh

    v = mesh%tensao
    i1 = mesh%corrente1
    i2 = mesh%corrente2
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
