program test_impedance
  use mImpedance
  use check
  implicit none

  ! Test suite variables
  real(8) :: result, expected, tolerance
  real(8) :: za1(3), zb1(3), zva(3), zvb(3), zla, zlb
  real(8) :: x, y
  integer :: i

  ! ----------------------------------------------------------------
  ! Test 1: FUNCBARRA - Distância entre pontos paralelos
  ! ----------------------------------------------------------------
  call test_init("FUNCBARRA function tests")

  ! Pontos coincidentes na origem
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [0.0d0, 0.0d0, 0.0d0]
  zva = [0.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  ! Configurar common block para teste
  call set_params(za1, zb1, zva, zvb, zlb)

  ! x=0, y=0 -> distância 0, 1/0 = infinito - vamos testar caso não singular
  x = 1.0d0
  y = 0.0d0

  call test_ok("FUNCBARRA retorna valor positivo", &
               FUNCBARRA(x, y) > 0.0d0, &
               "Esperado valor positivo")

  call test_ok("FUNCBARRA é função decrescente com distância", &
               FUNCBARRA(1.0d0, 0.0d0) > FUNCBARRA(2.0d0, 0.0d0), &
               "Quanto maior x, maior distância, menor o valor")

  ! ----------------------------------------------------------------
  ! Test 2: LIMINF - sempre retorna 0
  ! ----------------------------------------------------------------
  call test_init("LIMINF function tests")

  call test_ok("LIMINF(0.0) = 0.0", &
               LIMINF(0.0d0) == 0.0d0, &
               "Deve retornar zero")

  call test_ok("LIMINF(5.0) = 0.0", &
               LIMINF(5.0d0) == 0.0d0, &
               "Deve retornar zero independente do argumento")

  call test_ok("LIMINF(-3.0) = 0.0", &
               LIMINF(-3.0d0) == 0.0d0, &
               "Deve retornar zero para argumentos negativos")

  ! ----------------------------------------------------------------
  ! Test 3: LIMSUP - retorna lb do common block
  ! ----------------------------------------------------------------
  call test_init("LIMSUP function tests")

  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [1.0d0, 0.0d0, 0.0d0]
  zva = [0.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 0.0d0, 0.0d0]
  zla = 2.0d0
  zlb = 3.0d0

  call set_params(za1, zb1, zva, zvb, zlb)

  call test_ok("LIMSUP retorna lb", &
               abs(LIMSUP(0.0d0) - 3.0d0) < 1.0d-15, &
               "Deve retornar 3.0")

  call test_ok("LIMSUP é constante", &
               abs(LIMSUP(1.0d0) - LIMSUP(2.0d0)) < 1.0d-15, &
               "Deve ser independente do argumento")

  ! ----------------------------------------------------------------
  ! Test 4: Integração 1D - Gauss-Kronrod
  ! ----------------------------------------------------------------
  call test_init("1D Integration tests")

  tolerance = 1.0d-10

  ! Integral de x^2 de 0 a 1
  expected = 1.0d0/3.0d0
  result = integrate_1d(quadratic, 0.0d0, 1.0d0, 0.0d0, 1.0d-12)
  call test_ok("∫₀¹ x² dx = 1/3", &
               abs(result - expected) < tolerance, &
               "Resultado: " // trim(adjustl(real_to_str(result)))

  ! Integral de sin(x) de 0 a pi
  expected = 2.0d0
  result = integrate_1d(sin_func, 0.0d0, acos(-1.0d0), 0.0d0, 1.0d-12)
  call test_ok("∫₀ᵠ sin(x) dx = 2", &
               abs(result - expected) < tolerance, &
               "Resultado: " // trim(adjustl(real_to_str(result)))

  ! ----------------------------------------------------------------
  ! Test 5: Integração 2D - TWODQ
  ! ----------------------------------------------------------------
  call test_init("2D Integration (TWODQ) tests")

  tolerance = 1.0d-6

  ! Integral de x*y sobre retângulo [0,1]x[0,1]
  expected = 0.25d0
  call test_twodq_simple(xy_func, constant_0, constant_1, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-6, &
                         result, "∫₀¹∫₀¹ x·y dy dx = 0.25")
  call test_ok("∫₀¹∫₀¹ x·y dy dx = 0.25", &
               abs(result - expected) < tolerance, &
               "Resultado: " // trim(adjustl(real_to_str(result)))

  ! Integral de x^2 + y^2 sobre retângulo [0,1]x[0,2]
  expected = 10.0d0/3.0d0  ! = 3.333...
  call test_twodq_simple(sum_squares, constant_0, constant_2, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-6, &
                         result, "∫₀¹∫₀² (x²+y²) dy dx = 10/3")
  call test_ok("∫₀¹∫₀² (x²+y²) dy dx = 10/3", &
               abs(result - expected) < tolerance, &
               "Resultado: " // trim(adjustl(real_to_str(result)))

  ! ----------------------------------------------------------------
  ! Test 6: IMPMUTUA - Casos de geometria simples
  ! ----------------------------------------------------------------
  call test_init("IMPMUTUA geometry tests")

  tolerance = 1.0d-4

  ! Caso 1: Dois segmentos paralelos unitários, separados por distância d=1
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [1.0d0, 0.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [1.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call IMPMUTUA(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("IMPMUTUA - segmentos paralelos unitários", &
               result > 0.0d0, &
               "Resultado positivo: " // trim(adjustl(real_to_str(result))))

  ! Valor de referência (pode ser calculado analiticamente)
  call test_ok("IMPMUTUA - valor razoável", &
               result > 0.1d0 .and. result < 10.0d0, &
               "Resultado deve ser finito e positivo")

  ! Caso 2: Segmentos coincidentes (singular, mas deve ser tratado)
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [0.0d0, 0.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [1.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call IMPMUTUA(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("IMPMUTUA - segmentos coincidentes (singular)", &
               result > 0.0d0, &
               "Integral singular deve convergir")

  ! Caso 3: Segmentos afastados
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [0.0d0, 0.0d0, 10.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [1.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call IMPMUTUA(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("IMPMUTUA - segmentos afastados", &
               result < 1.0d0, &
               "Valor pequeno para grande separação: " // trim(adjustl(real_to_str(result))))

  ! Caso 4: Segmentos perpendiculares
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [1.0d0, 0.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 1.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call IMPMUTUA(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("IMPMUTUA - segmentos perpendiculares", &
               result > 0.0d0 .and. result < 10.0d0, &
               "Resultado finito: " // trim(adjustl(real_to_str(result))))

  ! ----------------------------------------------------------------
  ! Test 7: Consistência e simetria
  ! ----------------------------------------------------------------
  call test_init("IMPMUTUA symmetry tests")

  tolerance = 1.0d-6

  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [2.0d0, 1.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 1.0d0, 0.0d0]
  zla = 2.0d0
  zlb = 3.0d0

  ! IMPMUTUA deve ser simétrica
  real(8) :: result1, result2
  call IMPMUTUA(za1, zva, zla, zb1, zvb, zlb, result1)
  call IMPMUTUA(zb1, zvb, zlb, za1, zva, zla, result2)

  call test_ok("IMPMUTUA é simétrica", &
               abs(result1 - result2) < tolerance, &
               "Result1: " // trim(adjustl(real_to_str(result1))) // &
               " Result2: " // trim(adjustl(real_to_str(result2))))

  ! ----------------------------------------------------------------
  ! Test 8: Testes de stress - domínios não retangulares
  ! ----------------------------------------------------------------
  call test_init("TWODQ non-rectangular domain tests")

  tolerance = 1.0d-6

  ! Integral com limite superior variável: ∫₀¹ ∫₀ˣ 1 dy dx = 0.5
  expected = 0.5d0
  call test_twodq_simple(const_1, constant_0, identity_func, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-12, &
                         result, "∫₀¹ ∫₀ˣ 1 dy dx = 0.5")
  call test_ok("∫₀¹ ∫₀ˣ 1 dy dx = 0.5", &
               abs(result - expected) < tolerance, &
               "Resultado: " // trim(adjustl(real_to_str(result))))

  ! Integral com limites variáveis: ∫₀¹ ∫₋ₓˣ x·y dy dx
  expected = 0.0d0  ! Função ímpar em y
  call test_twodq_simple(xy_func, neg_identity, identity_func, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-12, &
                         result, "∫₀¹ ∫₋ₓˣ x·y dy dx = 0")
  call test_ok("∫₀¹ ∫₋ₓˣ x·y dy dx = 0", &
               abs(result - expected) < 1.0d-8, &
               "Resultado: " // trim(adjustl(real_to_str(result))))

  ! Resumo final
  call test_summary()

contains

  ! Helper: Set common block params
  subroutine set_params(a1, b1, va, vb, lb)
    real(8), intent(in) :: a1(3), b1(3), va(3), vb(3), lb
    real(8) :: common_a1(3), common_b1(3), common_va(3), common_vb(3), common_lb
    common /params/ common_a1, common_b1, common_va, common_vb, common_lb

    common_a1 = a1
    common_b1 = b1
    common_va = va
    common_vb = vb
    common_lb = lb
  end subroutine set_params

  ! Test function for 1D integration
  real(8) function quadratic(x)
    real(8), intent(in) :: x
    quadratic = x * x
  end function quadratic

  real(8) function sin_func(x)
    real(8), intent(in) :: x
    sin_func = sin(x)
  end function sin_func

  ! Test functions for 2D integration
  real(8) function xy_func(x, y)
    real(8), intent(in) :: x, y
    xy_func = x * y
  end function xy_func

  real(8) function sum_squares(x, y)
    real(8), intent(in) :: x, y
    sum_squares = x*x + y*y
  end function sum_squares

  real(8) function const_1(x, y)
    real(8), intent(in) :: x, y
    const_1 = 1.0d0
  end function const_1

  ! Boundary functions
  real(8) function constant_0(x)
    real(8), intent(in) :: x
    constant_0 = 0.0d0
  end function constant_0

  real(8) function constant_1(x)
    real(8), intent(in) :: x
    constant_1 = 1.0d0
  end function constant_1

  real(8) function constant_2(x)
    real(8), intent(in) :: x
    constant_2 = 2.0d0
  end function constant_2

  real(8) function identity_func(x)
    real(8), intent(in) :: x
    identity_func = x
  end function identity_func

  real(8) function neg_identity(x)
    real(8), intent(in) :: x
    neg_identity = -x
  end function neg_identity

  ! Wrapper to integrate 1D functions using the module's integrator
  real(8) function integrate_1d(f, a, b, epsabs, epsrel)
    real(8), external :: f
    real(8), intent(in) :: a, b, epsabs, epsrel
    real(8) :: err

    call dqag_k15(f, a, b, epsabs, epsrel, integrate_1d, err)
  end function integrate_1d

  ! Wrapper for TWODQ testing with simple functions
  subroutine test_twodq_simple(f, g, h, a, b, epsabs, epsrel, result, description)
    interface
      real(8) function f(x, y)
        real(8), intent(in) :: x, y
      end function f
      real(8) function g(x)
        real(8), intent(in) :: x
      end function g
      real(8) function h(x)
        real(8), intent(in) :: x
      end function h
    end interface
    real(8), intent(in) :: a, b, epsabs, epsrel
    real(8), intent(out) :: result
    character(len=*), intent(in) :: description
    real(8) :: errest
    integer, parameter :: irule = 2  ! not used

    call TWODQ(f, a, b, g, h, epsabs, epsrel, irule, result, errest)
  end subroutine test_twodq_simple

  ! Convert real(8) to string for output
  function real_to_str(val) result(str)
    real(8), intent(in) :: val
    character(len=20) :: str
    write(str, '(ES20.12)') val
    str = adjustl(str)
  end function real_to_str

end program test_impedance