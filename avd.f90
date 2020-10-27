! Created by  on 28/02/2020.

! Compile with default real set to 8 bytes (-r8 on ifort)

module AVD

    integer, parameter, private :: sin_f   = 1
    integer, parameter, private :: cos_f   = 2
    integer, parameter, private :: tan_f   = 3
    integer, parameter, private :: sinh_f  = 4
    integer, parameter, private :: cosh_f  = 5
    integer, parameter, private :: tanh_f  = 6
    integer, parameter, private :: asin_f  = 7
    integer, parameter, private :: acos_f  = 8
    integer, parameter, private :: atan_f  = 9
    integer, parameter, private :: exp_f   = 10
    integer, parameter, private :: log_f   = 11
    integer, parameter, private :: log10_f = 12
    integer, parameter, private :: sqrt_f  = 13

    abstract interface
        pure function gen(x) result(r)
            real(8) :: r
            real(8), intent(in) :: x
        end function gen
    end interface

    type f_map_t
        procedure(gen), nopass, pointer :: f_ptr => null()
    end type f_map_t
    type(f_map_t), target :: f_map(0:4,13)

    type :: avd_b
        real(8) :: v
        contains
    end type avd_b

    type, extends(avd_b)  :: avd_d1
        real(8) :: d1 = 1
        contains
        procedure            :: set_from_avd1
        generic, public      :: assignment(=) => set_from_avd1
        procedure            :: avd1_equals_avd1
        generic, public      :: operator(==) => avd1_equals_avd1
        procedure, public    :: as_string => as_string_d1
    end type avd_d1

    type, extends(avd_d1) :: avd_d2
        real(8) :: d2 = 0
        contains
        procedure            :: set_from_avd2
        generic, public      :: assignment(=) => set_from_avd2
        procedure            :: avd2_equals_avd2
        generic, public      :: operator(==) => avd2_equals_avd2
        procedure, public    :: as_string => as_string_d2
        procedure, public    :: print => print_avd_d2
    end type avd_d2

    type, extends(avd_d2) :: avd_d3
        real(8) :: d3 = 0
        contains
        procedure            :: avd3_equals_avd3
        generic, public      :: operator(==) => avd3_equals_avd3
        procedure, public    :: as_string => as_string_d3
    end type avd_d3

    type, extends(avd_d3) :: avd_d4
        real(8) :: d4 = 0
        contains
        procedure            :: avd4_equals_avd4
        generic, public      :: operator(==) => avd4_equals_avd4
        procedure, public    :: as_string => as_string_d4
    end type avd_d4

    interface assignment(=)
        module procedure :: set_d1_from_d2, set_d2_from_d3, set_d3_from_d4, set_v
    end interface
    
    interface operator(**)
        module procedure :: d0_pow_i, d0_pow_r
        module procedure :: d1_pow_i, d1_pow_r
        module procedure :: d2_pow_i, d2_pow_r
        module procedure :: d3_pow_i, d3_pow_r
        module procedure :: d4_pow_i, d4_pow_r
    end interface

    interface operator(+)
        module procedure :: d0_plus_i, d0_plus_r, i_plus_d0, r_plus_d0, d0_plus_d0
        module procedure :: d1_plus_i, d1_plus_r, i_plus_d1, r_plus_d1, d1_plus_d1
        module procedure :: d2_plus_i, d2_plus_r, i_plus_d2, r_plus_d2, d2_plus_d2
        module procedure :: d3_plus_i, d3_plus_r, i_plus_d3, r_plus_d3, d3_plus_d3
        module procedure :: d4_plus_i, d4_plus_r, i_plus_d4, r_plus_d4, d4_plus_d4
    end interface

    interface operator(-)
        module procedure :: minus_d0,   minus_d1,   minus_d2 ,   minus_d3,   minus_d4
        module procedure :: d0_minus_i, d0_minus_r, i_minus_d0, r_minus_d0, d0_minus_d0
        module procedure :: d1_minus_i, d1_minus_r, i_minus_d1, r_minus_d1, d1_minus_d1
        module procedure :: d2_minus_i, d2_minus_r, i_minus_d2, r_minus_d2, d2_minus_d2
        module procedure :: d3_minus_i, d3_minus_r, i_minus_d3, r_minus_d3, d3_minus_d3
        module procedure :: d4_minus_i, d4_minus_r, i_minus_d4, r_minus_d4, d4_minus_d4
    end interface

    interface operator(*)
        module procedure :: d0_times_i, d0_times_r, i_times_d0, r_times_d0, d0_times_d0
        module procedure :: d1_times_i, d1_times_r, i_times_d1, r_times_d1, d1_times_d1
        module procedure :: d2_times_i, d2_times_r, i_times_d2, r_times_d2, d2_times_d2
        module procedure :: d3_times_i, d3_times_r, i_times_d3, r_times_d3, d3_times_d3
        module procedure :: d4_times_i, d4_times_r, i_times_d4, r_times_d4, d4_times_d4
    end interface

    interface operator( / )
        module function d0_over_i(obj, a) result(r)
            type(avd_b) :: r
            type(avd_b), intent(in) :: obj
            integer, intent(in)     :: a
        end function d0_over_i

        module function d0_over_r(obj, a) result(r)
            type(avd_b) :: r
            type(avd_b), intent(in) :: obj
            real(8), intent(in)     :: a
        end function d0_over_r

        module function i_over_d0(a, obj) result(r)
            type(avd_b) :: r
            type(avd_b), intent(in) :: obj
            integer, intent(in)     :: a
        end function i_over_d0

        module function r_over_d0(a, obj) result(r)
            type(avd_b) :: r
            type(avd_b), intent(in) :: obj
            real(8), intent(in)     :: a
        end function r_over_d0

        module function d1_over_i(obj, a) result(r)
            type(avd_d1) :: r
            type(avd_d1), intent(in) :: obj
            integer, intent(in)     :: a
        end function d1_over_i

        module function d1_over_r(obj, a) result(r)
            type(avd_d1) :: r
            type(avd_d1), intent(in) :: obj
            real(8), intent(in)     :: a
        end function d1_over_r
            
        module function i_over_d1(a, obj) result(r)
            type(avd_d1) :: r
            type(avd_d1), intent(in) :: obj
            integer, intent(in)     :: a
        end function i_over_d1

        module function r_over_d1(a, obj) result(r)
            type(avd_d1) :: r
            type(avd_d1), intent(in) :: obj
            real(8), intent(in)     :: a
        end function r_over_d1

        module function d2_over_i(obj, a) result(r)
            type(avd_d2) :: r
            type(avd_d2), intent(in) :: obj
            integer, intent(in)     :: a
        end function d2_over_i

        module function d2_over_r(obj, a) result(r)
            type(avd_d2) :: r
            type(avd_d2), intent(in) :: obj
            real(8), intent(in)     :: a
        end function d2_over_r

        module function i_over_d2(a, obj) result(r)
            type(avd_d2) :: r
            type(avd_d2), intent(in) :: obj
            integer, intent(in)     :: a
        end function i_over_d2

        module function r_over_d2(a, obj) result(r)
            type(avd_d2) :: r
            type(avd_d2), intent(in) :: obj
            real(8), intent(in)     :: a
        end function r_over_d2

        module function d3_over_i(obj, a) result(r)
            type(avd_d3) :: r
            type(avd_d3), intent(in) :: obj
            integer, intent(in)     :: a
        end function d3_over_i

        module function d3_over_r(obj, a) result(r)
            type(avd_d3) :: r
            type(avd_d3), intent(in) :: obj
            real(8), intent(in)     :: a
        end function d3_over_r

        module function i_over_d3(a, obj) result(r)
            type(avd_d3) :: r
            type(avd_d3), intent(in) :: obj
            integer, intent(in)     :: a
        end function i_over_d3

        module function r_over_d3(a, obj) result(r)
            type(avd_d3) :: r
            type(avd_d3), intent(in) :: obj
            real(8), intent(in)     :: a
        end function r_over_d3

        module function d4_over_i(obj, a) result(r)
            type(avd_d4) :: r
            type(avd_d4), intent(in) :: obj
            integer, intent(in)     :: a
        end function d4_over_i

        module function d4_over_r(obj, a) result(r)
            type(avd_d4) :: r
            type(avd_d4), intent(in) :: obj
            real(8), intent(in)     :: a
        end function d4_over_r

        module function i_over_d4(a, obj) result(r)
            type(avd_d4) :: r
            type(avd_d4), intent(in) :: obj
            integer, intent(in)     :: a
        end function i_over_d4

        module function r_over_d4(a, obj) result(r)
            type(avd_d4) :: r
            type(avd_d4), intent(in) :: obj
            real(8), intent(in)     :: a
        end function r_over_d4

        module function d4_over_d4(a, b) result(r)
            type(avd_d4) :: r
            type(avd_d4), intent(in) :: a, b
        end function d4_over_d4

    end interface

    interface sin
        module procedure :: sin_d0, sin_d1, sin_d2, sin_d3, sin_d4
    end interface

    interface cos
        module procedure :: cos_d0, cos_d1, cos_d2, cos_d3, cos_d4
    end interface

    interface sinh
        module procedure :: sinh_d0, sinh_d1, sinh_d2, sinh_d3, sinh_d4
    end interface

    interface cosh
        module procedure :: cosh_d0, cosh_d1, cosh_d2, cosh_d3, cosh_d4
    end interface

    interface tanh
        module procedure :: tanh_d0, tanh_d1, tanh_d2, tanh_d3, tanh_d4
    end interface

    interface tan
        module procedure :: tan_d0, tan_d1, tan_d2, tan_d3, tan_d4
    end interface

    interface sqrt
        module procedure :: sqrt_d0, sqrt_d1, sqrt_d2, sqrt_d3, sqrt_d4
    end interface

    interface exp
        module procedure :: exp_d0, exp_d1, exp_d2, exp_d3, exp_d4
    end interface

    interface log
        module procedure :: log_d0, log_d1, log_d2, log_d3, log_d4
    end interface

    interface log10
        module procedure :: log10_d0, log10_d1, log10_d2, log10_d3, log10_d4
    end interface


contains

    subroutine init

        f_map(0,sin_f)%f_ptr => d0_sin
        f_map(1,sin_f)%f_ptr => d1_sin
        f_map(2,sin_f)%f_ptr => d2_sin
        f_map(3,sin_f)%f_ptr => d3_sin
        f_map(4,sin_f)%f_ptr => d4_sin

        f_map(0,cos_f)%f_ptr => d0_cos
        f_map(1,cos_f)%f_ptr => d1_cos
        f_map(2,cos_f)%f_ptr => d2_cos
        f_map(3,cos_f)%f_ptr => d3_cos
        f_map(4,cos_f)%f_ptr => d4_cos

        f_map(0,tan_f)%f_ptr => d0_tan
        f_map(1,tan_f)%f_ptr => d1_tan
        f_map(2,tan_f)%f_ptr => d2_tan
        f_map(3,tan_f)%f_ptr => d3_tan
        f_map(4,tan_f)%f_ptr => d4_tan

        f_map(0,sinh_f)%f_ptr => d0_sinh
        f_map(1,sinh_f)%f_ptr => d1_sinh
        f_map(2,sinh_f)%f_ptr => d2_sinh
        f_map(3,sinh_f)%f_ptr => d3_sinh
        f_map(4,sinh_f)%f_ptr => d4_sinh

        f_map(0,cosh_f)%f_ptr => d0_cosh
        f_map(1,cosh_f)%f_ptr => d1_cosh
        f_map(2,cosh_f)%f_ptr => d2_cosh
        f_map(3,cosh_f)%f_ptr => d3_cosh
        f_map(4,cosh_f)%f_ptr => d4_cosh

        f_map(0,tanh_f)%f_ptr => d0_tanh
        f_map(1,tanh_f)%f_ptr => d1_tanh
        f_map(2,tanh_f)%f_ptr => d2_tanh
        f_map(3,tanh_f)%f_ptr => d3_tanh
        f_map(4,tanh_f)%f_ptr => d4_tanh

        f_map(0,sqrt_f)%f_ptr => d0_sqrt
        f_map(1,sqrt_f)%f_ptr => d1_sqrt
        f_map(2,sqrt_f)%f_ptr => d2_sqrt
        f_map(3,sqrt_f)%f_ptr => d3_sqrt
        f_map(4,sqrt_f)%f_ptr => d4_sqrt

        f_map(0,exp_f)%f_ptr => d0_exp
        f_map(1,exp_f)%f_ptr => d1_exp
        f_map(2,exp_f)%f_ptr => d2_exp
        f_map(3,exp_f)%f_ptr => d3_exp
        f_map(4,exp_f)%f_ptr => d4_exp

        f_map(0,log_f)%f_ptr => d0_log
        f_map(1,log_f)%f_ptr => d1_log
        f_map(2,log_f)%f_ptr => d2_log
        f_map(3,log_f)%f_ptr => d3_log
        f_map(4,log_f)%f_ptr => d4_log

        f_map(0,log10_f)%f_ptr => d0_log10
        f_map(1,log10_f)%f_ptr => d1_log10
        f_map(2,log10_f)%f_ptr => d2_log10
        f_map(3,log10_f)%f_ptr => d3_log10
        f_map(4,log10_f)%f_ptr => d4_log10

    end subroutine init

    pure function d0_times_d0(a, b) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: a, b
        r%v = a%v * b%v
    end function d0_times_d0

    pure function d1_times_d1(a, b) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: a, b
        r%avd_b = a%avd_b * b%avd_b
        associate(u=>a%v, du=>a%d1, v=>b%v, dv=>b%d1)
            r%d1 = u*dv + v*du
        end associate
    end function d1_times_d1

    pure function d2_times_d2(a, b) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: a, b
        r%avd_d1 = a%avd_d1 * b%avd_d1
        associate(u=>a%v, du=>a%d1, d2u=>a%d2, v=>b%v, dv=>b%d1, d2v=>b%d2)
            r%d2 = u*d2v + 2*du*dv + d2u*v
        end associate
    end function d2_times_d2

    pure function d3_times_d3(a, b) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: a, b
        r%avd_d2 = a%avd_d2 * b%avd_d2
        associate(u=>a%v, du=>a%d1, d2u=>a%d2, d3u=>a%d3, v=>b%v, dv=>b%d1, d2v=>b%d2, d3v=>b%d3)
            r%d3 = u*d3v + 3*du*d2v + 3*d2u*dv + d3u*v
        end associate
    end function d3_times_d3

    pure function d4_times_d4(a, b) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: a, b
        r%avd_d3 = a%avd_d3 * b%avd_d3
        associate(u=>a%v, du=>a%d1, d2u=>a%d2, d3u=>a%d3, d4u=>a%d4, v=>b%v, dv=>b%d1, d2v=>b%d2, d3v=>b%d3, d4v=>b%d4)
            r%d4 = u*d4v + 4*du*d3v + 6*d2u*d2v + 4*d3u*dv + d4u*v
        end associate
    end function d4_times_d4
    
    pure function d0_sin(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sin(x)
    end function d0_sin

    pure function d1_sin(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = cos(x)
    end function d1_sin

    pure function d2_sin(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -sin(x)
    end function d2_sin

    pure function d3_sin(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -cos(x)
    end function d3_sin

    pure function d4_sin(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = d0_sin(x)
    end function d4_sin
    
    pure function d0_cos(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = cos(x)
    end function d0_cos

    pure function d1_cos(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -sin(x)
    end function d1_cos

    pure function d2_cos(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -cos(x)
    end function d2_cos

    pure function d3_cos(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sin(x)
    end function d3_cos

    pure function d4_cos(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = d0_cos(x)
    end function d4_cos

    pure function d0_sinh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sinh(x)
    end function d0_sinh

    pure function d1_sinh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = cosh(x)
    end function d1_sinh

    pure function d2_sinh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sinh(x)
    end function d2_sinh

    pure function d3_sinh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = cosh(x)
    end function d3_sinh

    pure function d4_sinh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sinh(x)
    end function d4_sinh

    pure function d0_cosh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = cosh(x)
    end function d0_cosh

    pure function d1_cosh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sinh(x)
    end function d1_cosh

    pure function d2_cosh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = cosh(x)
    end function d2_cosh

    pure function d3_cosh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sinh(x)
    end function d3_cosh

    pure function d4_cosh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = cosh(x)
    end function d4_cosh

    pure function d0_tanh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = tanh(x)
    end function d0_tanh

    pure function d1_tanh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = 1 - tanh(x)**2
    end function d1_tanh

    pure function d2_tanh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -2*d0_tanh(x)*d1_tanh(x)
    end function d2_tanh

    pure function d3_tanh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -2*(d1_tanh(x)**2 + d0_tanh(x)*d2_tanh(x))
    end function d3_tanh

    pure function d4_tanh(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -2*(3*d1_tanh(x)*d2_tanh(x) + d0_tanh(x)*d3_tanh(x))
    end function d4_tanh

    pure function d0_sqrt(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = sqrt(x)
    end function d0_sqrt

    pure function d1_sqrt(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = 0.5*x**(-0.5)
    end function d1_sqrt

    pure function d2_sqrt(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8), parameter :: c = (0.5)*(-0.5)
        r = c*x**(-1.5)
    end function d2_sqrt

    pure function d3_sqrt(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8), parameter :: c = (0.5)*(-0.5)*(-1.5)
        r = c*x**(-2.5)
    end function d3_sqrt

    pure function d4_sqrt(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8), parameter :: c = (0.5)*(-0.5)*(-1.5)*(-2.5)
        r = c*x**(-3.5)
    end function d4_sqrt

    pure function d0_tan(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = tan(x)
    end function d0_tan

    pure function d1_tan(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8) :: y
        y = tan(x)
        r = 1 + y**2
    end function d1_tan

    pure function d2_tan(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8) :: y
        y = tan(x)
        r = 2*y*d1_tan(x)
    end function d2_tan

    pure function d3_tan(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8) :: y
        y = tan(x)
        r = 2*d1_tan(x)**2 + 2*y*d2_tan(x)
    end function d3_tan

    pure function d4_tan(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8) :: y
        y = tan(x)
        r = 4*d1_tan(x)*d2_tan(x) + 2*(d1_tan(x)*d2_tan(x)+y*d3_tan(x))
    end function d4_tan

    pure function d0_exp(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = exp(x)
    end function d0_exp

    pure function d1_exp(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = exp(x)
    end function d1_exp

    pure function d2_exp(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = exp(x)
    end function d2_exp

    pure function d3_exp(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = exp(x)
    end function d3_exp

    pure function d4_exp(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = exp(x)
    end function d4_exp

    pure function d0_log(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = log(x)
    end function d0_log

    pure function d1_log(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = 1/x
    end function d1_log

    pure function d2_log(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -1/x**2
    end function d2_log

    pure function d3_log(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = 2/x**3
    end function d3_log

    pure function d4_log(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = -6/x**4
    end function d4_log

    pure function d0_log10(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        r = log10(x)
    end function d0_log10

    pure function d1_log10(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8), parameter :: b = 1/log(10.0)
        r = b/x
    end function d1_log10

    pure function d2_log10(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8), parameter :: b = 1/log(10.0)
        r = -b/x**2
    end function d2_log10

    pure function d3_log10(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8), parameter :: b = 1/log(10.0)
        r = 2*b/x**3
    end function d3_log10

    pure function d4_log10(x) result(r)
        real(8) :: r
        real(8), intent(in) :: x
        real(8), parameter :: b = 1/log(10.0)
        r = -6*b/x**4
    end function d4_log10

    pure function Q0_f(ft, av) result(r)
        type(avd_b) :: r
        integer, intent(in)     :: ft
        type(avd_b), intent(in) :: av
        r%v = f_map(0,ft)%f_ptr(av%v)
    end function Q0_f

    pure function Q1_f(ft, av) result(r)
        type(avd_d1) :: r
        integer, intent(in)      :: ft
        type(avd_d1), intent(in) :: av

        procedure(gen), pointer :: f1
        r%avd_b = Q0_f(ft, avd_b(av%v))
        f1 => f_map(1,ft)%f_ptr
        associate(g1=>av%d1, x=>av%v)
            r%d1 = f1(x)*av%d1
        end associate
    end function Q1_f

    pure function Q2_f(ft, av) result(r)
        type(avd_d2) :: r
        integer, intent(in)      :: ft
        type(avd_d2), intent(in) :: av

        procedure(gen), pointer :: f1, f2
        r%avd_d1 = Q1_f(ft, avd_d1(av%v, av%d1))
        f1 => f_map(1,ft)%f_ptr
        f2 => f_map(2,ft)%f_ptr
        associate(g1=>av%d1, g2=>av%d2, x=>av%v)
            r%d2 = f2(x)*g1**2 + f1(x)*g2
        end associate
    end function Q2_f

    pure function Q3_f(ft, av) result(r)
        type(avd_d3) :: r
        integer, intent(in)      :: ft
        type(avd_d3), intent(in) :: av

        procedure(gen), pointer :: f1, f2, f3
        r%avd_d2 = Q2_f(ft, avd_d2(av%v, av%d1, av%d2))
        f1 => f_map(1,ft)%f_ptr
        f2 => f_map(2,ft)%f_ptr
        f3 => f_map(3,ft)%f_ptr
        associate(g1=>av%d1, g2=>av%d2, g3=>av%d3, x=>av%v)
            r%d3 = f3(x)*g1**3 + 3*g1*g2*f2(x) + f1(x)*g3
        end associate
    end function Q3_f

    pure function Q4_f(ft, av) result(r)
        type(avd_d4) :: r
        integer, intent(in)      :: ft
        type(avd_d4), intent(in) :: av

        procedure(gen), pointer :: f1, f2, f3, f4
        r%avd_d3 = Q3_f(ft, avd_d3(av%v, av%d1, av%d2, av%d3))
        f1 => f_map(1,ft)%f_ptr
        f2 => f_map(2,ft)%f_ptr
        f3 => f_map(3,ft)%f_ptr
        f4 => f_map(4,ft)%f_ptr
        associate(g1=>av%d1, g2=>av%d2, g3=>av%d3, g4=>av%d4, x=>av%v)
            r%d4 = g4*f1(x) + 4*g3*g1*f2(x) + 3*g2**2*f2(x) + 6*g1**2*g2*f3(x) + g1**4*f4(x)
        end associate
    end function Q4_f

    pure function sin_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = sin(obj%v)
    end function sin_d0

    pure function sin_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1) :: r
        r = Q1_f(sin_f, obj)
    end function sin_d1

    pure function sin_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(sin_f,obj)
    end function sin_d2

    pure function sin_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(sin_f,obj)
    end function sin_d3

    pure function sin_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(sin_f,obj)
    end function sin_d4

    pure function cos_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = cos(obj%v)
    end function cos_d0

    pure function cos_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1)             ::r
        r = Q1_f(cos_f,obj)
    end function cos_d1

    pure function cos_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(cos_f,obj)
    end function cos_d2

    pure function cos_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(cos_f,obj)
    end function cos_d3

    pure function cos_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(cos_f,obj)
    end function cos_d4

    pure function tan_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r = tan(obj%v)
    end function tan_d0

    pure function tan_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1)             ::r
        r = Q1_f(tan_f,obj)
    end function tan_d1

    pure function tan_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(tan_f,obj)
    end function tan_d2

    pure function tan_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(tan_f,obj)
    end function tan_d3

    pure function tan_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(tan_f,obj)
    end function tan_d4

    pure function sqrt_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = sqrt(obj%v)
    end function sqrt_d0

    pure function sqrt_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1)             ::r
        r = Q1_f(sqrt_f,obj)
    end function sqrt_d1

    pure function sqrt_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(sqrt_f,obj)
    end function sqrt_d2

    pure function sqrt_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(sqrt_f,obj)
    end function sqrt_d3

    pure function sqrt_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(sqrt_f,obj)
    end function sqrt_d4

    pure function sinh_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = sinh(obj%v)
    end function sinh_d0

    pure function sinh_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1) :: r
        r = Q1_f(sinh_f, obj)
    end function sinh_d1

    pure function sinh_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(sinh_f,obj)
    end function sinh_d2

    pure function sinh_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(sinh_f,obj)
    end function sinh_d3

    pure function sinh_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(sinh_f,obj)
    end function sinh_d4
    
    pure function cosh_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = cosh(obj%v)
    end function cosh_d0

    pure function cosh_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1) :: r
        r = Q1_f(cosh_f, obj)
    end function cosh_d1

    pure function cosh_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(cosh_f,obj)
    end function cosh_d2

    pure function cosh_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(cosh_f,obj)
    end function cosh_d3

    pure function cosh_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(cosh_f,obj)
    end function cosh_d4

    pure function tanh_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = tanh(obj%v)
    end function tanh_d0

    pure function tanh_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1) :: r
        r = Q1_f(tanh_f, obj)
    end function tanh_d1

    pure function tanh_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(tanh_f,obj)
    end function tanh_d2

    pure function tanh_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(tanh_f,obj)
    end function tanh_d3

    pure function tanh_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(tanh_f,obj)
    end function tanh_d4

    pure function exp_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = exp(obj%v)
    end function exp_d0

    pure function exp_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1)             ::r
        r = Q1_f(exp_f,obj)
    end function exp_d1

    pure function exp_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(exp_f,obj)
    end function exp_d2

    pure function exp_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(exp_f,obj)
    end function exp_d3

    pure function exp_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(exp_f,obj)
    end function exp_d4

    pure function log_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = log(obj%v)
    end function log_d0

    pure function log_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1)             ::r
        r = Q1_f(log_f,obj)
    end function log_d1

    pure function log_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(log_f,obj)
    end function log_d2

    pure function log_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(log_f,obj)
    end function log_d3

    pure function log_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(log_f,obj)
    end function log_d4

    pure function log10_d0(obj) result(r)
        type(avd_b), intent(in) :: obj
        type(avd_b)             ::r
        r%v = log(obj%v)
    end function log10_d0

    pure function log10_d1(obj) result(r)
        type(avd_d1), intent(in) :: obj
        type(avd_d1)             ::r
        r = Q1_f(log10_f,obj)
    end function log10_d1

    pure function log10_d2(obj) result(r)
        type(avd_d2), intent(in) :: obj
        type(avd_d2)             ::r
        r = Q2_f(log10_f,obj)
    end function log10_d2

    pure function log10_d3(obj) result(r)
        type(avd_d3), intent(in) :: obj
        type(avd_d3)             ::r
        r = Q3_f(log10_f,obj)
    end function log10_d3

    pure function log10_d4(obj) result(r)
        type(avd_d4), intent(in) :: obj
        type(avd_d4)             ::r
        r = Q4_f(log10_f,obj)
    end function log10_d4

    pure function avd1_equals_avd1(this, a) result(r)
        logical :: r
        class(avd_d1), intent(in) :: this
        type(avd_d1), intent(in) :: a
        r =  (abs(this%v - a%v) < 1.0e-6 .and. abs(this%d1 - a%d1) < 1.0e-6)
    end function avd1_equals_avd1

    pure function avd2_equals_avd2(this, a) result(r)
        logical :: r
        class(avd_d2), intent(in) :: this
        type(avd_d2), intent(in) :: a
        r =  (this%avd_d1 == a%avd_d1) .and. abs(this%d2 - a%d2) < 1.0e-6
    end function avd2_equals_avd2

    pure function avd3_equals_avd3(this, a) result(r)
        logical :: r
        class(avd_d3), intent(in) :: this
        type(avd_d3), intent(in) :: a
        r =  (this%avd_d2 == a%avd_d2) .and. abs(this%d3 - a%d3) < 1.0e-6
    end function avd3_equals_avd3

    pure function avd4_equals_avd4(this, a) result(r)
        logical :: r
        class(avd_d4), intent(in) :: this
        type(avd_d4), intent(in) :: a
        r =  (this%avd_d3 == a%avd_d3) .and. abs(this%d4 - a%d4) < 1.0e-6
    end function avd4_equals_avd4

    subroutine print_avd_d2(this, str)
        class(avd_d2), intent(in) :: this
        character(len=*), intent(in)  :: str
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(f0.4,a,f0.4)') this%v,':',this%d1
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.4)') ',',this%d2
        write(*,'(a)') str//trim(buff)
    end subroutine print_avd_d2

    pure function as_string_d1(this) result(r)
        class(avd_d1), intent(in) :: this
        character(len=:), allocatable :: r
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(2(a,f0.6))') '[',this%v,':',this%d1
        c => buff(len_trim(buff):)
        write(c,fmt='(a)') ']'
        r = trim(buff)
    end function as_string_d1

    pure function as_string_d2(this) result(r)
        class(avd_d2), intent(in) :: this
        character(len=:), allocatable :: r
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(2(a,f0.6))') '[',this%v,':',this%d1
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.6,a)') ',',this%d2,']'
        r = trim(buff)
    end function as_string_d2

    pure function as_string_d3(this) result(r)
        class(avd_d3), intent(in) :: this
        character(len=:), allocatable :: r
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(3(a,f0.6))') '[',this%v,':',this%d1,',',this%d2
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.6,a)') ',',this%d3,']'
        r = trim(buff)
    end function as_string_d3

    pure function as_string_d4(this) result(r)
        class(avd_d4), intent(in) :: this
        character(len=:), allocatable :: r
        character(len=200), target :: buff
        character(len=:), pointer  :: c
        c => buff(1:)
        write(c,fmt='(4(a,f0.6))') '[',this%v,':',this%d1,',',this%d2,',',this%d3
        c => buff(len_trim(buff):)
        write(c,fmt='(a,f0.6,a)') ',',this%d4,']'
        r = trim(buff)
    end function as_string_d4
    
    ! Times - d0  =============================================================

    pure function i_times_d0(this, a) result(r)
        type(avd_b), intent(in) :: this
        integer, intent(in)       :: a
        type(avd_b) :: r
        r%v = this%v * a
    end function i_times_d0

    pure function r_times_d0(this, a) result(r)
        type(avd_b), intent(in) :: this
        real(8), intent(in)       :: a
        type(avd_b) :: r
        r%v = this%v * a
    end function r_times_d0

    pure function d0_times_i(a, obj) result(r)
        type(avd_b), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_b) :: r
        r = obj*a
    end function d0_times_i

    pure function d0_times_r(a, obj) result(r)
        type(avd_b), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_b) :: r
        r = obj*a
    end function d0_times_r

    ! Times - d1  =============================================================
    
    pure function i_times_d1(this, a) result(r)
        type(avd_d1), intent(in) :: this
        integer, intent(in)       :: a
        type(avd_d1) :: r
        r%v = this%v * a
        r%d1 = this%d1 * a
    end function i_times_d1

    pure function r_times_d1(this, a) result(r)
        type(avd_d1), intent(in) :: this
        real(8), intent(in)       :: a
        type(avd_d1) :: r
        r%v = this%v * a
        r%d1 = this%d1 * a
    end function r_times_d1

    pure function d1_times_i(a, obj) result(r)
        type(avd_d1), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d1) :: r
        r = obj*a
    end function d1_times_i

    pure function d1_times_r(a, obj) result(r)
        type(avd_d1), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d1) :: r
        r = obj*a
    end function d1_times_r

    ! Times - d2  =============================================================

    pure function i_times_d2(this, a) result(r)
        type(avd_d2), intent(in) :: this
        integer, intent(in)       :: a
        type(avd_d2) :: r
        r%avd_d1 = this%avd_d1 * a
        r%d2 = this%d2 * a
    end function i_times_d2

    pure function r_times_d2(this, a) result(r)
        type(avd_d2), intent(in) :: this
        real(8), intent(in)       :: a
        type(avd_d2) :: r
        r%avd_d1 = this%avd_d1 * a
        r%d2 = this%d2 * a
    end function r_times_d2

    pure function d2_times_i(a, obj) result(r)
        type(avd_d2), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d2) :: r
        r = obj*a
    end function d2_times_i

    pure function d2_times_r(a, obj) result(r)
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d2) :: r
        r = obj*a
    end function d2_times_r

    ! Times - d3  =============================================================

    pure function i_times_d3(this, a) result(r)
        type(avd_d3), intent(in) :: this
        integer, intent(in)       :: a
        type(avd_d3) :: r
        r%avd_d2 = this%avd_d2 * a
        r%d3 = this%d3 * a
    end function i_times_d3

    pure function r_times_d3(this, a) result(r)
        type(avd_d3), intent(in) :: this
        real(8), intent(in)       :: a
        type(avd_d3) :: r
        r%avd_d2 = this%avd_d2 * a
        r%d3 = this%d3 * a
    end function r_times_d3

    pure function d3_times_i(a, obj) result(r)
        type(avd_d3), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d3) :: r
        r = obj*a
    end function d3_times_i

    pure function d3_times_r(a, obj) result(r)
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d3) :: r
        r = obj*a
    end function d3_times_r

    ! Times - d4  =============================================================

    pure function i_times_d4(this, a) result(r)
        type(avd_d4), intent(in) :: this
        integer, intent(in)       :: a
        type(avd_d4) :: r
        r%avd_d3 = this%avd_d3 * a
        r%d4 = this%d4 * a
    end function i_times_d4

    pure function r_times_d4(this, a) result(r)
        type(avd_d4), intent(in) :: this
        real(8), intent(in)       :: a
        type(avd_d4) :: r
        r%avd_d3 = this%avd_d3 * a
        r%d4 = this%d4 * a
    end function r_times_d4

    pure function d4_times_i(a, obj) result(r)
        type(avd_d4), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d4) :: r
        r = obj*a
    end function d4_times_i

    pure function d4_times_r(a, obj) result(r)
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d4) :: r
        r = obj*a
    end function d4_times_r

    ! minus  d0  =================================================================

    pure function d0_minus_d0(a, b) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: a, b
        r%v = a%v - b%v
    end function d0_minus_d0

    pure function minus_d0(this) result(r)
        type(avd_b), intent(in) :: this
        type(avd_b) :: r
        r%v = -this%v
    end function minus_d0

    pure function i_minus_d0(a, obj) result(r)
        type(avd_b), intent(in) :: obj
        integer, intent(in)     :: a
        type(avd_b) :: r
        r%v = a - obj%v
    end function i_minus_d0

    pure function r_minus_d0(a, obj) result(r)
        type(avd_b), intent(in) :: obj
        real(8), intent(in)     :: a
        type(avd_b) :: r
        r%v = a - obj%v
    end function r_minus_d0

    pure function d0_minus_i(obj, a) result(r)
        type(avd_b), intent(in) :: obj
        integer, intent(in)     :: a
        type(avd_b) :: r
        r = -(a - obj)
    end function d0_minus_i

    pure function d0_minus_r(obj, a) result(r)
        type(avd_b), intent(in) :: obj
        real(8), intent(in)     :: a
        type(avd_b) :: r
        r = -(a - obj)
    end function d0_minus_r
    
    ! minus  d1  =================================================================

    pure function d1_minus_d1(a, b) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: a, b
        r%avd_b = a%avd_b - b%avd_b
        r%d1 = a%d1 - b%d1
    end function d1_minus_d1

    pure function minus_d1(this) result(r)
        type(avd_d1), intent(in) :: this
        type(avd_d1) :: r
        r%v = -this%v
        r%d1 = -this%d1
    end function minus_d1

    pure function i_minus_d1(this, a) result(r)
        type(avd_d1), intent(in) :: this
        integer, intent(in)       :: a
        type(avd_d1) :: r
        r%v = this%v - a
        r%d1 = this%d1
    end function i_minus_d1

    pure function r_minus_d1(this, a) result(r)
        type(avd_d1), intent(in) :: this
        real(8), intent(in)       :: a
        type(avd_d1) :: r
        r%v = this%v - a
        r%d1 = this%d1
    end function r_minus_d1

    pure function d1_minus_i(a, obj) result(r)
        type(avd_d1), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d1) :: r
        r%v = a - obj%v
        r%d1 = -obj%d1
    end function d1_minus_i

    pure function d1_minus_r(a, obj) result(r)
        type(avd_d1), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d1) :: r
        r%v = a - obj%v
        r%d1 = -obj%d1
    end function d1_minus_r

    ! minus  d2  =================================================================

    pure function d2_minus_d2(a, b) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: a, b
        r%avd_d1 = a%avd_d1 - b%avd_d1
        r%d2 = a%d2 - b%d2
    end function d2_minus_d2

    pure function minus_d2(this) result(r)
        type(avd_d2), intent(in) :: this
        type(avd_d2) :: r
        r%avd_d1 = -this%avd_d1
        r%d2 = -this%d2
    end function minus_d2

    pure function i_minus_d2(a, obj) result(r)
        type(avd_d2), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d2) :: r
        r%avd_d1 = a - obj%avd_d1
        r%d2 = -obj%d2
    end function i_minus_d2

    pure function r_minus_d2(a, obj) result(r)
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)      :: a
        type(avd_d2) :: r
        r%avd_d1 = a - obj%avd_d1
        r%d2 = -obj%d2
    end function r_minus_d2

    pure function d2_minus_i(obj, a) result(r)
        type(avd_d2), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d2) :: r
        r = -(a - obj)
    end function d2_minus_i

    pure function d2_minus_r(obj, a) result(r)
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d2) :: r
        r = -(a - obj)
    end function d2_minus_r

    ! minus  d3  =================================================================

    pure function d3_minus_d3(a, b) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: a, b
        r%avd_d2 = a%avd_d2 - b%avd_d2
        r%d3 = a%d3 - b%d3
    end function d3_minus_d3

    pure function minus_d3(this) result(r)
        type(avd_d3), intent(in) :: this
        type(avd_d3) :: r
        r%avd_d2 = -this%avd_d2
        r%d3 = -this%d3
    end function minus_d3

    pure function i_minus_d3(a, obj) result(r)
        type(avd_d3), intent(in) :: obj
        integer, intent(in)      :: a
        type(avd_d3) :: r
        r%avd_d2 = a - obj%avd_d2
        r%d3 = -obj%d3
    end function i_minus_d3

    pure function r_minus_d3(a, obj) result(r)
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d3) :: r
        r%avd_d2 = a - obj%avd_d2
        r%d3 = -obj%d3
    end function r_minus_d3

    pure function d3_minus_i(obj, a) result(r)
        type(avd_d3), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d3) :: r
        r = -(a - obj)
    end function d3_minus_i

    pure function d3_minus_r(obj, a) result(r)
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d3) :: r
        r = -(a - obj)
    end function d3_minus_r

    ! minus  d4  =================================================================

    pure function d4_minus_d4(a, b) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: a, b
        r%avd_d3 = a%avd_d3 - b%avd_d3
        r%d4 = a%d4 - b%d4
    end function d4_minus_d4

    pure function minus_d4(this) result(r)
        type(avd_d4), intent(in) :: this
        type(avd_d4) :: r
        r%avd_d3 = -this%avd_d3
        r%d4 = -this%d4
    end function minus_d4

    pure function i_minus_d4(a, obj) result(r)
        type(avd_d4), intent(in) :: obj
        integer, intent(in)      :: a
        type(avd_d4) :: r
        r%avd_d3 = a - obj%avd_d3
        r%d4 = -obj%d4
    end function i_minus_d4

    pure function r_minus_d4(a, obj) result(r)
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)      :: a
        type(avd_d4) :: r
        r%avd_d3 = a - obj%avd_d3
        r%d4 = -obj%d4
    end function r_minus_d4

    pure function d4_minus_i(obj, a) result(r)
        type(avd_d4), intent(in) :: obj
        integer, intent(in)       :: a
        type(avd_d4) :: r
        r = -(a - obj)
    end function d4_minus_i

    pure function d4_minus_r(obj, a) result(r)
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)       :: a
        type(avd_d4) :: r
        r = -(a - obj)
    end function d4_minus_r

    ! set  =================================================================
    pure subroutine set_v(this, v)
        class(avd_b), intent(inout) :: this
        real(8), intent(in)         :: v
        this%v = v
    end subroutine set_v

    pure subroutine set_d1_from_d2(this, v)
        type(avd_d1), intent(inout) :: this
        type(avd_d2), intent(in)    :: v
        this%v = v%v
        this%d1 = v%d1
    end subroutine set_d1_from_d2

    pure subroutine set_d2_from_d3(this, v)
        type(avd_d2), intent(inout) :: this
        type(avd_d3), intent(in)    :: v
        this%avd_d1 = avd_d1(v%v,v%d1)
        this%d2 = v%d2
    end subroutine set_d2_from_d3

    pure subroutine set_d3_from_d4(this, v)
        type(avd_d3), intent(inout) :: this
        type(avd_d4), intent(in)    :: v
        this%avd_d2 = avd_d2(v%v,v%d1,v%d2)
        this%d3 = v%d3
    end subroutine set_d3_from_d4

    pure subroutine set_from_avd1(this, v)
        class(avd_d1), intent(inout) :: this
        type(avd_d1), intent(in)    :: v
        this%v = v%v
        this%d1 = v%d1
    end subroutine set_from_avd1

    pure subroutine set_from_avd2(this, v)
        class(avd_d2), intent(inout) :: this
        type(avd_d2), intent(in)    :: v
        this%avd_d1 = avd_d1(v%v,v%d1)
        this%d2 = v%d2
    end subroutine set_from_avd2

    pure subroutine set_from_avd3(this, v)
        class(avd_d3), intent(inout) :: this
        type(avd_d3), intent(in)    :: v
        this%avd_d2 = avd_d2(v%v,v%d1,v%d2)
        this%d3 = v%d3
    end subroutine set_from_avd3

    pure subroutine set_from_avd4(this, v)
        class(avd_d4), intent(inout) :: this
        type(avd_d4), intent(in)    :: v
        this%avd_d3 = avd_d3(v%v,v%d1,v%d2,v%d3)
        this%d4 = v%d4
    end subroutine set_from_avd4

    ! Power =================================================================
    pure function d0_pow_i(obj, n) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        integer, intent(in)     :: n
        r%v = obj%v**n
    end function d0_pow_i

    pure function d0_pow_r(obj, x) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        real(8), intent(in)     :: x
        r%v = obj%v**x
    end function d0_pow_r
    
    pure function d1_pow_i(obj, n) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        integer, intent(in)     :: n
        r = obj**(real(n,8))
    end function d1_pow_i

    pure function d1_pow_r(obj, x) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        real(8), intent(in)     :: x
        real(8) :: f1
        r%avd_b = obj%avd_b**x
        f1 = x*obj%v**(x-1)
        associate(g1=>obj%d1)
            r%d1 = f1*g1
        end associate
    end function d1_pow_r

    pure function d2_pow_i(obj, n) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        integer, intent(in)     :: n
        r = obj**(real(n,8))
    end function d2_pow_i

    pure function d2_pow_r(obj, x) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)     :: x
        real(8) :: f1, f2
        r%avd_d1 = obj%avd_d1**x
        f1 = x*obj%v**(x-1)
        f2 = x*(x-1)*obj%v**(x-2)
        associate(g1=>obj%d1, g2=>obj%d2)
            r%d2 = f2*g1**2 + f1*g2
        end associate
    end function d2_pow_r

    pure function d3_pow_i(obj, n) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        integer, intent(in)      :: n
        r = obj**(real(n,8))
    end function d3_pow_i

    pure function d3_pow_r(obj, x) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)     :: x
        real(8) :: f1, f2, f3
        r%avd_d2 = obj%avd_d2**x
        f1 = x*obj%v**(x-1)
        f2 = x*(x-1)*obj%v**(x-2)
        f3 = x*(x-1)*(x-2)*obj%v**(x-3)
        associate(g1=>obj%d1, g2=>obj%d2, g3=>obj%d3)
            r%d3 = f3*g1**3 + 3*g1*g2*f2 + f1*g3
        end associate
    end function d3_pow_r
    
    pure function d4_pow_i(obj, n) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        integer, intent(in)     :: n
        r = obj**(real(n,8))
   end function d4_pow_i

    pure function d4_pow_r(obj, x) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)     :: x
        real(8) :: f1, f2, f3, f4
        r%avd_d3 = obj%avd_d3**x
        f1 = x*obj%v**(x-1)
        f2 = x*(x-1)*obj%v**(x-2)
        f3 = x*(x-1)*(x-2)*obj%v**(x-3)
        f4 = x*(x-1)*(x-2)*(x-3)*obj%v**(x-4)
        associate(g1=>obj%d1, g2=>obj%d2, g3=>obj%d3, g4=>obj%d4)
            r%d4 = g4*f1 + 4*g3*g1*f2 + 3*g2**2*f2 + 6*g1**2*g2*f3 + g1**4*f4
        end associate
    end function d4_pow_r

    ! Plus  =================================================================

    pure function d0_plus_d0(a, b) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: a, b
        r%v = a%v + b%v
    end function d0_plus_d0

    pure function d0_plus_i(obj, n) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        integer, intent(in)     :: n
        r%v = obj%v + n
    end function d0_plus_i

    pure function d0_plus_r(obj, x) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        real(8), intent(in)     :: x
        r%v = obj%v + x
    end function d0_plus_r

    pure function i_plus_d0(n, obj) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        integer, intent(in)     :: n
        r%v = obj%v + n
    end function i_plus_d0

    pure function r_plus_d0(x, obj) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        real(8), intent(in)     :: x
        r%v = obj%v + x
    end function r_plus_d0

    pure function d1_plus_d1(a, b) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: a, b
        r%avd_b = a%avd_b + b%avd_b
        r%d1 = a%d1 + b%d1
    end function d1_plus_d1

    pure function d1_plus_i(obj, n) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_b = obj%avd_b + n
        r%d1 = obj%d1
    end function d1_plus_i

    pure function d1_plus_r(obj, x) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_b = obj%avd_b + x
        r%d1 = obj%d1
    end function d1_plus_r

    pure function i_plus_d1(n, obj) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_b = obj%avd_b + n
        r%d1 = obj%d1
    end function i_plus_d1

    pure function r_plus_d1(x, obj) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_b = obj%avd_b + x
        r%d1 = obj%d1
    end function r_plus_d1

    pure function d2_plus_d2(a, b) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: a, b
        r%avd_d1 = a%avd_d1 + b%avd_d1
        r%d2 = a%d2 + b%d2
    end function d2_plus_d2

    pure function d2_plus_i(obj, n) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_d1 = obj%avd_d1 + n
        r%d2 = obj%d2
    end function d2_plus_i

    pure function d2_plus_r(obj, x) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_d1 = obj%avd_d1 + x
        r%d2 = obj%d2
    end function d2_plus_r

    pure function i_plus_d2(n, obj) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_d1 = obj%avd_d1 + n
        r%d2 = obj%d2
    end function i_plus_d2

    pure function r_plus_d2(x, obj) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_d1 = obj%avd_d1 + x
        r%d2 = obj%d2
    end function r_plus_d2

    pure function d3_plus_d3(a, b) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: a, b
        r%avd_d2 = a%avd_d2 + b%avd_d2
        r%d3 = a%d3 + b%d3
    end function d3_plus_d3

    pure function d3_plus_i(obj, n) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_d2 = obj%avd_d2 + n
        r%d3 = obj%d3
    end function d3_plus_i

    pure function d3_plus_r(obj, x) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_d2 = obj%avd_d2 + x
        r%d3 = obj%d3
    end function d3_plus_r

    pure function i_plus_d3(n, obj) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_d2 = obj%avd_d2 + n
        r%d3 = obj%d3
    end function i_plus_d3

    pure function r_plus_d3(x, obj) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_d2 = obj%avd_d2 + x
        r%d3 = obj%d3
    end function r_plus_d3

    pure function d4_plus_d4(a, b) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: a, b
        r%avd_d3 = a%avd_d3 + b%avd_d3
        r%d4 = a%d4 + b%d4
    end function d4_plus_d4

    pure function d4_plus_i(obj, n) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_d3 = obj%avd_d3 + n
        r%d4 = obj%d4
    end function d4_plus_i

    pure function d4_plus_r(obj, x) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_d3 = obj%avd_d3 + x
        r%d4 = obj%d4
    end function d4_plus_r

    pure function i_plus_d4(n, obj) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        integer, intent(in)     :: n
        r%avd_d3 = obj%avd_d3 + n
        r%d4 = obj%d4
    end function i_plus_d4

    pure function r_plus_d4(x, obj) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)     :: x
        r%avd_d3 = obj%avd_d3 + x
        r%d4 = obj%d4
    end function r_plus_d4

end module AVD