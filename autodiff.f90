! Auto differentiation
! ====================

! This is a powerful numerical technique which provides numerically exact vaulues of a first derivative,
! not an approximation.

! Represent a variable by a pair containing its value and first derivative's value {x, d}, and
! implementation is by extensive use of operator overloading.
!
! For example:
!    x**2 is represented by {x**2, 2*x} (a specific case of x**n)
!  sin(x) is represented by {sin(x), cos(x)}
!
! The product rule: {x dx}*{y dy} => {xy (x*dy+y*dx)}
!
! so x**2*sin(x) => {x**2 2*x}*{sin(x) cos(x)} => {x**2*sin(x) (x**2*cos(x)+2*x*sin(x))}

! The advantage is that complicated algebra calculating first derivatives is avoided without using an approximation
! - very useful when working with a Jacobian for example.
! The disadvantage is that the function value is always calculated as well.

!
! Attribution
! ===========
! Read about this somewhere at least 10  years ago and thought it looked interesting. So the idea was someone else's
! but the implementation is all mine.

!
! Example
! =======
!
! To find the first an second derivatives of 1/(1-x) at x=0.5
!
! 1) Set the auto_var x  call x%set(0.5) - the components of x are now {0.5 1}
! 2) Assign the auto_var f  f = 1/(1-x) which generates the following call sequence:
!            sub_n_x        {0.5  1} -> {0.5 -1}
!            div_r_x
!            quotient rule  {0.5 -1} -> {2 4}
! 3) So f is now set to {2 4}
!
!
! Usage
! =====
!
! To get the numerical value of the derivative of x**2 at x = 3
!
!     type(auto_var) :: x, y
!     call x%set(3)
!     y = x**2
!     write(*,'(a,f0.1)') 'Derivative of x**2 at x = 3 is ',y%get_derivative()
!
! Tested
! ======
!     gfortran -o autodiff -cpp -DUTEST autodiff.f90
!    
!    ./autodiff
!    passed :: x**2
!    passed :: x**3 + x
!    passed :: 3*x**2
!    passed :: 3*x**4 + 2*x
!    passed :: 3*x**2 + 2*x - 5
!    passed :: (x + 2)*(x + 3)*(x - 1)
!    passed :: (x + 2)*(3 - x)*(x**2 - 1)
!    passed :: (x + 2)/(x**2 - 1)
!    passed :: 1.5d0/(x-1) - 0.5d0/(x+1) [previous function as partial fractions]
!    passed :: 1/(1-x)
!    passed :: 4/x
!    passed :: e**x
!    passed :: 2**x
!    passed :: sin(x)
!    passed :: cos(x)
!    passed :: sin(1/x)
!    passed :: sqrt(x)
!    passed :: ||y||
!    passed :: sqrt(9+u(2)**2) == ||u||
!    passed :: _t_ x _x_ (1)
!    passed :: _t_ x _x_ (2)
!    passed :: _t_ x _x_ (3)
!    passed :: x**2 - 2*x*y , Fx
!    passed :: x**2 - 2*x*y , Fy
!    passed :: (x-1)*sin(z) + (y+x)*cos(z/2)**2 Fx
!    passed :: (x-1)*sin(z) + (y+x)*cos(z/2)**2 Fy
!    passed :: (x-1)*sin(z) + (y+x)*cos(z/2)**2 Fz
!    passed :: log(4 + x)
!    passed :: log(4 + x**2)
!    sub_n_x
!    div_n_x
!    quotient rule
!    passed :: 1/(1-x)


module auto_diff
  implicit none
  
  public :: auto_var
  public :: set_val, make_var, make_const, get_val, get_der, create
  public :: operator(==), operator(+), operator(-), operator(*), operator(/), operator(**), assignment(=)
  public :: operator(<), operator(>)
  public :: cos, sin, tan, sqrt, operator(.dot.), operator(.cross.)
  public :: cosh, sinh, tanh, exp, log

  
#ifdef UTEST
  public :: utest
#endif
  integer :: debug_level = 0
  
private

  type auto_var
      private
      real(8) :: x = 0
      real(8) :: d = 1
  contains
      procedure, public :: set => set_ad
      procedure, public :: set_constant => set_constant_ad
      procedure, public :: get_value => get_value_ad
      procedure, public :: get_derivative => get_derivative_ad
  end type auto_var

  interface create
     module procedure create_av
  end interface
  
  interface make_var
    module procedure make_var_s
    module procedure make_var_v
  end interface
  
  interface sqrt
     module procedure sqrt_av
  end interface

  interface norm2
     module procedure norm2_av
  end interface

  interface assignment(=)
     module procedure assign_s
     module procedure assign_v
  end interface
  
  interface operator(+)
     module procedure add_x_x_v
     module procedure add_x_x
     module procedure add_x_r
     module procedure add_r_x
     module procedure add_x_n
     module procedure add_n_x
  end interface

  interface operator(-)
     module procedure sub_x_x_v
     module procedure sub_x_x
     module procedure sub_x_r
     module procedure sub_r_x
     module procedure sub_r_x_a
     module procedure sub_x_n
     module procedure sub_n_x
  end interface

  interface operator(*)
     module procedure mult_x_x
     module procedure mult_x_r
     module procedure mult_r_x
     module procedure mult_x_n
     module procedure mult_n_x
  end interface

  interface operator(/)
     module procedure div_x_x
     module procedure div_xv_x
     module procedure div_x_r
     module procedure div_r_x
     module procedure div_x_n
     module procedure div_n_x
  end interface

  interface operator(**)
     module procedure pow_x_x
     module procedure pow_x_n
     module procedure pow_x_r
     module procedure pow_n_x
     module procedure pow_r_x
  end interface

  interface operator(==)
     module procedure equal
  end interface

  interface operator(<)
     module procedure less_than
  end interface

  interface operator(>)
     module procedure greater_than
  end interface

  interface operator(<=)
     module procedure less_equal
  end interface

  interface operator(>=)
     module procedure greater_equal
  end interface

  interface get_val
     module procedure get_val_s
     module procedure get_val_v
  end interface

  interface get_der
     module procedure get_der_s
     module procedure get_der_v
  end interface

  interface set_val
     module procedure set_val_n
     module procedure set_val_r
  end interface

  interface sin
     module procedure sin_x_r
  end interface

  interface cos
     module procedure cos_x_r
  end interface

  interface tan
     module procedure tan_x_r
  end interface

  interface sinh
     module procedure sinh_x_r
  end interface

  interface cosh
     module procedure cosh_x_r
  end interface

  interface tanh
     module procedure tanh_x_r
  end interface

  interface log
     module procedure log_x_r
  end interface

  interface exp
     module procedure exp_x_r
  end interface

  interface operator(.dot.)
     module procedure dot_x_y
     module procedure dot_r_x
     module procedure dot_x_r
  end interface

  interface operator(.cross.)
     module procedure cross_x_y
  end interface
  
contains

    subroutine set_ad(this, v)
        class(auto_var), intent(inout) :: this
        real(8), intent(in)            :: v
        this%x = v
        this%d = 1
    end subroutine set_ad

    subroutine set_constant_ad(this, v)
        class(auto_var), intent(inout) :: this
        real(8), intent(in)            :: v
        this%x = v
        this%d = 0
    end subroutine set_constant_ad
    
    function get_value_ad(this)
        class(auto_var), intent(in) :: this
        real(8) :: get_value_ad
        get_value_ad = this%x
    end function  get_value_ad
    
    function get_derivative_ad(this)
        class(auto_var), intent(in) :: this
        real(8) :: get_derivative_ad
        get_derivative_ad = this%d
    end function  get_derivative_ad

  type(auto_var) function create_av(x) result(r)
     real(8), intent(in) :: x
     r%x = x
  end function create_av
     
  subroutine assign_s(a, b)
    type(auto_var), intent(out) :: a
    type(auto_var), intent(in) :: b
    a%x = b%x
    a%d = b%d
  end subroutine assign_s
  
  subroutine assign_v(a, b)
    type(auto_var), intent(out) :: a(:)
    type(auto_var), intent(in) :: b(:)
    integer :: i
    do i=1,size(b)
      a(i)%x = b(i)%x
      a(i)%d = b(i)%d
    end do
  end subroutine assign_v
  
  function cross_x_y(a, b)
    type(auto_var) :: cross_x_y(3)
    type(auto_var), intent(in) :: a(3), b(3)
    integer :: i, j, k
    do i=1,3
       j = merge(1,i+1,i==3)
       k = merge(1,j+1,j==3)
       cross_x_y(i) = a(j)*b(k) - a(k)*b(j)
    end do
  end function cross_x_y

  function sqrt_av(x)
    type(auto_var) :: sqrt_av
    type(auto_var), intent(in) :: x
    sqrt_av = pow_x_r(x,0.5d0)
  end function sqrt_av

  function norm2_av(x)
    type(auto_var) :: norm2_av
    type(auto_var), intent(in) :: x(:)
    type(auto_var) :: v
    norm2_av%d = 0
    v = x .dot. x
    norm2_av = pow_x_r(v,0.5d0)
  end function norm2_av

  function dot_x_y(x, y)
    type(auto_var) :: dot_x_y
    type(auto_var), intent(in) :: x(:), y(:)
    integer :: i
    dot_x_y%d = 0
    do i=1,size(x)
       dot_x_y = dot_x_y + x(i)*y(i)
    end do
  end function dot_x_y

  function dot_r_x(r, x)
    type(auto_var) :: dot_r_x
    real(8), intent(in) :: r(:)
    type(auto_var), intent(in) :: x(:)
    type(auto_var) :: s(size(x))
    integer :: i
    do i=1,size(x)
       call set_val(s(i),r(i))
    end do
    dot_r_x = dot_x_y(s, x)
  end function dot_r_x

  function dot_x_r(x, r)
    type(auto_var) :: dot_x_r
    real(8), intent(in) :: r(:)
    type(auto_var), intent(in) :: x(:)
    type(auto_var) :: s(size(x))
    integer :: i
    do i=1,size(x)
       call set_val(s(i),r(i))
    end do
    dot_x_r = dot_x_y(s, x)
  end function dot_x_r

  real(8) function get_val_s(x)
    type(auto_var), intent(in) :: x
    get_val_s = x%x
  end function get_val_s

  function get_val_v(x)
    type(auto_var), intent(in) :: x(:)
    real(8) :: get_val_v(size(x))
    integer :: i
    do i=1,size(x)
      get_val_v(i) = x(i)%x
    end do
  end function get_val_v

  real(8) function get_der_s(x)
    type(auto_var), intent(in) :: x
    get_der_s = x%d
  end function get_der_s

  function get_der_v(x)
    type(auto_var), intent(in) :: x(:)
    real(8) :: get_der_v(size(x))
    integer :: i
    do i=1,size(x)
      get_der_v(i) = x(i)%d
    end do
  end function get_der_v

  subroutine set_val_r(x, v, isvar)
    type(auto_var), intent(inout) :: x
    real(8), intent(in) :: v
    logical, optional, intent(in) :: isvar
    x%x = v
    if (present(isvar)) then
       x%d = merge(1,0,isvar)
    end if
  end subroutine set_val_r

  subroutine set_val_n(x, v, isvar)
    type(auto_var), intent(inout) :: x
    integer, intent(in) :: v
    logical, optional, intent(in) :: isvar
    x%x = v
    if (present(isvar)) then
       x%d = merge(1,0,isvar)
    end if
  end subroutine set_val_n

   subroutine make_var_s(x, f)
    type(auto_var), intent(inout) :: x
    logical, optional, intent(in) :: f
    if (present(f)) then
       x%d = merge(1, 0, f)
    else
       x%d = 1
    end if
  end subroutine make_var_s

  ! Create an auto_var version of the input array
  subroutine make_var_v(in, out, index)
    real(8), intent(in)           :: in(:)
    type(auto_var), intent(out)   :: out(size(in))
    integer, optional, intent(in) :: index
    integer :: i
    do i=1,size(in)
      out(i)%x = in(i)
      if (present(index)) then
        out(i)%d = merge(1, 0, index == i)
      else
        out(i)%d = 0
      end if
    end do
  end subroutine make_var_v

  subroutine make_const(x)
    type(auto_var), intent(inout) :: x
    x%d = 0
  end subroutine make_const


  !----------------------------------------------------------------------------
  ! Equality test
  function equal(x, y) result(m)
    logical                    :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y

    m = abs(x%x-y%x) < 1.0d-8 .and. abs(x%d-y%d) < 1.0d-8
  end function equal

  !----------------------------------------------------------------------------
  ! Less-than test
  function less_than(x, y) result(m)
    logical                    :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y

    m = x%x < y%x
  end function less_than

  !----------------------------------------------------------------------------
  ! Greater-than test
  function greater_than(x, y) result(m)
    logical                    :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y

    m = x%x > y%x
  end function greater_than

  !----------------------------------------------------------------------------
  ! Less-equal test
  function less_equal(x, y) result(m)
    logical                    :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y

    m = x%x <= y%x
  end function less_equal

  !----------------------------------------------------------------------------
  ! Greater-equal test
  function greater_equal(x, y) result(m)
    logical                    :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y

    m = x%x >= y%x
  end function greater_equal

  !----------------------------------------------------------------------------
  ! Add

  function add_x_x_v(x, y) result(m)
    type(auto_var), intent(in) :: x(:)
    type(auto_var), intent(in) :: y(:)
    type(auto_var) :: m(size(x))
    integer :: i
    if (debug_level > 0) write(*,'(a)') 'add_x_x_v'
    do i=1,size(x)
      m(i) = x(i) + y(i)
    end do
  end function add_x_x_v

  function add_x_x(x, y) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y
    if (debug_level > 0) write(*,'(a)') 'add_x_x'
    m%x = x%x + y%x
    m%d = x%d + y%d
  end function add_x_x

  function add_x_r(x, r) result(m)
    type(auto_var)             :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'add_x_r'
    m%x = x%x + r
    m%d = x%d
  end function add_x_r

  function add_r_x(r, x) result(m)
    type(auto_var)             :: m
    real(8), intent(in)        :: r
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'add_r_x'
    m%x = x%x + r
    m%d = x%d
  end function add_r_x

  function add_x_n(x, r) result(m)
    type(auto_var)             :: m
    type(auto_var), intent(in) :: x
    integer, intent(in)        :: r
    if (debug_level > 0) write(*,'(a)') 'add_x_n'
    m%x = x%x + r
    m%d = x%d
  end function add_x_n

  function add_n_x(r, x) result(m)
    type(auto_var)             :: m
    integer, intent(in)        :: r
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'add_n_x'
    m%x = x%x + r
    m%d = x%d
  end function add_n_x

  !----------------------------------------------------------------------------
  ! Subtract

  function sub_x_x_v(x, y) result(m)
    type(auto_var), intent(in) :: x(:)
    type(auto_var), intent(in) :: y(:)
    type(auto_var) :: m(size(x))
    integer :: i
    if (debug_level > 0) write(*,'(a)') 'sub_x_x_v'
    do i=1,size(x)
      m(i) = x(i) - y(i)
    end do
  end function sub_x_x_v

  function sub_x_x(x, y) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y
    if (debug_level > 0) write(*,'(a)') 'sub_x_x'
    m%x = x%x - y%x
    m%d = x%d - y%d
  end function sub_x_x

  function sub_x_r(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'sub_x_r'
    m%x = x%x - r
    m%d = x%d
  end function sub_x_r

  function sub_r_x(r, x) result(m)
    type(auto_var) :: m
    real(8), intent(in) :: r
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'sub_r_x'
    m%x = r - x%x
    m%d = -x%d
  end function sub_r_x

  function sub_r_x_a(r, x) result(m)
    real(8), intent(in) :: r(:)
    type(auto_var), intent(in) :: x(:)
    type(auto_var) :: m(size(r))
    integer :: i
    if (debug_level > 0) write(*,'(a)') 'sub_r_x_a'
    do i=1, size(r)
       m(i) = r(i) - x(i)
    end do
  end function sub_r_x_a

  function sub_x_n(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    integer, intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'sub_x_n'
    m%x = x%x - r
    m%d = x%d
  end function sub_x_n

  function sub_n_x(r, x) result(m)
    type(auto_var) :: m
    integer, intent(in) :: r
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'sub_n_x'
    m%x = r - x%x
    m%d = -x%d
  end function sub_n_x

  !----------------------------------------------------------------------------
  ! Multiply

  function mult_x_x(uf, vf) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: uf
    type(auto_var), intent(in) :: vf
    if (debug_level > 0) write(*,'(a)') 'product rule'
    associate (u =>uf%x, v => vf%x, du => uf%d, dv => vf%d)
      m%x = u*v
      m%d = u*dv + v*du
    end associate
  end function mult_x_x

  function mult_x_r(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'mult_x_r'
    m = mult_x_x(x,auto_var(r,0))
  end function mult_x_r

  function mult_r_x(r, x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'mult_r_x'
    m = mult_x_x(auto_var(r,0),x)
  end function mult_r_x

  function mult_x_n(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    integer, intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'mult_x_r'
    m = mult_x_x(x,auto_var(r,0))
  end function mult_x_n

  function mult_n_x(r, x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    integer, intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'mult_n_x'
    m = mult_x_x(auto_var(r,0),x)
  end function mult_n_x

  !----------------------------------------------------------------------------
  ! Divide

  function div_x_x(uf, vf) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: uf
    type(auto_var), intent(in) :: vf
    if (debug_level > 0) write(*,'(a)') 'quotient rule'
    associate (u =>uf%x, v => vf%x, du => uf%d, dv => vf%d)
      m%x = u/v
      m%d = (v * du - u * dv)/v**2
    end associate
  end function div_x_x

  function div_x_r(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'div_x_r'
    m = div_x_x(x,auto_var(r,0))
  end function div_x_r

  function div_xv_x(x, r) result(m)
    type(auto_var), intent(in) :: x(:)
    type(auto_var), intent(in) :: r
    type(auto_var) :: m(size(x))
    integer :: i
    if (debug_level > 0) write(*,'(a)') 'div_xv_x'
    do i=1, size(x)
      m(i) = div_x_x(x(i),r)
    end do
  end function div_xv_x

  function div_r_x(r, x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'div_r_x'
    m = div_x_x(auto_var(r,0),x)
  end function div_r_x

  function div_x_n(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    integer, intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'div_x_n'
    m = div_x_x(x,auto_var(r,0))
  end function div_x_n

  function div_n_x(r, x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    integer, intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'div_n_x'
    m = div_x_x(auto_var(r,0),x)
  end function div_n_x

  !----------------------------------------------------------------------------
  ! Power

  function pow_x_x(x, y) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    type(auto_var), intent(in) :: y
    if (debug_level > 0) write(*,'(a)') 'pow_x_x'
    stop '[pow_x_x] not yet implemented'
    m%x = x%x ** y%x
    m%d = x%d * (1 + log(x%x))*m%x
  end function pow_x_x

  function pow_x_n(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    integer, intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'x**n'
    m%x = x%x ** r
    m%d = x%d * r*x%x ** (r-1)
  end function pow_x_n

  function pow_x_r(x, r) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'x**r'
    m%x = x%x ** r
    m%d = x%d * r*x%x ** (r-1)
  end function pow_x_r

  function pow_n_x(r, x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    integer, intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'n**x'
    m%x = r ** x%x
    m%d = x%d * r**x%x * log(dble(r))
  end function pow_n_x

  function pow_r_x(r, x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    real(8), intent(in) :: r
    if (debug_level > 0) write(*,'(a)') 'r**x'
    m%x = r ** x%x
    m%d = x%d * r**x%x * log(r)
  end function pow_r_x

  !----------------------------------------------------------------------------
  ! Trig
  function sin_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'sin_x_r'
    m%x = sin(x%x)
    m%d = x%d * cos(x%x)
  end function sin_x_r

  function cos_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'cos_x_r'
    m%x = cos(x%x)
    m%d = x%d * (-sin(x%x))
  end function cos_x_r

  function tan_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'tan_x_r'
    m  = div_x_x(sin(x),cos(x))
  end function tan_x_r

  !----------------------------------------------------------------------------
  ! Exponential
  function sinh_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'sinh_x_r'
    m%x = sinh(x%x)
    m%d = x%d * cosh(x%x)
  end function sinh_x_r

  function cosh_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'cosh_x_r'
    m%x = cosh(x%x)
    m%d = x%d * sinh(x%x)
  end function cosh_x_r

  function tanh_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'tanh_x_r'
    m  = div_x_x(sinh(x),cosh(x))
  end function tanh_x_r

  function log_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'log_x_r'
    m%x = log(x%x)
    m%d = x%d/x%x
  end function log_x_r

  function exp_x_r(x) result(m)
    type(auto_var) :: m
    type(auto_var), intent(in) :: x
    if (debug_level > 0) write(*,'(a)') 'exp_x_r'
    m%x = exp(x%x)
    m%d = m%x
  end function exp_x_r
 

  !============================================================================
  !
  ! Unit test
#ifdef UTEST
  subroutine utest

    type(auto_var) :: x, y, z, f, u(2), s(3), t(3), v(3)
    real(8) :: e = exp(1.0d0)

    call set_val(x,3)
    y = x**2
    call testResult('x**2',auto_var(9,6),y)

    y = x**3 + x
    call testResult('x**3 + x',auto_var(30,28),y)

    y = 3*x**2
    call testResult('3*x**2',auto_var(27,18),y)

    y = 3*x**4 + x*2
    call testResult('3*x**4 + 2*x',auto_var(3*3**4+2*3,12*3**3+2),y)

    y = 3*x**2 + x*2.0d0 - 5
    call testResult('3*x**2 + 2*x - 5',auto_var(28,20),y)

    y = (x + 2)*(x + 3)*(x - 1)
    call testResult('(x + 2)*(x + 3)*(x - 1)',auto_var((3+2)*(3+3)*(3-1),3*3**2+8*3+1),y)

    y = (x + 2)*(3 - x)*(x**2 - 1)
    call testResult('(x + 2)*(3 - x)*(x**2 - 1)',auto_var(0,-40),y)

    y = (x + 2)/(x**2 - 1)
    call testResult('(x + 2)/(x**2 - 1)',auto_var(1.5d0/2 - 0.5d0/4,-1.5d0/2**2+0.5d0/4**2),y)
    ! and now the same function as partial fractions
    y = 1.5d0/(x-1) - 0.5d0/(x+1)
    call testResult('1.5d0/(x-1) - 0.5d0/(x+1) [previous function as partial fractions]',&
         auto_var(1.5d0/2 - 0.5d0/4,-1.5d0/2**2+0.5d0/4**2),y)

    call set_val(x,0.5d0)
    y = 1.0d0/(1-x)
    call testResult('1/(1-x)',auto_var(2,4),y)

    call set_val(x,4)
    y = 4/x
    call testResult('4/x',auto_var(1,-1.0/4),y)

    y = e**x
    call testResult('e**x',auto_var(e**4,e**4),y)

    y = 2**x
    call testResult('2**x',auto_var(2**4,2**4*log(2.0d0)),y)

!!$    y = x**x  ! - not yet correctly implemented
!!$    call testResult('x**x',auto_var(4**4,(1+log(4.0d0))*4**4),y)

    call set_val(x,1)
    y = sin(x)
    call testResult('sin(x)',auto_var(sin(1.0d0),cos(1.0d0)),y)

    call set_val(x,2)
    y = cos(x)
    call testResult('cos(x)',auto_var(cos(2.0d0),-sin(2.0d0)),y)

    call set_val(x,2)
    y = sin(1/x)
    call testResult('sin(1/x)',auto_var(sin(1/2.0d0),-cos(1/2.0d0)/4),y)
    
    y = sqrt(x)
    call testResult('sqrt(x)',auto_var(sqrt(2.0d0), 0.5d0/sqrt(2.0d0)),y)

    call set_val(u(1),3,.false.)
    call set_val(u(2),4,.false.)
    y = norm2(u)
    call testResult('||y||',auto_var(5,0),y)
    call make_var(u(2))
    y = norm2(u)
    call testResult('sqrt(9+u(2)**2) == ||u||',sqrt(9 + u(2)**2),y)

    ! Cross product
    call set_val(x,2)
    call make_var([0.0d0, 1.0d0, -1.0d0],t)
    t(1) = x**2
    call set_val(s(1), 2, .false.)
    s(2) = x
    s(3) = 1/x
    v = t .cross. s
    call testResult('_t_ x _x_ (1)',auto_var(2.5d0,0.75d0),v(1))
    call testResult('_t_ x _x_ (2)',auto_var(-4.0d0,-1.0),v(2))
    call testResult('_t_ x _x_ (3)',auto_var(6.0d0,12.0d0),v(3))
    
   ! Partial derivatives
    call set_val(x,3,.true.)
    call set_val(y,2,.false.)
    z = x**2 - 2*x*y
    call testResult('x**2 - 2*x*y , Fx',auto_var(-3,2),z)

    call set_val(x,3,.false.)
    call set_val(y,2,.true.)
    z = x**2 - 2.0d0*x*y
    call testResult('x**2 - 2*x*y , Fy',auto_var(-3,-6),z)

    call set_val(x,2,.true.); call set_val(y,3,.false.); call set_val(z,0.6d0,.false.)
    f = (x-1)*sin(z) + (y+x)*cos(z/2)**2
    call testResult('(x-1)*sin(z) + (y+x)*cos(z/2)**2 Fx',auto_var(sin(0.6d0)+5*cos(0.3d0)**2,sin(0.6d0)+cos(0.3d0)**2),f)

    call set_val(x,2,.false.); call set_val(y,3,.true.); call set_val(z,0.6d0,.false.)
    f = (x-1)*sin(z) + (y+x)*cos(z/2)**2
    call testResult('(x-1)*sin(z) + (y+x)*cos(z/2)**2 Fy',auto_var(sin(0.6d0)+5*cos(0.3d0)**2,cos(0.3d0)**2),f)

    call set_val(x,2,.false.); call set_val(y,3,.false.); call set_val(z,0.6d0,.true.)
    f = (x-1)*sin(z) + (y+x)*cos(z/2)**2
    call testResult('(x-1)*sin(z) + (y+x)*cos(z/2)**2 Fz', &
         auto_var(sin(0.6d0)+5*cos(0.3d0)**2,cos(0.6d0) - 5*cos(0.3d0)*sin(0.3d0)),f)
         
         
    call set_val(x,3,.true.)
    f = log(4.0d0 + x)
    call testResult('log(4 + x)', &
         auto_var(log(4.0d0 + 3),1/(4.0+3)),f)

    call set_val(x,3,.true.)
    call set_val(y,2,.false.)
    f = log(4.0d0 + x**2)
    call testResult('log(4 + x**2)', &
         auto_var(log(4.0d0 + 3**2),2*3/(4.0+3**2)),f)
         
    ! The example in the comment at the top with call sequence
    block
        type(auto_var) :: f, x
        debug_level = 1
        write(*,'(a)') 'Call sequence for 1/(1-x), '
        call x%set(0.5d0)
        f = 1/(1-x)
        call testResult('1/(1-x)', &
         auto_var(1/(1-0.5d0),1/(1-0.5d0)**2),f)
    end block

        
    
  contains
    subroutine testResult(str,ref,res)
      character(len=*), intent(in) :: str
      type(auto_var), intent(in) :: res
      type(auto_var), intent(in) :: ref
      if (res == ref) then
         write(*,'(a)') 'passed'//' :: '//str
      else
         write(*,'(2(a,f0.6))') 'FAILED :: '//str//' => ', res%x - ref%x,' ; ', res%d - ref%d
      end if
    end subroutine testResult

  end subroutine utest
#endif
end module auto_diff

#ifdef UTEST
program tauto
  use auto_diff
  call utest
end program tauto
#endif
