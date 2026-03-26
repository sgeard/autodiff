program utest

    use AVD, T1=>avd_d1, T2=>avd_d2, T3=>avd_d3, T4=>avd_d4

    real(8)  :: c, t

    t = 0.5d0
    call init

    t_1: block
        type(T1) :: a, u, v, w, s, ref
        a = t
        u = sin(a)
        v = cos(a)
        w = u*v ! w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
        ref = T1(0.5*sin(1.0),cos(1.0)) !,-2*sin(1.0d0)) !,-4*cos(1.0d0),8*sin(1.0d0))
        call testResult_av1('T1 => sin(x)*cos(x)',ref,w)

        u = 0.5*sin(2*a)
        call testResult_av1('T1 => 0.5*sin(2x)',ref,u)

        s = 0.5
        w = s**3 + s
        ref = T1(0.5**3+0.5,3*0.5**2 + 1) !,6*0.5) !, 6, 0)
        call testResult_av1('T1 => x**3 + x',ref,w)

        w = (3*s + 1) * (1 + s) * (1 - s)
        ref = T1((3*t + 1)*(1 + t)*(1 - t),3-2*t-9*t**2) !,-18*t-2,-18.0d0)
        call testResult_av1('T1 => (3*x + 1)*(1 + x)*(1 - x)',ref,w)
    end block t_1

    t_2: block
        type(T2) :: a, u, v, w, s, ref
        a = t
        u = sin(a)
        v = cos(a)
        w = u*v ! w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
        ref = T2(0.5*sin(1.0),cos(1.0),-2*sin(1.0)) !,-4*cos(1.0d0),8*sin(1.0d0))
        call testResult_av2('T2 => sin(x)*cos(x)',ref,w)

        u = 0.5*sin(2*a)
        call testResult_av2('T2 => 0.5*sin(2x)',ref,u)

        s = 0.5
        w = s**3 + s
        ref = T2(0.5**3+0.5,3*0.5**2 + 1,6*0.5) !, 6, 0)
        call testResult_av2('T2 => x**3 + x',ref,w)

        w = (3*s + 1) * (1 + s) * (1 - s)
        ref = T2((3*t + 1)*(1 + t)*(1 - t),3-2*t-9*t**2,-18*t-2) !,-18.0d0)
        call testResult_av2('T2 => (3*x + 1)*(1 + x)*(1 - x)',ref,w)
    end block t_2

    t_3: block
        type(T3) :: a, u, v, w, s, ref

        a = t
        u = sin(a)
        v = cos(a)
        w = u*v ! w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
        ref = T3(0.5*sin(1.0),cos(1.0),-2*sin(1.0),-4*cos(1.0)) !,8*sin(1.0d0))
        call testResult_av3('T3 => sin(x)*cos(x)',ref,w)

        u = 0.5*sin(2*a)
        call testResult_av3('T3 => 0.5*sin(2x)',ref,u)

        s = 0.5
        w = s**3 + s
        ref = T3(0.5**3+0.5,3*0.5**2 + 1,6*0.5, 6)
        call testResult_av3('T3 => x**3 + x',ref,w)

        w = (3*s + 1) * (1 - s) * (1 + s)
        ref = T3((3*t + 1)*(1 + t)*(1 - t),3-2*t-9*t**2,-18*t-2,-18.0)
        call testResult_av3('T3 => (3*x + 1)*(1 + x)*(1 - x)',ref,w)

        w = s**5
        c = t
        ref = T3(c**5,5*c**4,5*4*c**3,5*4*3*c**2)
        call testResult_av3('T3 => x**5',ref,w)

        w = (2 - 3*s)**5
        c = (2 - 3*t)
        ref = T3(c**5,5*(-3)*c**4,5*4*(-3)*(-3)*c**3,5*4*3*(-3)*(-3)*(-3)*c**2)
        call testResult_av3('T3 => (2-3x)**5',ref,w)
    end block t_3

    t_4: block
        type(T4) :: a, u, v, w, s, ref
        real(8)  :: r4, s4, y, y1, y2, y3
        a = t
        u = sin(a)
        v = cos(a)
        w = u*v ! w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
        ref = T4(0.5*sin(1.0),cos(1.0),-2*sin(1.0),-4*cos(1.0),8*sin(1.0))
        call testResult_av4('T4 => sin(x)*cos(x)',ref,w)

        u = 0.5*sin(2*a)
        call testResult_av4('T4 => 0.5*sin(2x)',ref,u)

        s = 0.5
        w = s**3 + s
        ref = T4(0.5**3+0.5,3*0.5**2 + 1,6*0.5, 6, 0)
        call testResult_av4('T4 => x**3 + x',ref,w)

        w = (3*s + 1) * (1 + s) * (1 - s)
        ref = T4((3*t + 1)*(1 + t)*(1 - t),3-2*t-9*t**2,-18*t-2,-18.0, 0.0)
        call testResult_av4('T4 => (3*x + 1)*(1 + x)*(1 - x)',ref,w)

        w = (2 - 3*s)**5
        ref = T4(t**5,5*(-3)*t**4,5*4*(-3)*(-3)*t**3,5*4*3*(-3)*(-3)*(-3)*t**2,5*4*3*2*(-3)*(-3)*(-3)*(-3)*t)
        call testResult_av4('T4 => (2-3x)**5',ref,w)

        w = 1/s;
        ref = T4(1/0.5,-(1/0.5**2),2/(0.5**3),-6/(0.5**4),24/(0.5**5))
        call testResult_av4('T4 => 1/x',ref,w)

        w = 1/sqrt(4-s**2)
        s4 = 1/sqrt(4-t**2)
        r4 = 24*(0.5**4 + 12*0.5**2 + 6)*s4**9
        ref = T4( &
        s4, &
        0.5*s4**3, &
        s4**3 + 3*0.5**2*s4**5, &
        3*0.5*s4**5*(3+5*(0.5*s4)**2), &
        r4)
        call testResult_av4('T4 => 1/sqrt(4-x**2)',ref,w)

        w = tan(s)
        y = tan(t)
        ref = T4(y, 1+y**2, 2*y*(1+y**2), 2*(1+3*y**2)*(1+y**2), 8*y*(3*y**2+2)*(1+y**2))
        call testResult_av4('T4 => tan(x)',ref,w)
        w = 2*tan(s/2) / (1-tan(s/2)**2)
        call testResult_av4('T4 => 2tan(x/2)/(1-tan(x/2)**2) == tan(x)',ref,w)

        w = s*exp(2*s)
        y = exp(2*t)
        ref = T4(t*y,(2*t+1)*y,4*(t+1)*y,4*(2*t+3)*y,16*(t+2)*y)
        call testResult_av4('T4 => x*exp(2x)',ref,w)

        w = s*log(2*s)
        y = log(2*t)
        ref = T4(t*y,1+log(2.0)+log(t),1/t,-1/t**2,2/t**3)
        call testResult_av4('T4 => x*log(2x)',ref,w)

        u = 0.5*sinh(2*a)
        ref = T4(0.5*sinh(1.0),cosh(1.0),2*sinh(1.0),4*cosh(1.0),8*sinh(1.0))
        call testResult_av4('T4 => 0.5*sinh(2x)',ref,u)

        u = 0.5*cosh(2*a)
        ref = T4(0.5*cosh(1.0),sinh(1.0),2*cosh(1.0),4*sinh(1.0),8*cosh(1.0))
        call testResult_av4('T4 => 0.5*cosh(2x)',ref,u)

        w = tanh(s)
        y = tanh(t)
        y1 = 1 - y**2
        y2 = -2*y*y1
        y3 = -2*(y1**2 + y*y2)
        ref = T4(y, y1, y2, y3, -2*(3*y1*y2 + y*y3))
        call testResult_av4('T4 => tanh(x)',ref,w)

        w = 2*tanh(s/2) / (1+tanh(s/2)**2)
        call testResult_av4('T4 => 2tanh(x/2)/(1+tanh(x/2)**2) == tanh(x)',ref,w)

        !w = 2**s
    end block t_4

    stop
    contains
!        subroutine testResult(str,ref,res)
!            character(len=*), intent(in) :: str
!            class(auto_var_u), intent(in) :: res
!            class(auto_var_u), intent(in) :: ref
!            if (res == ref) then
!                write(*,'(a)') 'passed'//' :: '//str
!                return
!            end if
!            write(*,'(a)') 'FAILED :: '//str//' => '//res%as_string()//' != '//ref%as_string()
!        end subroutine testResult

        subroutine testResult_av1(str,ref,res)
            character(len=*), intent(in) :: str
            type(T1), intent(in) :: res
            type(T1), intent(in) :: ref
            if (res == ref) then
                write(*,'(a)') 'passed'//' :: '//str
                return
            end if
            write(*,'(a)') 'FAILED :: '//str//' => '//res%as_string()//' != '//ref%as_string()
        end subroutine testResult_av1

        subroutine testResult_av2(str,ref,res)
            character(len=*), intent(in) :: str
            type(T2), intent(in) :: res
            type(T2), intent(in) :: ref
            if (res == ref) then
                write(*,'(a)') 'passed'//' :: '//str
                return
            end if
            write(*,'(a)') 'FAILED :: '//str//' => '//res%as_string()//' != '//ref%as_string()
        end subroutine testResult_av2

        subroutine testResult_av3(str,ref,res)
            character(len=*), intent(in) :: str
            type(T3), intent(in) :: res
            type(T3), intent(in) :: ref
            if (res == ref) then
                write(*,'(a)') 'passed'//' :: '//str
                return
            end if
            write(*,'(a)') 'FAILED :: '//str//' => '//res%as_string()//' != '//ref%as_string()
        end subroutine testResult_av3

        subroutine testResult_av4(str,ref,res)
            character(len=*), intent(in) :: str
            type(T4), intent(in) :: res
            type(T4), intent(in) :: ref
            if (res == ref) then
                write(*,'(a)') 'passed'//' :: '//trim(str)
                return
            end if
            write(*,'(a)') 'FAILED :: '//trim(str)//' => '//res%as_string()//' != '//ref%as_string()
        end subroutine testResult_av4

end program utest