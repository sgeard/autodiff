! Created by  on 13/03/2020.

submodule (AVD) AVD_sm
contains
    module function d0_over_i(obj, a) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        integer, intent(in)     :: a
        r%v = obj%v/a
    end function d0_over_i

    module function d0_over_r(obj, a) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        real(8), intent(in)     :: a
        r%v = obj%v/a
    end function d0_over_r

    module function i_over_d0(a, obj) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        integer, intent(in)     :: a
        r = a*obj**(-1)
    end function i_over_d0

    module function r_over_d0(a, obj) result(r)
        type(avd_b) :: r
        type(avd_b), intent(in) :: obj
        real(8), intent(in)     :: a
        r = a*obj**(-1)
    end function r_over_d0

    module function d1_over_i(obj, a) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        integer, intent(in)      :: a
        r%avd_b = obj%avd_b/a
        r%d1 = obj%d1/a
    end function d1_over_i

    module function d1_over_r(obj, a) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        real(8), intent(in)      :: a
        r%avd_b = obj%avd_b/a
        r%d1 = obj%d1/a
    end function d1_over_r

    module function i_over_d1(a, obj) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        integer, intent(in)      :: a
        r = a*obj**(-1)
    end function i_over_d1

    module function r_over_d1(a, obj) result(r)
        type(avd_d1) :: r
        type(avd_d1), intent(in) :: obj
        real(8), intent(in)      :: a
        r = a*obj**(-1)
    end function r_over_d1

    module function d2_over_i(obj, a) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        integer, intent(in)      :: a
        r%avd_d1 = obj%avd_d1/a
        r%d2 = obj%d2/a
    end function d2_over_i

    module function d2_over_r(obj, a) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)      :: a
        r%avd_d1 = obj%avd_d1/a
        r%d2 = obj%d2/a
    end function d2_over_r

    module function i_over_d2(a, obj) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        integer, intent(in)      :: a
        r = a*obj**(-1)
    end function i_over_d2

    module function r_over_d2(a, obj) result(r)
        type(avd_d2) :: r
        type(avd_d2), intent(in) :: obj
        real(8), intent(in)      :: a
        r = a*obj**(-1)
    end function r_over_d2

    module function d3_over_i(obj, a) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        integer, intent(in)      :: a
        r%avd_d2 = obj%avd_d2/a
        r%d3 = obj%d3/a
    end function d3_over_i

    module function d3_over_r(obj, a) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)      :: a
        r%avd_d2 = obj%avd_d2/a
        r%d3 = obj%d3/a
    end function d3_over_r

    module function i_over_d3(a, obj) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        integer, intent(in)      :: a
        r = a*obj**(-1)
    end function i_over_d3

    module function r_over_d3(a, obj) result(r)
        type(avd_d3) :: r
        type(avd_d3), intent(in) :: obj
        real(8), intent(in)      :: a
        r = a*obj**(-1)
    end function r_over_d3

    module function d4_over_i(obj, a) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        integer, intent(in)      :: a
        r%avd_d3 = obj%avd_d3/a
        r%d4 = obj%d4/a
    end function d4_over_i

    module function d4_over_r(obj, a) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)      :: a
        r%avd_d3 = obj%avd_d3/a
        r%d4 = obj%d4/a
    end function d4_over_r

    module function i_over_d4(a, obj) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        integer, intent(in)      :: a
        r = a*obj**(-1)
    end function i_over_d4

    module function r_over_d4(a, obj) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: obj
        real(8), intent(in)      :: a
        r = a*obj**(-1)
    end function r_over_d4

    module function d4_over_d4(a, b) result(r)
        type(avd_d4) :: r
        type(avd_d4), intent(in) :: a, b
        r = a*b**(-1)
    end function d4_over_d4

end submodule AVD_sm