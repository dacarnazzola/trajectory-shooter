program main
implicit none

  integer, parameter :: dp = selected_real_kind(15)
  integer :: i

  do i=0,100000,5000
    write(*,'(a,i3,a,e12.6,a)') '[',i/1000,'kft] Mach 1 = ',mach1(real(i, dp)),' feet/second'
  end do

  contains

    !! mach1 WORKS - calculates speed of sound at sea level as 1116.27 ft/sec vs WolframAlpha says 1116.4 ft/sec
    pure elemental function mach1(alt_ft) result(mach1_fps)
    implicit none
      real(dp), intent(in) :: alt_ft
      real(dp) :: mach1_fps
      real(dp) :: T_F, T_R, yair
      !! earth atmosphere model for temperature comes from NASA: https://www.grc.nasa.gov/www/k-12/airplane/atmos.html
      !! fahrenheit to rankine from engineeringtoolbox: https://www.engineeringtoolbox.com/temperature-d_291.html
      if (alt_ft .lt. 36152.0_dp) then
        T_R = 518.67_dp - 0.00356_dp*alt_ft
      else if ((alt_ft .ge. 36152.0_dp) .and. (alt_ft .lt. 82345.0_dp)) then
        T_R = 389.67_dp
      else
        T_R = 254.62_dp + 0.00164_dp*alt_ft
      end if
      !! specific heat for calorically imperfect gas comes from NASA: https://www.grc.nasa.gov/www/BGH/realspec.html
      yair = 1.0_dp + 0.4_dp/(1.0_dp + 0.4_dp*(((5500.0_dp/T_R)**2)*exp(5500.0_dp/T_R)/(exp(5500.0_dp/T_R) - 1.0_dp)**2))
      !! specific gas constant for air comes from engineeringtoolbox: https://www.engineeringtoolbox.com/individual-universal-gas-constant-d_587.html
      mach1_fps = sqrt(yair*1716.541777_dp*T_R)
    end function mach1

end program main
