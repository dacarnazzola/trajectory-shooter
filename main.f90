program main
implicit none

  integer, parameter :: dp = selected_real_kind(15)
  type fly_data
    real(dp) :: altitude !! [FT] height above ground
    real(dp) :: mach     !! [N/A] Mach number
    real(dp) :: heading  !! [DEG] heading in degrees, 0 = due east along [1, 0, 0]
    real(dp) :: fly_time !! [SEC] max fly time in seconds
    real(dp) :: fly_dt   !! [SEC] timestep for generated trajectory in seconds
  end type fly_data
  integer :: i, j
  type(fly_data) :: x0

  x0%heading = 30.0_dp
  x0%fly_time = 60.0_dp
  x0%fly_dt = 5.0_dp
  do i=20000,35000,5000
    x0%altitude = real(i, dp)
    do j=9,12
      x0%mach = real(j, dp)/10.0_dp
      call fly(x0)
      write(*,'(a)') ''
    end do
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

    pure elemental function deg2rad(deg_in) result(rad_out)
    implicit none
      real(dp), intent(in) :: deg_in
      real(dp) :: rad_out
      real(dp), parameter :: rad_per_deg = acos(-1.0_dp)/180.0_dp
      rad_out = deg_in*rad_per_deg
    end function deg2rad

    subroutine fly(x0)
    implicit none
      type(fly_data), intent(in) :: x0
      real(dp) :: x(3), v(3), speed_fps, heading_rad, t
      x = [0.0_dp, 0.0_dp, x0%altitude]
      speed_fps = x0%mach*mach1(x(3))
      heading_rad = deg2rad(x0%heading)
      v = speed_fps*[cos(heading_rad), sin(heading_rad), 0.0_dp]
      t = 0.0_dp
      do while (t .le. x0%fly_time)
        write(*,'(7(a,e22.15))') 'T=',t,',X=',x(1),',Y=',x(2),',Z=',x(3),',VX=',v(1),',VY=',v(2),',VZ=',v(3)
        t = t + x0%fly_dt
        x = x + v*x0%fly_dt
      end do
    end subroutine fly

end program main
