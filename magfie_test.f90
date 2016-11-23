PROGRAM magfie_test
  USE neo_exchange, ONLY: nper,b_min,b_max, &
       theta_bmin,theta_bmax,phi_bmin,phi_bmax
  USE magfie_mod, ONLY: magfie, stevvo, magfie_deallocate
  USE neo_magfie_mod, ONLY: magfie_spline, magfie_sarray
  USE nrtype, ONLY: twopi
  IMPLICIT NONE  
  
  INTEGER, PARAMETER     :: DP = KIND(1.0D0)
  REAL(dp)               :: pi
  REAL(dp), DIMENSION(3) :: x
  REAL(dp)               :: sqrtg
  REAL(dp), DIMENSION(3) :: bder
  REAL(dp), DIMENSION(3) :: hcovar
  REAL(dp), DIMENSION(3) :: hctrvr
  REAL(dp), DIMENSION(3) :: hcurl
    
  REAL(dp) :: bmod

!!$  REAL(dp)               :: RT0
!!$  REAL(dp)               :: R0i
!!$  INTEGER                :: nfp
!!$  REAL(dp)               :: cbfi
!!$  REAL(dp)               :: bz0i
!!$  REAL(dp)               :: bf0

  REAL(dp)               :: thetab,phib,boozer_s
  INTEGER                :: i, k
  !INTEGER                :: test_len

  REAL(dp), DIMENSION(:), ALLOCATABLE :: theta_arr,phi_arr,s_arr
  integer                :: theta_n,theta_k,phi_n,phi_k,s_n,s_k

  pi=4.d0*atan(1.d0)

  !*******************************************************************
  ! Settings
  !*******************************************************************
  magfie_spline = 1
  !*******************************************************************
  ! Reading
  !*******************************************************************
  OPEN(unit=7,file='magfie_test.in',status='old')
  READ(7,*) boozer_s
  READ(7,*) phi_n
  READ(7,*) theta_n
  CLOSE(unit=7)

  !*******************************************************************
  ! Initialization
  ! First call to magfie through stevv0
  !*******************************************************************

  ALLOCATE(magfie_sarray(1))
  magfie_sarray = boozer_s
  
  x(1) = boozer_s
  x(2:3) = 0.0d0
  CALL magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )

  ! CALL stevvo( RT0, R0i, nfp, cbfi, bz0i, bf0 )

  !*******************************************************************
  ! Testoutput
  !*******************************************************************
  PRINT *, 'nper:   ', nper
  !*******************************************************************
  ! Files
  !*******************************************************************
  IF (magfie_spline .EQ. 0) THEN
     OPEN(unit=7,file='magfie_dir.dat',status='replace')
  ELSE
     OPEN(unit=7,file='magfie_spl.dat',status='replace')
  END IF
  
  ! prepare theta and phi
  allocate(theta_arr(theta_n+1))
  do k = 0,theta_n
     theta_arr(k+1) = 2d0*pi * k /theta_n
  end do
  allocate(phi_arr(phi_n+1))
  do k = 0,phi_n
     phi_arr(k+1) = 2d0*pi / nper * k /phi_n
  end do

  ! change s just for testing
  s_n = 3
  allocate(s_arr(s_n))
  do k = 1,s_n
     s_arr(k) = magfie_sarray(1) * k
  end do
  
  ! make the loop and produce output
  do s_k = 1,s_n
     x(1) = s_arr(s_k)
     print *, 'Computation for s = ',x(1)
     do phi_k = 1,phi_n+1
        x(2) = phi_arr(phi_k)
        do theta_k = 1,theta_n+1
           x(3) = theta_arr(theta_k)
           CALL magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
           write(7,*) x,bmod,sqrtg
        end do
     end do
     PRINT *, 'b_min:   ', b_min
     PRINT *, 'b_max:   ', b_max
  end do

  ! run loop for second time
  ! make the loop and produce output
  do s_k = 1,s_n
     x(1) = s_arr(s_k)
     print *, 'Computation for s = ',x(1)
     do phi_k = 1,phi_n+1
        x(2) = phi_arr(phi_k)
        do theta_k = 1,theta_n+1
           x(3) = theta_arr(theta_k)
           CALL magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
           CALL magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
           CALL magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
           CALL magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
           CALL magfie( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
           write(7,*) x,bmod,sqrtg
        end do
     end do
     PRINT *, 'b_min:   ', b_min
     PRINT *, 'b_max:   ', b_max
  end do
  
  DEALLOCATE( magfie_sarray )
  DEALLOCATE( theta_arr )
  DEALLOCATE( phi_arr )
  CALL magfie_deallocate
  CLOSE(unit=7)

END PROGRAM magfie_test

