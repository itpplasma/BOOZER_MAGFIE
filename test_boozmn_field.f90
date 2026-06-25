PROGRAM test_boozmn_field
! Real (non-vacuous) test for neo_read_boozmn.
! Reads a boozmn NetCDF generated from circ.bc via the libneo bc_to_booz_xform
! converter and cross-checks loaded arrays against Python-computed reference
! values to floating-point accuracy.
!
! Reference values (from bc_to_booz_xform applied to circ.bc):
!   nsurf = 63, nfp = 1, mnmax = 19, m0b = 18, n0b = 0
!   bmnc(1,1) = 1.9633546400  (m=0 n=0, first surface)
!   bmnc(1,2) = -0.0754078850 (m=1 n=0, first surface)
!   zmnc(1,2) = 0.0705114198  (zmns_b, Z sin-mode, first surface)
!   lmnc(1,2) = 3.61678092e-5 (pmns_b, lambda sin-mode, first surface)
!   iota(1)   = 0.90001405
!   es(1)     = 0.01562500
!
  USE neo_precision
  USE neo_input
  USE neo_exchange, ONLY: iota, curr_pol, curr_tor
  USE neo_sub_mod,  ONLY: neo_read_boozmn
  IMPLICIT NONE

  CHARACTER(len=256) :: boozmn_file
  INTEGER :: env_len, env_stat
  INTEGER :: nfail
  REAL(dp) :: tol_dp, tol_lo

  nfail  = 0
  tol_dp = 1.0e-7_dp
  tol_lo = 1.0e-4_dp

  CALL get_environment_variable('BOOZMN_FIXTURE', boozmn_file, &
       env_len, env_stat)
  IF (env_stat /= 0 .OR. env_len == 0) THEN
     boozmn_file = 'tokamak_circ/boozmn_circ.nc'
  END IF

  WRITE(6,'(a)') 'test_boozmn_field: reading ' // trim(boozmn_file)
  CALL neo_read_boozmn(boozmn_file)

  ! ---- dimensions (Bug 5: n0b/m0b from actual mode content) ----
  IF (ns /= 63) THEN
     WRITE(6,'(a,i0,a)') 'FAIL ns: got ', ns, ', expected 63'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,i0)') 'PASS ns = ', ns
  END IF

  IF (nfp /= 1) THEN
     WRITE(6,'(a,i0,a)') 'FAIL nfp: got ', nfp, ', expected 1'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,i0)') 'PASS nfp = ', nfp
  END IF

  IF (mnmax /= 19) THEN
     WRITE(6,'(a,i0,a)') 'FAIL mnmax: got ', mnmax, ', expected 19'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,i0)') 'PASS mnmax = ', mnmax
  END IF

  IF (m0b /= 18) THEN
     WRITE(6,'(a,i0,a)') 'FAIL m0b: got ', m0b, ', expected 18'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,i0)') 'PASS m0b = ', m0b
  END IF

  IF (n0b /= 0) THEN
     WRITE(6,'(a,i0,a)') 'FAIL n0b: got ', n0b, ', expected 0'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,i0)') 'PASS n0b = ', n0b
  END IF

  ! ---- mode indices ----
  IF (ixm(1) /= 0) THEN
     WRITE(6,'(a,i0,a)') 'FAIL ixm(1): got ', ixm(1), ', expected 0'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,i0)') 'PASS ixm(1) = ', ixm(1)
  END IF

  IF (ixm(2) /= 1) THEN
     WRITE(6,'(a,i0,a)') 'FAIL ixm(2): got ', ixm(2), ', expected 1'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,i0)') 'PASS ixm(2) = ', ixm(2)
  END IF

  ! ---- pixm/pixn populated (Bug 2: was never filled after early RETURN) ----
  IF (ANY(pixm == 0)) THEN
     WRITE(6,'(a)') 'FAIL pixm: contains zero entries (not populated)'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a)') 'PASS pixm fully populated'
  END IF

  IF (ANY(pixn == 0)) THEN
     WRITE(6,'(a)') 'FAIL pixn: contains zero entries (not populated)'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a)') 'PASS pixn fully populated'
  END IF

  ! ---- bmnc: B-field Fourier coefficients ----
  IF (ABS(bmnc(1,1) - 1.9633546400_dp) > tol_dp * 1.9633546400_dp) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL bmnc(1,1): got ', bmnc(1,1), &
          ', expected 1.9633546400'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS bmnc(1,1) = ', bmnc(1,1)
  END IF

  IF (ABS(bmnc(1,2) + 0.0754078850_dp) > tol_dp * 0.0754078850_dp) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL bmnc(1,2): got ', bmnc(1,2), &
          ', expected -0.0754078850'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS bmnc(1,2) = ', bmnc(1,2)
  END IF

  ! ---- zmnc: Z sin-modes (Bug 1+3: zmns_b must go into zmnc, not unallocated zmns) ----
  IF (ABS(zmnc(1,1)) > 1.0e-14_dp) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL zmnc(1,1): got ', zmnc(1,1), &
          ', expected ~0'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS zmnc(1,1) = ', zmnc(1,1)
  END IF

  IF (ABS(zmnc(1,2) - 0.0705114198_dp) > tol_dp * 0.0705114198_dp) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL zmnc(1,2): got ', zmnc(1,2), &
          ', expected 0.0705114198'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS zmnc(1,2) = ', zmnc(1,2)
  END IF

  ! ---- lmnc: lambda sin-modes (Bug 4: pmns_b must be used, not hardcoded 0) ----
  IF (ABS(lmnc(1,1)) > 1.0e-14_dp) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL lmnc(1,1): got ', lmnc(1,1), &
          ', expected ~0'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS lmnc(1,1) = ', lmnc(1,1)
  END IF

  IF (ABS(lmnc(1,2) - 3.61678092e-5_dp) > tol_lo * ABS(3.61678092e-5_dp)) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL lmnc(1,2): got ', lmnc(1,2), &
          ', expected 3.61678092e-5'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS lmnc(1,2) = ', lmnc(1,2)
  END IF

  IF (ALL(lmnc == 0.0_dp)) THEN
     WRITE(6,'(a)') 'FAIL lmnc: all zero, pmns_b not used'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a)') 'PASS lmnc non-zero (pmns_b used)'
  END IF

  ! ---- iota and radial coordinate ----
  IF (ABS(iota(1) - 0.90001405_dp) > tol_lo) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL iota(1): got ', iota(1), &
          ', expected ~0.90001405'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS iota(1) = ', iota(1)
  END IF

  IF (ABS(es(1) - 0.01562500_dp) > tol_dp) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL es(1): got ', es(1), &
          ', expected 0.01562500'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS es(1) = ', es(1)
  END IF

  ! ---- rmnc sanity ----
  IF (rmnc(1,1) < 0.5_dp .OR. rmnc(1,1) > 5.0_dp) THEN
     WRITE(6,'(a,es20.12,a)') 'FAIL rmnc(1,1): got ', rmnc(1,1), &
          ', expected order ~1 m'
     nfail = nfail + 1
  ELSE
     WRITE(6,'(a,es20.12)') 'PASS rmnc(1,1) = ', rmnc(1,1)
  END IF

  ! ---- summary ----
  WRITE(6,'(/,a)') '==============================='
  IF (nfail == 0) THEN
     WRITE(6,'(a)') 'ALL TESTS PASSED'
     STOP 0
  ELSE
     WRITE(6,'(a,i0,a)') 'FAILED: ', nfail, ' assertion(s)'
     STOP 1
  END IF

END PROGRAM test_boozmn_field
