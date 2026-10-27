PROGRAM test_booz_xform_integration
  USE neo_precision, ONLY: dp
  USE magfie_mod, ONLY: magfie
  USE neo_magfie_mod, ONLY: magfie_spline, magfie_sarray
  USE, INTRINSIC :: ieee_arithmetic, ONLY: ieee_is_finite
  IMPLICIT NONE

  CHARACTER(len=512) :: mode, reference_file
  REAL(dp) :: x(3), actual(14), expected(14), scale(14)
  INTEGER :: i, j, k, unit, nfail

  CALL get_command_argument(1, mode)
  CALL get_command_argument(2, reference_file)
  IF (mode /= 'write' .AND. mode /= 'compare') ERROR STOP 'Expected write or compare'
  IF (LEN_TRIM(reference_file) == 0) ERROR STOP 'Missing reference file'
  IF (mode == 'write') THEN
     OPEN(newunit=unit, file=TRIM(reference_file), status='replace')
  ELSE
     OPEN(newunit=unit, file=TRIM(reference_file), status='old')
  END IF

  magfie_spline = 1
  ALLOCATE(magfie_sarray(3))
  magfie_sarray = [0.15_dp, 0.4_dp, 0.7_dp]
  nfail = 0
  DO i = 1, SIZE(magfie_sarray)
     x(1) = magfie_sarray(i)
     DO j = 1, 5
        x(2) = 0.31_dp * j
        x(3) = 0.73_dp * j
        CALL magfie(x, actual(1), actual(2), actual(3:5), &
             actual(6:8), actual(9:11), actual(12:14))
        IF (.NOT. ALL(ieee_is_finite(actual))) ERROR STOP 'Non-finite field'
        IF (mode == 'write') THEN
           WRITE(unit, '(14es25.16)') actual
        ELSE
           READ(unit, *) expected
           IF (.NOT. ALL(ieee_is_finite(expected))) ERROR STOP 'Non-finite reference'
           ! The text .bc rounds s to five significant digits; boozmn uses exact half-grid s.
           scale(1:2) = ABS(expected(1:2))
           DO k = 3, 12, 3
              scale(k:k+2) = MAXVAL(ABS(expected(k:k+2)))
           END DO
           IF (ANY(ABS(actual - expected) > 1.0e-8_dp + 5.0e-4_dp * scale)) THEN
              WRITE(*, *) 'FAIL same-source field at s, phi, theta = ', x
              WRITE(*, *) 'bc: ', expected
              WRITE(*, *) 'nc: ', actual
              nfail = nfail + 1
           END IF
        END IF
     END DO
  END DO
  CLOSE(unit)
  IF (nfail /= 0) ERROR STOP 'Same-source field mismatch'
END PROGRAM test_booz_xform_integration
