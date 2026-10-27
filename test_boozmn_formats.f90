PROGRAM test_boozmn_formats
  USE netcdf
  USE neo_precision, ONLY: dp
  IMPLICIT NONE
  CHARACTER(len=512) :: input_file, output_file, variant
  CHARACTER(len=nf90_max_name) :: name
  INTEGER :: src, dst, id, out_id, ns, nm, nr, nvars, nd, xtype, k
  INTEGER :: dims(nf90_max_var_dims), newdims(3), radius, modes, surfaces, packed
  INTEGER, ALLOCATABLE :: ints(:)
  REAL(dp), ALLOCATABLE :: profile(:), harmonic(:, :)
  INTEGER :: scalar

  CALL get_command_argument(1, input_file)
  CALL get_command_argument(2, output_file)
  CALL get_command_argument(3, variant)
  CALL check(nf90_open(TRIM(input_file), nf90_nowrite, src))
  CALL check(nf90_inq_dimid(src, 'radius', id))
  CALL check(nf90_inquire_dimension(src, id, len=nr))
  CALL check(nf90_inq_dimid(src, 'mn_mode', id))
  CALL check(nf90_inquire_dimension(src, id, len=nm))
  CALL check(nf90_inq_dimid(src, 'comput_surfs', id))
  CALL check(nf90_inquire_dimension(src, id, len=ns))
  CALL check(nf90_inquire(src, nVariables=nvars))
  CALL check(nf90_create(TRIM(output_file), nf90_clobber, dst))
  CALL check(nf90_def_dim(dst, 'radius', nr, radius))
  CALL check(nf90_def_dim(dst, 'mn_modes', nm, modes))
  CALL check(nf90_def_dim(dst, 'comput_surfs', ns, surfaces))
  CALL check(nf90_def_dim(dst, 'pack_rad', ns, packed))

  ! Match the public booz_xform writer, including its on-disk pmns sign.
  DO id = 1, nvars
     CALL check(nf90_inquire_variable(src, id, name, xtype, nd, dims))
     IF (variant == 'missing-bmnc' .AND. name == 'bmnc_b') CYCLE
     SELECT CASE (nd)
     CASE (0)
        CALL check(nf90_def_var(dst, name, xtype, out_id))
     CASE (1)
        CALL check(nf90_inquire_dimension(src, dims(1), name=name))
        SELECT CASE (name)
        CASE ('radius')
           newdims(1) = radius
        CASE ('mn_mode')
           newdims(1) = modes
        CASE ('comput_surfs')
           newdims(1) = surfaces
        CASE DEFAULT
           ERROR STOP 'Unexpected input dimension'
        END SELECT
        CALL check(nf90_inquire_variable(src, id, name=name))
        CALL check(nf90_def_var(dst, name, xtype, newdims(:1), out_id))
     CASE (2)
        CALL check(nf90_def_var(dst, name, xtype, [packed, modes], out_id))
     CASE DEFAULT
        ERROR STOP 'Unexpected input rank'
     END SELECT
  END DO
  CALL check(nf90_enddef(dst))
  DO id = 1, nvars
     CALL check(nf90_inquire_variable(src, id, name, xtype, nd, dims))
     IF (variant == 'missing-bmnc' .AND. name == 'bmnc_b') CYCLE
     CALL check(nf90_inq_varid(dst, name, out_id))
     SELECT CASE (nd)
     CASE (0)
        CALL check(nf90_get_var(src, id, scalar))
        IF (variant == 'asymmetric' .AND. name == 'lasym__logical__') scalar = 1
        IF (variant == 'nonaxisymmetric' .AND. name == 'nboz_b') scalar = 1
        CALL check(nf90_put_var(dst, out_id, scalar))
     CASE (1)
        CALL check(nf90_inquire_dimension(src, dims(1), len=k))
        IF (xtype == nf90_int) THEN
           ALLOCATE(ints(k))
           CALL check(nf90_get_var(src, id, ints))
           IF (variant == 'bad-jlist' .AND. name == 'jlist') ints(1) = 0
           CALL check(nf90_put_var(dst, out_id, ints))
           DEALLOCATE(ints)
        ELSE
           ALLOCATE(profile(k))
           CALL check(nf90_get_var(src, id, profile))
           CALL check(nf90_put_var(dst, out_id, profile))
           DEALLOCATE(profile)
        END IF
     CASE (2)
        ALLOCATE(harmonic(nm, ns))
        CALL check(nf90_get_var(src, id, harmonic))
        IF (name == 'pmns_b') harmonic = -harmonic
        CALL check(nf90_put_var(dst, out_id, TRANSPOSE(harmonic)))
        DEALLOCATE(harmonic)
     END SELECT
  END DO
  CALL check(nf90_close(src))
  CALL check(nf90_close(dst))
CONTAINS
  SUBROUTINE check(status)
    INTEGER, INTENT(in) :: status
    IF (status /= nf90_noerr) THEN
       WRITE(*, *) TRIM(nf90_strerror(status))
       ERROR STOP 'Fixture conversion failed'
    END IF
  END SUBROUTINE check
END PROGRAM test_boozmn_formats
