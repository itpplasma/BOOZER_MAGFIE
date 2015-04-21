MODULE neo_magfie_mod

  USE neo_precision
  USE neo_input,                                                       &
       ONLY: es, ixm, ixn, mnmax, psi_pr, pixm, pixn, nfp
  USE neo_control,                                                     &
       ONLY: fluxs_interp, write_progress, phi_n, theta_n, lab_swi,    &
       inp_swi
  USE neo_sub_mod,                                                     &
       ONLY: neo_read_control, neo_init, neo_init_spline
  USE neo_spline_data,                                                 &
       ONLY: r_mhalf,                                                  &
       a_bmnc, b_bmnc, c_bmnc, d_bmnc,                                 &
       a_bmns, b_bmns, c_bmns, d_bmns,                                 &
       a_rmnc, b_rmnc, c_rmnc, d_rmnc,                                 &
       a_rmns, b_rmns, c_rmns, d_rmns,                                 &
       a_zmnc, b_zmnc, c_zmnc, d_zmnc,                                 &
       a_zmns, b_zmns, c_zmns, d_zmns,                                 &
       a_lmnc, b_lmnc, c_lmnc, d_lmnc,                                 &
       a_lmns, b_lmns, c_lmns, d_lmns,                                 &
       a_iota, b_iota, c_iota, d_iota,                                 &
       a_curr_tor, b_curr_tor, c_curr_tor, d_curr_tor,                 &
       a_curr_pol, b_curr_pol, c_curr_pol, d_curr_pol,                 &
       a_pprime,   b_pprime,   c_pprime,   d_pprime,                   &
       a_sqrtg00,  b_sqrtg00,  c_sqrtg00,  d_sqrtg00
  USE inter_interfaces,                                                &
       ONLY: splint_horner3,                                           &
       tf, tfp, tfpp, tfppp,                                           &
       tfone, tfzero
  USE neo_work,                                                        &
       ONLY: cosmth, cosnph, sinmth, sinnph, theta_int, phi_int,       &
       theta_start, theta_end, phi_start, phi_end
  USE neo_actual_fluxs, ONLY : s_sqrtg00
  USE spline_mod, ONLY: spl2d, poi2d, eva2d
  !! Modifications by Andreas F. Martitsch (12.03.2014)
  ! Use this quantity for normalization. Note:
  ! Variable is computed in mag_interface.f90 ("boozer_bmod0").
  ! It is available for the first time after 1st call
  ! of "make_magnetics". Therefore, within the first two calls
  ! of "neo_magfie" this variable is zero, but these calls are
  ! not used for the computation of physical quantities.
  USE partpa_mod,  ONLY : bmod0
  !! End Modifications by Andreas F. Martitsch (12.03.2014)  
  
  !---------------------------------------------------------------------------
  !USE var_sub_misc, ONLY: fac_c,iota_m ! fac_m
  !---------------------------------------------------------------------------

  IMPLICIT NONE
  REAL(dp), DIMENSION(:), ALLOCATABLE              :: magfie_sarray
  INTEGER                                          :: magfie_spline    = 0
  INTEGER                                          :: magfie_newspline = 1
  ! switch for different output:
  !  0:  output for SMT and BMC
  !  1:  output for NEO 
  INTEGER                                          :: magfie_result    = 0
  INTEGER                                          :: magfie_sarray_len

  REAL(dp), DIMENSION(:), ALLOCATABLE              :: curr_tor_array
  REAL(dp), DIMENSION(:), ALLOCATABLE              :: curr_tor_s_array
  REAL(dp), DIMENSION(:), ALLOCATABLE              :: curr_pol_array
  REAL(dp), DIMENSION(:), ALLOCATABLE              :: curr_pol_s_array
  REAL(dp), DIMENSION(:), ALLOCATABLE              :: iota_array
  REAL(dp), DIMENSION(:), ALLOCATABLE              :: pprime_array
  REAL(dp), DIMENSION(:), ALLOCATABLE              :: sqrtg00_array

  REAL(dp)                                         :: s_pprime

  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: bmod_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: bb_s_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: bb_tb_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: bb_pb_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: gval_spl
  !! Modifications by Andreas F. Martitsch (11.03.2014)
  ! Storage arrays for the 2d splines (over the flux-surface) of the additionally
  ! needed metric tensor elements (used to compute the B-field components,
  ! which are necessary for modeling the magnetic rotation).
  ! Once computed these arrays can be used to reconstruct the desired
  ! quantities at intermediate (theta,phi)-values.
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: gstb_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: gspb_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: gstb_tb_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: gspb_tb_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: gstb_pb_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: gspb_pb_spl
  !! End Modifications by Andreas F. Martitsch (11.03.2014)
  !! Modifications by Andreas F. Martitsch (13.11.2014)
  ! Storage arrays for the 2d splines (over the flux-surface) of the additionally
  ! needed quantities for NTV output
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: R_spl
  REAL(dp), DIMENSION(:,:,:,:,:), ALLOCATABLE, TARGET      :: Z_spl
  !! End Modifications by Andreas F. Martitsch (13.11.2014)
  
  REAL(dp) :: boozer_iota
  REAL(dp) :: boozer_sqrtg00
  !! Modifications by Andreas F. Martitsch (12.03.2014)
  ! boozer_curr_tor, boozer_curr_pol, boozer_psi_pr,
  ! boozer_sqrtg11 and boozer_isqrg are now converted
  ! to cgs-units.
  ! This step requires changes within rhs_kin.f90 and
  ! ripple_solver.f90!
  REAL(dp) :: boozer_curr_pol_hat
  REAL(dp) :: boozer_curr_tor_hat
  ! Radial derivatives of toroidal / poloidal currents
  ! (In fact these currents are already the respective
  ! covariant B-field components; conversion done within
  ! neo_read)
  REAL(dp) :: boozer_curr_pol_hat_s
  REAL(dp) :: boozer_curr_tor_hat_s
  REAL(dp) :: boozer_psi_pr_hat
  REAL(dp) :: boozer_sqrtg11 ! Test
  REAL(dp) :: boozer_isqrg
  !! End Modifications by Andreas F. Martitsch (12.03.2014)
  
  REAL(dp), PRIVATE :: av_b2_m ! Klaus
  
  !! Modifications by Andreas F. Martitsch (11.03.2014)
  ! Transfer the computed values of the additionally needed 
  ! B-field components between neo_magfie_a and neo_magfie_b
  ! (best solution at the moment, since keyword optional 
  ! does not seem to work for a module procedure)
  REAL(dp), PRIVATE :: dbcovar_s_dtheta
  REAL(dp), PRIVATE :: dbcovar_s_dphi
  !! End Modifications by Andreas F. Martitsch (11.03.2014)

  !! Modifications by Andreas F. Martitsch (13.11.2014)
  ! Local variables for the additionally needed quantities for NTV output
  REAL(dp), PRIVATE :: r_val, z_val    
  !! End Modifications by Andreas F. Martitsch (13.11.2014)
  
  INTERFACE neo_magfie
     MODULE PROCEDURE neo_magfie_a, neo_magfie_b, neo_magfie_c
  END INTERFACE

CONTAINS

  SUBROUTINE neo_magfie_a( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
    ! input / output
    REAL(dp), DIMENSION(:),       INTENT(in)            :: x
    REAL(dp),                     INTENT(out)           :: bmod
    REAL(dp),                     INTENT(out)           :: sqrtg
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out)           :: bder
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out)           :: hcovar
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out)           :: hctrvr
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out)           :: hcurl
    ! local definitions
    !! Modifications by Andreas F. Martitsch (11.03.2014)
    ! Locally computed values of the additionally needed 
    ! metric tensor elements
    REAL(dp)                                         :: gstb, gspb
    REAL(dp)                                         :: gstb_tb, gspb_tb
    REAL(dp)                                         :: gstb_pb, gspb_pb
    ! Locally computed value of the additionally needed 
    ! B-field component
    REAL(dp)                                         :: bcovar_s
    !! End Modifications by Andreas F. Martitsch (11.03.2014)
    INTEGER(i4b)                                     :: swd = 1
    INTEGER                                          :: i, m, n
    INTEGER                                          :: npsi
    REAL(dp)                                         :: m0  = 0.0_dp
    REAL(dp)                                         :: yp, ypp, yppp

    REAL(dp)                                         :: bmnc, bmnc_s
    REAL(dp)                                         :: sinv, cosv
    REAL(dp)                                         :: iota
    REAL(dp)                                         :: curr_tor, curr_tor_s
    REAL(dp)                                         :: curr_pol, curr_pol_s
    REAL(dp)                                         :: bb_s, bb_tb, bb_pb
    REAL(dp)                                         :: fac, fac1

    INTEGER                                          :: k_es
    INTEGER                                          :: s_detected
    INTEGER                                          :: imn    
    INTEGER                                          :: it, ip, im, in
    INTEGER                                          :: mt = 1
    INTEGER                                          :: mp = 1
    INTEGER                                          :: theta_ind, phi_ind
    INTEGER                                          :: ierr
    REAL(dp)                                         :: s
    REAL(dp)                                         :: magfie_epsi = 1.e-9
    REAL(dp)                                         :: bi, bi_s, ri, zi, li
    !! Modifications by Andreas F. Martitsch (07.03.2014)
    ! Auxiliary variables for the Fourier summation
    REAL(dp)                                         :: ri_s, zi_s, li_s
    REAL(dp)                                         :: bis, bis_s, ris, zis, lis
    REAL(dp)                                         :: ris_s, zis_s, lis_s
    !! End Modifications by Andreas F. Martitsch (07.03.2014)
    REAL(dp)                                         :: theta_d, phi_d
    
    REAL(dp), DIMENSION(:), ALLOCATABLE              :: s_bmnc, s_bmnc_s
    REAL(dp), DIMENSION(:), ALLOCATABLE              :: s_rmnc, s_zmnc, s_lmnc
    !! Modifications by Andreas F. Martitsch (06.03.2014)
    ! Radial derivatives of (R,Z,phi)-components obtained from the 1d spline
    REAL(dp), DIMENSION(:), ALLOCATABLE              :: s_rmnc_s, s_zmnc_s, s_lmnc_s
    !! End Modifications by Andreas F. Martitsch (06.03.2014)
    !! Modifications by Andreas F. Martitsch (06.08.2014)
    ! Additional data from Boozer files without Stellarator symmetry
    REAL(kind=dp),    DIMENSION(:),     ALLOCATABLE :: s_bmns, s_bmns_s    
    REAL(kind=dp),    DIMENSION(:),     ALLOCATABLE :: s_rmns, s_zmns, s_lmns
    REAL(kind=dp),    DIMENSION(:),     ALLOCATABLE :: s_rmns_s, s_zmns_s, s_lmns_s
    !! End Modifications by Andreas F. Martitsch (06.08.2014)
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: bmod_a
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: bb_s_a
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: bb_tb_a
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: bb_pb_a
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r,z,l
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_tb,z_tb,p_tb
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_pb,z_pb,p_pb
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: gtbtb,gpbpb,gtbpb
    !! Modifications by Andreas F. Martitsch (11.03.2014)
    ! Temporary storage arrays for the Fourier summations related to
    ! the radial derivatives of (R,Z,phi)-components and the
    ! additionally needed metric tensor elements
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_s,z_s,p_s
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_stb,z_stb,p_stb
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_tbtb,z_tbtb,p_tbtb
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_pbtb,z_pbtb,p_pbtb
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_spb,z_spb,p_spb
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: r_pbpb,z_pbpb,p_pbpb
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: gstb_a,gspb_a
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: gstb_tb_a,gspb_tb_a
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: gstb_pb_a,gspb_pb_a
    !! End Modifications by Andreas F. Martitsch (11.03.2014)
    REAL(dp), DIMENSION(:,:), ALLOCATABLE            :: sqrg11_met

    REAL(dp), DIMENSION(:,:,:,:), POINTER            :: p_spl
    
    REAL(dp) :: isqrg, sqrg11

    !REAL(dp) :: s_sqrtg00_m Klaus
    
    !*******************************************************************
    ! Initialisation if necessary
    !*******************************************************************
    IF ( .NOT. ALLOCATED(es) ) THEN
       CALL neo_read_control()
       fluxs_interp = 1
       CALL neo_init(npsi)
       PRINT *, 'theta_start,theta_end,phi_start,phi_end'
       PRINT *, theta_start,theta_end,phi_start,phi_end
    END IF
    !*******************************************************************
    ! Spline of surfaces in magfie_sarray
    !*******************************************************************
    IF (magfie_spline .EQ. 1 .AND. magfie_newspline .EQ. 1) THEN
       magfie_sarray_len =  SIZE(magfie_sarray)
       !****************************************************************
       ! Allocation
       !****************************************************************
       ALLOCATE( bmod_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( bb_s_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( bb_tb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( bb_pb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( gval_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       !! Modifications by Andreas F. Martitsch (11.03.2014)
       ! Allocate the storage arrays for the 2d spline interpolation
       ! (over the flux-surface) of the additionally needed metric
       ! tensor elements 
       ALLOCATE( gstb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( gspb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( gstb_tb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( gspb_tb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( gstb_pb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( gspb_pb_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       !! End Modifications by Andreas F. Martitsch (11.03.2014)
       !! Modifications by Andreas F. Martitsch (13.11.2014)
       ! Allocate storage arrays for the 2d periodic splines
       ! of the additionally needed quantities for NTV output
       ALLOCATE( R_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       ALLOCATE( Z_spl(4,4,theta_n,phi_n,magfie_sarray_len) )
       !! End Modifications by Andreas F. Martitsch (13.11.2014)
       
       ALLOCATE( curr_tor_array(magfie_sarray_len) )
       ALLOCATE( curr_tor_s_array(magfie_sarray_len) )
       ALLOCATE( curr_pol_array(magfie_sarray_len) )
       ALLOCATE( curr_pol_s_array(magfie_sarray_len) )
       ALLOCATE( iota_array(magfie_sarray_len) )
       ALLOCATE( pprime_array(magfie_sarray_len) )
       ALLOCATE( sqrtg00_array(magfie_sarray_len) )
       !****************************************************************
       ! Loop over predefined s-values 
       !****************************************************************
       DO k_es = 1, magfie_sarray_len
          s = magfie_sarray(k_es)
          !*************************************************************
          ! Surface
          !*************************************************************
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Initialize Surface, k_es = ',k_es
          END IF
          ALLOCATE ( s_bmnc(mnmax) )
          ALLOCATE ( s_bmnc_s(mnmax) )
          ALLOCATE ( s_rmnc(mnmax) )
          ALLOCATE ( s_zmnc(mnmax) )
          ALLOCATE ( s_lmnc(mnmax) )
          !! Modifications by Andreas F. Martitsch (06.03.2014)
          ! Compute the necessary radial derivatives for the 
          ! (R,Z,phi)-components obtained from the 1d spline
          ALLOCATE ( s_rmnc_s(mnmax) ) ! Allocate arrays for additional
          ALLOCATE ( s_zmnc_s(mnmax) ) ! radial derivatives
          ALLOCATE ( s_lmnc_s(mnmax) )
          !! End Modifications by Andreas F. Martitsch (06.03.2014)
          !
          !! Modifications by Andreas F. Martitsch (06.08.2014)
          ! Additional data from Boozer files without Stellarator symmetry
          IF (inp_swi .EQ. 9) THEN        ! ASDEX-U (E. Strumberger)
             ALLOCATE ( s_bmns(mnmax) )
             ALLOCATE ( s_bmns_s(mnmax) )
             ALLOCATE ( s_rmns(mnmax) )
             ALLOCATE ( s_zmns(mnmax) )
             ALLOCATE ( s_lmns(mnmax) )
             ALLOCATE ( s_rmns_s(mnmax) )
             ALLOCATE ( s_zmns_s(mnmax) )
             ALLOCATE ( s_lmns_s(mnmax) )             
          END IF
          !! End Modifications by Andreas F. Martitsch (06.08.2014)
          !
          DO imn = 1, mnmax
             ! Switch swd turns on (1) / off (0) the computation of the
             ! radial derivatives within splint_horner3
             swd = 1
             CALL splint_horner3(es,                                   &
                  a_bmnc(:,imn), b_bmnc(:,imn),                        &
                  c_bmnc(:,imn), d_bmnc(:,imn),                        &
                  swd, r_mhalf(imn),                                   &
                  s, tf, tfp, tfpp, tfppp,                             &
                  s_bmnc(imn), s_bmnc_s(imn), ypp, yppp)
             !swd = 0 ! Now we want to compute the radial derivatives
             swd = 1
             CALL splint_horner3(es,                                   &
                  a_rmnc(:,imn), b_rmnc(:,imn),                        &
                  c_rmnc(:,imn), d_rmnc(:,imn),                        &
                  swd, r_mhalf(imn),                                   &
                  s, tf, tfp, tfpp, tfppp,                             &
                  s_rmnc(imn), s_rmnc_s(imn), ypp, yppp)
             swd = 1
             CALL splint_horner3(es,                                   &
                  a_zmnc(:,imn), b_zmnc(:,imn),                        &
                  c_zmnc(:,imn), d_zmnc(:,imn),                        &
                  swd, r_mhalf(imn),                                   &
                  s, tf, tfp, tfpp, tfppp,                             &
                  s_zmnc(imn), s_zmnc_s(imn), ypp, yppp)
             swd = 1
             CALL splint_horner3(es,                                   &
                  a_lmnc(:,imn), b_lmnc(:,imn),                        &
                  c_lmnc(:,imn), d_lmnc(:,imn),                        &
                  swd, r_mhalf(imn),                                   &
                  s, tf, tfp, tfpp, tfppp,                             &
                  s_lmnc(imn), s_lmnc_s(imn), ypp, yppp)
             !
             !! Modifications by Andreas F. Martitsch (06.08.2014)
             ! Additional data from Boozer files without Stellarator symmetry
             IF (inp_swi .EQ. 9) THEN        ! ASDEX-U (E. Strumberger)
                swd = 1
                CALL splint_horner3(es,                                   &
                     a_bmns(:,imn), b_bmns(:,imn),                        &
                     c_bmns(:,imn), d_bmns(:,imn),                        &
                     swd, r_mhalf(imn),                                   &
                     s, tf, tfp, tfpp, tfppp,                             &
                     s_bmns(imn), s_bmns_s(imn), ypp, yppp)
                swd = 1
                CALL splint_horner3(es,                                   &
                     a_rmns(:,imn), b_rmns(:,imn),                        &
                     c_rmns(:,imn), d_rmns(:,imn),                        &
                     swd, r_mhalf(imn),                                   &
                     s, tf, tfp, tfpp, tfppp,                             &
                     s_rmns(imn), s_rmns_s(imn), ypp, yppp)
                swd = 1
                CALL splint_horner3(es,                                   &
                     a_zmns(:,imn), b_zmns(:,imn),                        &
                     c_zmns(:,imn), d_zmns(:,imn),                        &
                     swd, r_mhalf(imn),                                   &
                     s, tf, tfp, tfpp, tfppp,                             &
                     s_zmns(imn), s_zmns_s(imn), ypp, yppp)
                swd = 1
                CALL splint_horner3(es,                                   &
                     a_lmns(:,imn), b_lmns(:,imn),                        &
                     c_lmns(:,imn), d_lmns(:,imn),                        &
                     swd, r_mhalf(imn),                                   &
                     s, tf, tfp, tfpp, tfppp,                             &
                     s_lmns(imn), s_lmns_s(imn), ypp, yppp)
             END IF
             !! End Modifications by Andreas F. Martitsch (06.08.2014)
             !
          END DO
          !*************************************************************
          ! Fourier summation for the full theta-phi array
          !*************************************************************
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Do Fourier'
          END IF
          ALLOCATE( bmod_a(theta_n,phi_n) ) 
          ALLOCATE( bb_s_a(theta_n,phi_n) ) 
          ALLOCATE( bb_tb_a(theta_n,phi_n) ) 
          ALLOCATE( bb_pb_a(theta_n,phi_n) )
          bmod_a  = 0.0_dp
          bb_s_a  = 0.0_dp
          bb_tb_a = 0.0_dp
          bb_pb_a = 0.0_dp

          ALLOCATE( r(theta_n,phi_n) )  ! NEW
          ALLOCATE( z(theta_n,phi_n) ) 
          ALLOCATE( l(theta_n,phi_n) ) 
          ALLOCATE( r_tb(theta_n,phi_n) ) 
          ALLOCATE( z_tb(theta_n,phi_n) ) 
          ALLOCATE( p_tb(theta_n,phi_n) ) 
          ALLOCATE( r_pb(theta_n,phi_n) ) 
          ALLOCATE( z_pb(theta_n,phi_n) ) 
          ALLOCATE( p_pb(theta_n,phi_n) ) 
          r = 0.0d0
          z = 0.0d0
          l = 0.0d0
          r_tb = 0.0d0
          z_tb = 0.0d0
          p_tb = 0.0d0
          r_pb = 0.0d0
          z_pb = 0.0d0
          p_pb = 0.0d0
          !! Modifications by Andreas F. Martitsch (11.03.2014)
          ! Allocate temporary storage arrays for the Fourier summations
          ! related to the radial derivatives of (R,Z,phi)-components
          ALLOCATE( r_s(theta_n,phi_n) )
          ALLOCATE( z_s(theta_n,phi_n) )
          ALLOCATE( p_s(theta_n,phi_n) )
          ALLOCATE( r_stb(theta_n,phi_n) )
          ALLOCATE( z_stb(theta_n,phi_n) )
          ALLOCATE( p_stb(theta_n,phi_n) )
          ALLOCATE( r_tbtb(theta_n,phi_n) ) 
          ALLOCATE( z_tbtb(theta_n,phi_n) ) 
          ALLOCATE( p_tbtb(theta_n,phi_n) ) 
          ALLOCATE( r_pbtb(theta_n,phi_n) ) 
          ALLOCATE( z_pbtb(theta_n,phi_n) ) 
          ALLOCATE( p_pbtb(theta_n,phi_n) )
          ALLOCATE( r_spb(theta_n,phi_n) )
          ALLOCATE( z_spb(theta_n,phi_n) )
          ALLOCATE( p_spb(theta_n,phi_n) )
          ALLOCATE( r_pbpb(theta_n,phi_n) ) 
          ALLOCATE( z_pbpb(theta_n,phi_n) ) 
          ALLOCATE( p_pbpb(theta_n,phi_n) ) 
          r_s = 0.0d0
          z_s = 0.0d0
          p_s = 0.0d0
          r_stb = 0.0d0
          z_stb = 0.0d0
          p_stb = 0.0d0
          r_tbtb = 0.0d0
          z_tbtb = 0.0d0
          p_tbtb = 0.0d0
          r_pbtb = 0.0d0
          z_pbtb = 0.0d0
          p_pbtb = 0.0d0
          r_spb = 0.0d0
          z_spb = 0.0d0
          p_spb = 0.0d0
          r_pbpb = 0.0d0
          z_pbpb = 0.0d0
          p_pbpb = 0.0d0
          !! End Modifications by Andreas F. Martitsch (11.03.2014)

          DO imn=1,mnmax
             ri = s_rmnc(imn) ! NEW
             zi = s_zmnc(imn)
             li = s_lmnc(imn)
             !! Modifications by Andreas F. Martitsch (07.03.2014)
             ! Auxiliary variables for the Fourier summation
             ri_s = s_rmnc_s(imn)
             zi_s = s_zmnc_s(imn)
             li_s = s_lmnc_s(imn)
             !! End Modifications by Andreas F. Martitsch (07.03.2014)
             bi   = s_bmnc(imn)
             bi_s = s_bmnc_s(imn)
             !! Modifications by Andreas F. Martitsch (06.08.2014)
             ! Additional data from Boozer files without Stellarator symmetry
             IF (inp_swi .EQ. 9) THEN        ! ASDEX-U (E. Strumberger)
                !
                ris = s_rmns(imn)
                zis = s_zmns(imn)
                lis = s_lmns(imn)
                bis = s_bmns(imn)
                !
                ris_s = s_rmns_s(imn)
                zis_s = s_zmns_s(imn)
                lis_s = s_lmns_s(imn)
                bis_s = s_bmns_s(imn)
                !
             END IF
             !! End Modifications by Andreas F. Martitsch (06.08.2014)
             m = ixm(imn)
             n = ixn(imn)
             im = pixm(imn)
             in = pixn(imn)
             DO ip=1,phi_n
                DO it=1,theta_n
                   !! Modifications by Andreas F. Martitsch (06.08.2014)
                   ! Additional data from Boozer files without Stellarator symmetry
                   IF (inp_swi .EQ. 9) THEN        ! ASDEX-U (E. Strumberger)
                      cosv = cosmth(it,im) * cosnph(ip,in) - sinmth(it,im) * sinnph(ip,in)
                      sinv = sinmth(it,im) * cosnph(ip,in) + cosmth(it,im) * sinnph(ip,in)

                      bmod_a(it,ip) = bmod_a(it,ip)     + bi*cosv   + bis*sinv
                      bb_s_a(it,ip) = bb_s_a(it,ip)     + bi_s*cosv + bis_s*sinv
                      bb_tb_a(it,ip)  = bb_tb_a(it,ip)  - m*bi*sinv + m*bis*cosv
                      bb_pb_a(it,ip)  = bb_pb_a(it,ip)  - n*bi*sinv + n*bis*cosv
                      
                      r(it,ip) = r(it,ip) + ri*cosv + ris*sinv
                      z(it,ip) = z(it,ip) + zi*cosv + zis*sinv
                      l(it,ip) = l(it,ip) + li*cosv + lis*sinv                     

                      r_tb(it,ip) = r_tb(it,ip) - m*ri*sinv + m*ris*cosv
                      r_pb(it,ip) = r_pb(it,ip) - n*ri*sinv + n*ris*cosv
                      z_tb(it,ip) = z_tb(it,ip) - m*zi*sinv + m*zis*cosv
                      z_pb(it,ip) = z_pb(it,ip) - n*zi*sinv + n*zis*cosv
                      p_tb(it,ip) = p_tb(it,ip) + m*li*sinv - m*lis*cosv ! -l_tb
                      p_pb(it,ip) = p_pb(it,ip) + n*li*sinv - n*lis*cosv ! -l_pb

                      r_s(it,ip) = r_s(it,ip) + ri_s*cosv + ris_s*sinv
                      z_s(it,ip) = z_s(it,ip) + zi_s*cosv + zis_s*sinv
                      p_s(it,ip) = p_s(it,ip) + li_s*cosv + lis_s*sinv ! -l_s

                      r_stb(it,ip) = r_stb(it,ip) - m*ri_s*sinv + m*ris_s*cosv
                      z_stb(it,ip) = z_stb(it,ip) - m*zi_s*sinv + m*zis_s*cosv
                      p_stb(it,ip) = p_stb(it,ip) + m*li_s*sinv - m*lis_s*cosv ! -l_stb

                      r_spb(it,ip) = r_spb(it,ip) - n*ri_s*sinv + n*ris_s*cosv
                      z_spb(it,ip) = z_spb(it,ip) - n*zi_s*sinv + n*zis_s*cosv
                      p_spb(it,ip) = p_spb(it,ip) + n*li_s*sinv - n*lis_s*cosv ! -l_spb
                      
                      r_tbtb(it,ip) = r_tbtb(it,ip) - m*m*ri*cosv - m*m*ris*sinv
                      z_tbtb(it,ip) = z_tbtb(it,ip) - m*m*zi*cosv - m*m*zis*sinv                      
                      p_tbtb(it,ip) = p_tbtb(it,ip) + m*m*li*cosv + m*m*lis*sinv ! -l_tbtb

                      r_pbtb(it,ip) = r_pbtb(it,ip) - m*n*ri*cosv - m*n*ris*sinv
                      z_pbtb(it,ip) = z_pbtb(it,ip) - m*n*zi*cosv - m*n*zis*sinv                      
                      p_pbtb(it,ip) = p_pbtb(it,ip) + m*n*li*cosv + m*n*lis*sinv ! -l_pbtb                      
                      
                      r_pbpb(it,ip) = r_pbpb(it,ip) - n*n*ri*cosv - n*n*ris*sinv
                      z_pbpb(it,ip) = z_pbpb(it,ip) - n*n*zi*cosv - n*n*zis*sinv                      
                      p_pbpb(it,ip) = p_pbpb(it,ip) + n*n*li*cosv + n*n*lis*sinv ! -l_pbpb
                   ELSE
                      cosv = cosmth(it,im) * cosnph(ip,in) + sinmth(it,im) * sinnph(ip,in)
                      sinv = sinmth(it,im) * cosnph(ip,in) - cosmth(it,im) * sinnph(ip,in)

                      bmod_a(it,ip)   = bmod_a(it,ip)   +     bi   * cosv
                      bb_s_a(it,ip)   = bb_s_a(it,ip)   +     bi_s * cosv
                      bb_tb_a(it,ip)  = bb_tb_a(it,ip)  - m * bi   * sinv
                      bb_pb_a(it,ip)  = bb_pb_a(it,ip)  + n * bi   * sinv

                      r(it,ip) = r(it,ip) + ri*cosv
                      z(it,ip) = z(it,ip) + zi*sinv
                      l(it,ip) = l(it,ip) + li*sinv

                      r_tb(it,ip) = r_tb(it,ip) - m*ri*sinv
                      r_pb(it,ip) = r_pb(it,ip) + n*ri*sinv
                      z_tb(it,ip) = z_tb(it,ip) + m*zi*cosv
                      z_pb(it,ip) = z_pb(it,ip) - n*zi*cosv
                      p_tb(it,ip) = p_tb(it,ip) - m*li*cosv ! -l_tb
                      p_pb(it,ip) = p_pb(it,ip) + n*li*cosv ! -l_pb
                      !! Modifications by Andreas F. Martitsch (07.03.2014)
                      ! Temporary storage arrays for the Fourier summations
                      ! related to the radial derivatives of (R,Z,phi)-components
                      r_s(it,ip) = r_s(it,ip) + ri_s*cosv
                      z_s(it,ip) = z_s(it,ip) + zi_s*sinv
                      p_s(it,ip) = p_s(it,ip) - li_s*sinv ! -l_s
                      r_stb(it,ip) = r_stb(it,ip) - m*ri_s*sinv
                      z_stb(it,ip) = z_stb(it,ip) + m*zi_s*cosv
                      p_stb(it,ip) = p_stb(it,ip) - m*li_s*cosv ! -l_stb
                      r_tbtb(it,ip) = r_tbtb(it,ip) - m*m*ri*cosv
                      z_tbtb(it,ip) = z_tbtb(it,ip) - m*m*zi*sinv
                      p_tbtb(it,ip) = p_tbtb(it,ip) + m*m*li*sinv ! -l_tbtb
                      r_pbtb(it,ip) = r_pbtb(it,ip) + m*n*ri*cosv
                      z_pbtb(it,ip) = z_pbtb(it,ip) + m*n*zi*sinv
                      p_pbtb(it,ip) = p_pbtb(it,ip) - m*n*li*sinv ! -l_pbtb
                      r_spb(it,ip) = r_spb(it,ip) + n*ri_s*sinv
                      z_spb(it,ip) = z_spb(it,ip) - n*zi_s*cosv
                      p_spb(it,ip) = p_spb(it,ip) + n*li_s*cosv ! -l_spb
                      r_pbpb(it,ip) = r_pbpb(it,ip) - n*n*ri*cosv
                      z_pbpb(it,ip) = z_pbpb(it,ip) - n*n*zi*sinv
                      p_pbpb(it,ip) = p_pbpb(it,ip) + n*n*li*sinv ! -l_pbpb
                      !! End Modifications by Andreas F. Martitsch (07.03.2014)
                   END IF
                   !! End Modifications by Andreas F. Martitsch (06.08.2014)
                END DO
             END DO
          END DO
          DEALLOCATE( s_bmnc )
          DEALLOCATE( s_bmnc_s )
          DEALLOCATE( s_rmnc )
          DEALLOCATE( s_zmnc )
          DEALLOCATE( s_lmnc )
          !! Modifications by Andreas F. Martitsch (07.03.2014)
          ! Deallocate arrays for the additional radial derivatives
          DEALLOCATE( s_rmnc_s )
          DEALLOCATE( s_zmnc_s )
          DEALLOCATE( s_lmnc_s )
          !! End Modifications by Andreas F. Martitsch (07.03.2014)
          !
          !! Modifications by Andreas F. Martitsch (06.08.2014)
          ! Additional data from Boozer files without Stellarator symmetry
          IF (inp_swi .EQ. 9) THEN        ! ASDEX-U (E. Strumberger)
             DEALLOCATE ( s_bmns )
             DEALLOCATE ( s_bmns_s )
             DEALLOCATE ( s_rmns )
             DEALLOCATE ( s_zmns )
             DEALLOCATE ( s_lmns )
             DEALLOCATE ( s_rmns_s )
             DEALLOCATE ( s_zmns_s )
             DEALLOCATE ( s_lmns_s )             
          END IF
          !! End Modifications by Andreas F. Martitsch (06.08.2014)
          !
          IF (lab_swi .EQ. 5 .OR. lab_swi .EQ. 3) THEN ! CHS, LHD
             p_tb = - p_tb
             p_pb = 1 - p_pb
             !! Modifications by Andreas F. Martitsch (07.03.2014)
             ! ToDo: Implement conversion for p_s
             PRINT *,'WARNING FROM NEO_MAGFIE: CONVERSION FOR RADIAL DERIVATIVE OF BOOZER-PHI NOT IMPLEMENTED!'
             !! End Modifications by Andreas F. Martitsch (07.03.2014)
          ELSE
             p_tb = p_tb * twopi / nfp
             p_pb = 1.0_dp + p_pb * twopi / nfp
             !! Modifications by Andreas F. Martitsch (11.03.2014)
             ! Conversion factor between Boozer-phi and L (see Boozer-file)
             p_s = p_s * twopi / nfp
             p_stb = p_stb * twopi / nfp
             p_tbtb = p_tbtb * twopi / nfp
             p_pbtb = p_pbtb * twopi / nfp
             p_spb = p_spb * twopi / nfp
             p_pbpb = p_pbpb * twopi / nfp
             !! End Modifications by Andreas F. Martitsch (11.03.2014)
          END IF
          
          ! **********************************************************************
          ! Ensure periodicity boundaries to be the same
          ! **********************************************************************
          r(theta_n,:) = r(1,:)
          r(:,phi_n)   = r(:,1)
          z(theta_n,:) = z(1,:)
          z(:,phi_n)   = z(:,1)
          l(theta_n,:) = l(1,:)
          l(:,phi_n)   = l(:,1)
          bmod_a(theta_n,:) = bmod_a(1,:)
          bmod_a(:,phi_n)   = bmod_a(:,1)
          r_tb(theta_n,:) = r_tb(1,:)
          r_tb(:,phi_n)   = r_tb(:,1)
          r_pb(theta_n,:) = r_pb(1,:)
          r_pb(:,phi_n)   = r_pb(:,1)
          z_tb(theta_n,:) = z_tb(1,:)
          z_tb(:,phi_n)   = z_tb(:,1)
          z_pb(theta_n,:) = z_pb(1,:)
          z_pb(:,phi_n)   = z_pb(:,1)
          p_tb(theta_n,:) = p_tb(1,:)
          p_tb(:,phi_n)   = p_tb(:,1)
          p_pb(theta_n,:) = p_pb(1,:)
          p_pb(:,phi_n)   = p_pb(:,1)
          !! Modifications by Andreas F. Martitsch (07.03.2014)
          ! Temporary storage arrays for the Fourier summations
          ! related to the radial derivatives of (R,Z,phi)-components
          r_s(theta_n,:) = r_s(1,:)
          r_s(:,phi_n)   = r_s(:,1)
          z_s(theta_n,:) = z_s(1,:)
          z_s(:,phi_n)   = z_s(:,1)
          p_s(theta_n,:) = p_s(1,:)
          p_s(:,phi_n)   = p_s(:,1)
          r_stb(theta_n,:) = r_stb(1,:)
          r_stb(:,phi_n)   = r_stb(:,1)
          z_stb(theta_n,:) = z_stb(1,:)
          z_stb(:,phi_n)   = z_stb(:,1)
          p_stb(theta_n,:) = p_stb(1,:)
          p_stb(:,phi_n)   = p_stb(:,1)
          r_tbtb(theta_n,:) = r_tbtb(1,:)
          r_tbtb(:,phi_n)   = r_tbtb(:,1)
          z_tbtb(theta_n,:) = z_tbtb(1,:)
          z_tbtb(:,phi_n)   = z_tbtb(:,1)
          p_tbtb(theta_n,:) = p_tbtb(1,:)
          p_tbtb(:,phi_n)   = p_tbtb(:,1)
          r_pbtb(theta_n,:) = r_pbtb(1,:)
          r_pbtb(:,phi_n)   = r_pbtb(:,1)
          z_pbtb(theta_n,:) = z_pbtb(1,:)
          z_pbtb(:,phi_n)   = z_pbtb(:,1)
          p_pbtb(theta_n,:) = p_pbtb(1,:)
          p_pbtb(:,phi_n)   = p_pbtb(:,1)
          r_spb(theta_n,:) = r_spb(1,:)
          r_spb(:,phi_n)   = r_spb(:,1)
          z_spb(theta_n,:) = z_spb(1,:)
          z_spb(:,phi_n)   = z_spb(:,1)
          p_spb(theta_n,:) = p_spb(1,:)
          p_spb(:,phi_n)   = p_spb(:,1)
          r_pbpb(theta_n,:) = r_pbpb(1,:)
          r_pbpb(:,phi_n)   = r_pbpb(:,1)
          z_pbpb(theta_n,:) = z_pbpb(1,:)
          z_pbpb(:,phi_n)   = z_pbpb(:,1)
          p_pbpb(theta_n,:) = p_pbpb(1,:)
          p_pbpb(:,phi_n)   = p_pbpb(:,1)
          !! End Modifications by Andreas F. Martitsch (07.03.2014)
          bb_tb_a(theta_n,:) = bb_tb_a(1,:)
          bb_tb_a(:,phi_n)   = bb_tb_a(:,1)
          !! Modifications by Andreas F. Martitsch (25.08.2014)
          ! This seems to be a copy-paste, which moved one version
          ! to another:
          !bb_s_a(theta_n,:)  = bb_tb_a(1,:)
          !bb_s_a(:,phi_n)    = bb_tb_a(:,1)
          ! this should be correct now:
          bb_s_a(theta_n,:)  = bb_s_a(1,:)
          bb_s_a(:,phi_n)    = bb_s_a(:,1)
          !! End Modifications by Andreas F. Martitsch (25.08.2014)
          bb_pb_a(theta_n,:) = bb_pb_a(1,:)
          bb_pb_a(:,phi_n)   = bb_pb_a(:,1)
          
          ! **********************************************************************
          ! Derived quantities
          ! **********************************************************************
          ALLOCATE( gtbtb(theta_n,phi_n) ) 
          ALLOCATE( gpbpb(theta_n,phi_n) ) 
          ALLOCATE( gtbpb(theta_n,phi_n) ) 
          ALLOCATE( sqrg11_met(theta_n,phi_n) )
          !! Modifications by Andreas F. Martitsch (11.03.2014)
          ! Allocate temporary storage arrays for the Fourier summations
          ! related to the additionally needed metric tensor elements
          ALLOCATE( gstb_a(theta_n,phi_n) )
          ALLOCATE( gspb_a(theta_n,phi_n) )
          ALLOCATE( gstb_tb_a(theta_n,phi_n) )
          ALLOCATE( gspb_tb_a(theta_n,phi_n) )
          ALLOCATE( gstb_pb_a(theta_n,phi_n) )
          ALLOCATE( gspb_pb_a(theta_n,phi_n) )
          !! End Modifications by Andreas F. Martitsch (11.03.2014)
          ! metric tensor
          gtbtb = r_tb*r_tb + z_tb*z_tb + r*r*p_tb*p_tb  
          gpbpb = r_pb*r_pb + z_pb*z_pb + r*r*p_pb*p_pb  
          gtbpb = r_tb*r_pb + z_tb*z_pb + r*r*p_tb*p_pb
          !! Modifications by Andreas F. Martitsch (11.03.2014)
          ! Compute the additionally needed metric tensor elements
          gstb_a = r_s*r_tb + z_s*z_tb + r*r*p_s*p_tb  
          gspb_a = r_s*r_pb + z_s*z_pb + r*r*p_s*p_pb
          gstb_tb_a = r_stb*r_tb + r_s*r_tbtb  + z_stb*z_tb + z_s*z_tbtb + &
               2.0d0*r*r_tb*p_s*p_tb + r*r*(p_stb*p_tb + p_s*p_tbtb)
          gspb_tb_a = r_stb*r_pb + r_s*r_pbtb  + z_stb*z_pb + z_s*z_pbtb + &
               2.0d0*r*r_tb*p_s*p_pb + r*r*(p_stb*p_pb + p_s*p_pbtb)
          gstb_pb_a = r_spb*r_tb + r_s*r_pbtb  + z_spb*z_tb + z_s*z_pbtb + &
               2.0d0*r*r_pb*p_s*p_tb + r*r*(p_spb*p_tb + p_s*p_pbtb)
          gspb_pb_a = r_spb*r_pb + r_s*r_pbpb  + z_spb*z_pb + z_s*z_pbpb + &
               2.0d0*r*r_pb*p_s*p_pb + r*r*(p_spb*p_pb + p_s*p_pbpb)
          !! End Modifications by Andreas F. Martitsch (11.03.2014)

          ! Winny for Klaus
          av_b2_m = theta_n * phi_n / SUM(1 / (bmod_a*bmod_a))
          !PRINT *, 'theta_n,phi_n ',theta_n,phi_n 
          !PRINT *, 'av_b2_m ',av_b2_m
          ! Winny for Klaus - Ende

  
          ! $1/sqrt(g)$
          !! fac = s_curr_pol + s_iota * s_curr_tor  ! (J + iota I)
          !! isqrg  = b*b / fac 
          ! $sqrt(g^{11})$
          !IF (g11_swi .EQ. 0) THEN
          !   sqrg11 = 1.0_dp
          !ELSE
             !sqrg11 = SQRT( (gtbtb*gpbpb - gtbpb*gtbpb ) * isqrg**2)
             sqrg11_met = SQRT( (gtbtb*gpbpb - gtbpb*gtbpb ) )
          !END IF

             !PRINT *, 'max_gtbtb = ',maxval(gtbtb)
             !PRINT *, 'min_gtbtb = ',MINVAL(gtbtb)
             !PRINT *, 'max_gpbpb = ',maxval(gpbpb)
             !PRINT *, 'min_gpbpb = ',MINVAL(gpbpb)
             !PRINT *, 'max_gtbpb = ',MAXVAL(gtbpb)
             !PRINT *, 'min_gtbpb = ',MINVAL(gtbpb)
            
          !*************************************************************
          ! Do the 2-D periodic spline
          !*************************************************************
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Do 2-D spline of bmod'
          END IF
          p_spl => bmod_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               bmod_a,p_spl)
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Do 2-D spline of bb_s'
          END IF
          p_spl => bb_s_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               bb_s_a,p_spl)
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Do 2-D spline of bb_tb'
          END IF
          p_spl => bb_tb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               bb_tb_a,p_spl)
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Do 2-D spline of bb_pb'
          END IF
          p_spl => bb_pb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               bb_pb_a,p_spl)
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Do 2-D spline of sqrg11'
          END IF
          p_spl => gval_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               sqrg11_met,p_spl)
          !! Modifications by Andreas F. Martitsch (11.03.2014)
          ! Compute the 2d periodic splines (over the flux-surface)
          ! of the additionally needed metric tensor elements
          p_spl => gstb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               gstb_a,p_spl)
          p_spl => gspb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               gspb_a,p_spl)
          p_spl => gstb_tb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               gstb_tb_a,p_spl)
          p_spl => gspb_tb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               gspb_tb_a,p_spl)
          p_spl => gstb_pb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               gstb_pb_a,p_spl)
          p_spl => gspb_pb_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               gspb_pb_a,p_spl)
          !! End Modifications by Andreas F. Martitsch (11.03.2014)
          !! Modifications by Andreas F. Martitsch (13.11.2014)
          ! Compute the 2d periodic splines (over the flux-surface)
          ! of the additionally needed quantities for NTV output
          p_spl => R_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               r,p_spl)
          p_spl => Z_spl(:,:,:,:,k_es)
          CALL spl2d(theta_n,phi_n,theta_int,phi_int,mt,mp,            &
               z,p_spl)
          !! End Modifications by Andreas F. Martitsch (13.11.2014)
          
          !PRINT *, 'max_g11 = ', maxval(sqrg11_met)
          !PRINT *, 'min_g11 = ', minval(sqrg11_met)

          !stop "Compute magnetic field components via a spline (and not via direct Fourier summation)"

          DEALLOCATE( bmod_a )
          DEALLOCATE( bb_s_a )
          DEALLOCATE( bb_tb_a )
          DEALLOCATE( bb_pb_a )

          DEALLOCATE( r )  ! NEW
          DEALLOCATE( z ) 
          DEALLOCATE( l ) 
          DEALLOCATE( r_tb ) 
          DEALLOCATE( z_tb ) 
          DEALLOCATE( p_tb ) 
          DEALLOCATE( r_pb ) 
          DEALLOCATE( z_pb ) 
          DEALLOCATE( p_pb ) 

          DEALLOCATE( gtbtb ) 
          DEALLOCATE( gpbpb ) 
          DEALLOCATE( gtbpb ) 
          DEALLOCATE( sqrg11_met )

          !! Modifications by Andreas F. Martitsch (11.03.2014)
          ! Deallocate temporary storage arrays for the Fourier summations
          ! related to the radial derivatives of (R,Z,phi)-components
          ! and the additionally needed metric tensor elements
          DEALLOCATE( r_s )
          DEALLOCATE( z_s )
          DEALLOCATE( p_s )
          DEALLOCATE( r_stb )
          DEALLOCATE( z_stb )
          DEALLOCATE( p_stb )
          DEALLOCATE( r_tbtb ) 
          DEALLOCATE( z_tbtb ) 
          DEALLOCATE( p_tbtb ) 
          DEALLOCATE( r_pbtb ) 
          DEALLOCATE( z_pbtb ) 
          DEALLOCATE( p_pbtb )
          DEALLOCATE( gstb_a ) 
          DEALLOCATE( gspb_a ) 
          DEALLOCATE( gstb_tb_a ) 
          DEALLOCATE( gspb_tb_a )
          DEALLOCATE( r_spb )
          DEALLOCATE( z_spb )
          DEALLOCATE( p_spb )
          DEALLOCATE( r_pbpb ) 
          DEALLOCATE( z_pbpb ) 
          DEALLOCATE( p_pbpb )
          DEALLOCATE( gstb_pb_a ) 
          DEALLOCATE( gspb_pb_a )
          !! End Modifications by Andreas F. Martitsch (11.03.2014)

          !*************************************************************
          ! Provide curr_tor, curr_tor_s, curr_pol, curr_pol_s, iota
          !*************************************************************
          IF (write_progress .EQ. 1) THEN
             PRINT *, 'Prep of currents: ',s
          END IF
          swd = 1 ! derivative
          CALL splint_horner3(es,                                      &
               a_curr_tor, b_curr_tor, c_curr_tor, d_curr_tor,         &
               swd, m0,                                                &
               s, tfone, tfzero, tfzero, tfzero,                       &
               curr_tor_array(k_es), curr_tor_s_array(k_es), ypp, yppp)
          swd = 1 ! derivative
          CALL splint_horner3(es,                                      &
               a_curr_pol, b_curr_pol, c_curr_pol, d_curr_pol,         &
               swd, m0,                                                &
               s, tfone, tfzero, tfzero, tfzero,                       &
               curr_pol_array(k_es), curr_pol_s_array(k_es) ,ypp, yppp)    
          swd = 0 ! no derivative
          CALL splint_horner3(es,                                      &
               a_iota, b_iota, c_iota, d_iota, swd, m0,                &
               s, tfone, tfzero, tfzero, tfzero,                       &
               iota_array(k_es), yp, ypp, yppp)
          CALL splint_horner3(es,                                      &
               a_pprime, b_pprime, c_pprime, d_pprime, swd, m0,                &
               s, tfone, tfzero, tfzero, tfzero,                       &
               pprime_array(k_es), yp, ypp, yppp)
          CALL splint_horner3(es,                                      &
               a_sqrtg00, b_sqrtg00, c_sqrtg00, d_sqrtg00, swd, m0,                &
               s, tfone, tfzero, tfzero, tfzero,                       &
               sqrtg00_array(k_es), yp, ypp, yppp)
       END DO
       magfie_newspline = 0
    END IF

    s_detected = 0
    IF (magfie_spline .EQ. 1) THEN
       s = x(1)
       !****************************************************************
       ! Detection of index
       !****************************************************************
       DO k_es = 1, magfie_sarray_len
          IF ( ABS(s-magfie_sarray(k_es)) .LT. magfie_epsi) THEN
             s_detected = 1
             EXIT
          END IF
       END DO
       IF (s_detected .EQ. 1) THEN
          !PRINT *,magfie_sarray(k_es)
          !STOP "flux surface s detected"
          curr_tor   = curr_tor_array(k_es)
          curr_tor_s = curr_tor_s_array(k_es)
          curr_pol   = curr_pol_array(k_es)
          curr_pol_s = curr_pol_s_array(k_es)
          iota       = iota_array(k_es)
          s_pprime   = pprime_array(k_es) ! only local
          s_sqrtg00  = sqrtg00_array(k_es)
          ! ************************************************************
          ! Evaluation of 2d-splines
          ! ************************************************************
          CALL poi2d(theta_int,phi_int,mt,mp,                          &
               theta_start,theta_end,phi_start,phi_end,                &
               x(3),x(2),theta_ind,phi_ind,theta_d,phi_d,ierr)
          p_spl => bmod_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,bmod)
          p_spl => bb_s_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,bb_s)
          p_spl => bb_tb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,bb_tb)
          p_spl => bb_pb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,bb_pb)
          p_spl => gval_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,sqrg11)
          !! Modifications by Andreas F. Martitsch (11.03.2014)
          ! Evaluate the 2d periodic splines (over the flux-surface)
          ! of the additionally needed metric tensor elements
          !PRINT *,'x: ',x(1),x(2),x(3)
          p_spl => gstb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,gstb)
          !PRINT *,'gstb: ', gstb
          p_spl => gspb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,gspb)
          !PRINT *,'gspb: ', gspb
          p_spl => gstb_tb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,gstb_tb)
          !PRINT *,'gstb_tb: ', gstb_tb
          p_spl => gspb_tb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,gspb_tb)
          !PRINT *,'gspb_tb: ', gspb_tb
          p_spl => gstb_pb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,gstb_pb)
          !PRINT *,'gstb_pb: ', gstb_pb
          p_spl => gspb_pb_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,gspb_pb)
          !PRINT *,'gspb_pb: ', gspb_pb
          !! End Modifications by Andreas F. Martitsch (11.03.2014)
          !! Modifications by Andreas F. Martitsch (13.11.2014)
          ! Evaluate the 2d periodic splines (over the flux-surface)
          ! of the additionally needed quantities for NTV output
          p_spl => R_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,r_val)
          p_spl => Z_spl(:,:,:,:,k_es)
          CALL eva2d(theta_n,phi_n,theta_ind,phi_ind,theta_d,phi_d,    &
               p_spl,z_val)
          !! End Modifications by Andreas F. Martitsch (13.11.2014)
          
          ! $1/sqrt(g)$
          fac = curr_pol + iota * curr_tor  ! (J + iota I)
          isqrg  = bmod*bmod / fac

          ! Winny for Klaus
          !s_sqrtg00_m = fac / av_b2_m
          s_sqrtg00 = fac / av_b2_m
          !PRINT *, 's_sqrtg00, s_sqrtg00_m ',s_sqrtg00, s_sqrtg00_m
          !PRINT *, 'fac, av_b2_m ',fac, av_b2_m
          !PAUSE
          ! Winny for Klaus - Ende

          !PRINT *, ' '
          !PRINT *, 'curr_pol = ',curr_pol
          !PRINT *, 'curr_tor = ',curr_tor
          !PRINT *, 'iota     = ',iota
          !PRINT *, 'fac      = ',fac
          !PRINT *, 'bmod     = ',bmod
          !PRINT *, 'isqrg    = ',isqrg
          !PRINT *, 'sqrg     = ',1.d0 / isqrg

          !PRINT *, 'sqrg11_n = ',sqrg11
          sqrg11 = sqrg11 * ABS(isqrg)
          !PRINT *, 'sqrg11   = ',sqrg11

          !! Modifications by Andreas F. Martitsch (11.03.2014)
          ! Compute the values of the additionally needed 
          ! B-field components
          bcovar_s = isqrg*(gstb*iota+gspb)
          dbcovar_s_dtheta = (2.0d0*bmod*bb_tb/fac)*(gstb*iota+gspb) + &
               isqrg*(gstb_tb*iota+gspb_tb)
          !PRINT *,'dbcovar_s_dtheta: ', dbcovar_s_dtheta
          dbcovar_s_dphi = (2.0d0*bmod*bb_pb/fac)*(gstb*iota+gspb) + &
               isqrg*(gstb_pb*iota+gspb_pb)
          !PRINT *, 'dbcovar_s_dphi: ', dbcovar_s_dphi
          !STOP 
          !! End Modifications by Andreas F. Martitsch (11.03.2014)
       ELSE
          PRINT *, 'neo_magfie: s not detected!'
          STOP
       END IF
    END IF

    IF (magfie_spline .EQ. 0 .OR. s_detected .EQ. 0) THEN
       IF (magfie_spline .EQ. 1 .AND. s_detected .EQ. 0) THEN
          PRINT *, 'WARNING from neo_magfie - s out of range: ',s
          PRINT *, ' Using Fourier Summation directly'
       END IF
       
       PRINT *, 'magfie_spline .EQ. 0 not implemented'
       STOP

       !****************************************************************
       ! Direct summation of Fourier components
       !****************************************************************
       bmod   = 0.0_dp
       bb_s   = 0.0_dp
       bb_tb  = 0.0_dp
       bb_pb  = 0.0_dp

       !stop "Compute magnetic field components via direct Fourier summation"

       DO i = 1, mnmax
          swd = 1
          CALL splint_horner3(es,                                    &
               a_bmnc(:,i), b_bmnc(:,i), c_bmnc(:,i), d_bmnc(:,i),   &
               swd, r_mhalf(i),                                      &
               x(1), tf, tfp, tfpp, tfppp,                           &
               bmnc, bmnc_s, ypp, yppp)

          m = ixm(i)
          n = ixn(i)
          sinv = SIN(m*x(3) - n*x(2))
          cosv = COS(m*x(3) - n*x(2))
          
          bmod   = bmod   +     bmnc   * cosv
          bb_s   = bb_s   +     bmnc_s * cosv
          bb_tb  = bb_tb  - m * bmnc   * sinv
          bb_pb  = bb_pb  + n * bmnc   * sinv
       END DO

       swd = 1
       CALL splint_horner3(es,                                       &
            a_curr_tor, b_curr_tor, c_curr_tor, d_curr_tor,          &
            swd, m0,                                                 &
            x(1), tfone, tfzero, tfzero, tfzero,                     &
            curr_tor, curr_tor_s, ypp, yppp)
       CALL splint_horner3(es,                                       &
            a_curr_pol, b_curr_pol, c_curr_pol, d_curr_pol,          &
            swd, m0,                                                 &
            x(1), tfone, tfzero, tfzero, tfzero,                     &
            curr_pol, curr_pol_s ,ypp, yppp)    
       swd = 0 ! no derivative
       CALL splint_horner3(es,                                       &
            a_iota, b_iota, c_iota, d_iota, swd, m0,                 &
            x(1), tfone, tfzero, tfzero, tfzero,                     &
            iota, yp, ypp, yppp)       
    END IF

    IF (magfie_result .EQ. 1) THEN
       ! This was the original version:     
       ! derived quantities in (s,theta_b,phi_b)-system
       fac   = (curr_pol + iota * curr_tor) * psi_pr
       fac1  = fac  / bmod                 ! sqrtg*bmod
       sqrtg = fac1 / bmod 

       bder(1) = bb_s
       bder(2) = bb_tb
       bder(3) = bb_pb

       hcovar(1) = 0.0_dp
       hcovar(2) = curr_tor / bmod
       hcovar(3) = curr_pol / bmod

       hctrvr(1) = 0.0_dp
       hctrvr(2) = iota / fac1
       hctrvr(3) = 1.0_dp / fac1

       hcurl(1)  = (curr_pol * bb_pb      - curr_tor * bb_tb     ) / fac 
       hcurl(2)  = (curr_pol * bb_s       - bmod     * curr_pol_s) / fac 
       hcurl(3)  = (bmod     * curr_tor_s - curr_tor * bb_s      ) / fac 
       ! Remark by Winny:
       ! The consisteny check for curr_pol shows a problem in all
       ! Greifswald (standard) input files
       ! According to the consistency check, 
       ! curr_pol has to be changed to -curr_pol

    ELSEIF ( magfie_result .EQ. 0 ) THEN
       ! Modifications made by Sergie for use in SMT
       ! derived quantities in (s,theta_b,phi_b)-system
       !fac   = (curr_pol + iota * curr_tor) * psi_pr
       ! This is used in NEO2
       fac   =  curr_pol + iota * curr_tor                       !!!
       fac1  = fac  / bmod                 ! sqrtg*bmod
       fac = fac * psi_pr                                        !!!
       !    sqrtg = fac1 / bmod 
       sqrtg = - fac1 / bmod * psi_pr * 1d6                      !!!
       !---------------------------------------------------------------------------
       !  iota_m = iota
       ! fac_m  =  (curr_pol + iota * curr_tor) * 1d6 * psi_pr
       !  fac_c  =  (curr_pol + iota * curr_tor) * 1d6 
       !---------------------------------------------------------------------------

       bder(1) = bb_s
       bder(3) = bb_tb
       bder(2) = bb_pb
       bder=bder / bmod                                          !!!
       
       !! Modifications by Andreas F. Martitsch (07.03.2014)
       ! Radial covariant B-field component is now available
       hcovar(1) = bcovar_s / bmod
       !! End Modifications by Andreas F. Martitsch (07.03.2014)
       hcovar(3) = curr_tor / bmod
       hcovar(2) = curr_pol / bmod
       hcovar=hcovar * 1.d2                                      !!!

       hctrvr(1) = 0.0_dp
       hctrvr(3) = iota / fac1
       hctrvr(2) = 1.0_dp / fac1
       hctrvr=hctrvr * 1d-2                                      !!!

       !    hcurl(1)  = (curr_pol * bb_pb      - curr_tor * bb_tb     ) / fac 
       hcurl(1)  = (curr_tor * bb_pb      - curr_pol * bb_tb     ) / fac  !!!
       hcurl(3)  = (curr_pol * bb_s       - bmod     * curr_pol_s) / fac 
       hcurl(2)  = (bmod     * curr_tor_s - curr_tor * bb_s      ) / fac 
       hcurl=hcurl * 1d-4                                                 !!!

       !! Modifications by Andreas F. Martitsch (12.03.2014)
       ! boozer_curr_tor, boozer_curr_pol, boozer_psi_pr,
       ! boozer_sqrtg11 and boozer_isqrg are now converted
       ! to cgs-units.
       ! This step requires changes within rhs_kin.f90 and
       ! ripple_solver.f90!
       !PRINT *,bmod0
       boozer_curr_tor_hat = (curr_tor/bmod0)*1.0d2
       boozer_curr_pol_hat = (curr_pol/bmod0)*1.0d2
       boozer_curr_pol_hat_s = (curr_tor_s/bmod0)*1.0d2
       boozer_curr_tor_hat_s = (curr_pol_s/bmod0)*1.0d2
       boozer_psi_pr_hat = (psi_pr/bmod0)*1.0d4
       boozer_sqrtg11 = (1.0d0/psi_pr)*sqrg11*1.0d-2
       boozer_isqrg = (1.0d0/psi_pr)*isqrg*1.0d-6
       !! End Modifications by Andreas F. Martitsch (12.03.2014)

    END IF
    
    boozer_iota = iota
    ! CAUTION: This quantity is only used by Klaus.
    ! Conversion from SI- to cgs-units has not yet been
    ! checked for this quantity
    boozer_sqrtg00 = s_sqrtg00
!!$    !! Modifications by Andreas F. Martitsch (12.03.2014)
!!$    ! Note! Every quantity given below is written in
!!$    ! terms of SI-units. This part has been moved to the
!!$    ! block with "magfie_result eq. 0), where the quantities are
!!$    ! converted to cgs-units. In the previous version
!!$    ! conversions were done partly within rhs_kin.f90 and
!!$    ! partly within ripple_solver.f90.
!!$    ! For this reason, please note also the changes in
!!$    ! "rhs_kin" and "ripple_solver"!
!!$    boozer_curr_tor = curr_tor
!!$    boozer_curr_pol = curr_pol
!!$    ! Compute the radial derivatives of toroidal / poloidal currents
!!$    ! (In fact these currents are already the respective
!!$    ! covariant B-field components; conversion done within
!!$    ! neo_read)
!!$    ! Caution! boozer_curr_tor/ boozer_curr_pol are given in SI-units [Tm].
!!$    ! They are not converted to cgs-units - as done within (magfie_result .EQ. 0).
!!$    boozer_curr_pol_s = curr_tor_s
!!$    boozer_curr_tor_s = curr_pol_s
!!$    boozer_psi_pr = psi_pr 
!!$    boozer_sqrtg11 = sqrg11
!!$    boozer_isqrg = isqrg
!!$    !! End Modifications by Andreas F. Martitsch (12.03.2014)
  END SUBROUTINE neo_magfie_a
  
  !! Modifications by Andreas F. Martitsch (11.03.2014)
  ! Optional output (necessary for modeling the magnetic rotation)
  SUBROUTINE neo_magfie_b( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl, bcovar_s_hat_der )
    ! input / output
    REAL(dp), DIMENSION(:),       INTENT(in)  :: x
    REAL(dp),                     INTENT(out) :: bmod
    REAL(dp),                     INTENT(out) :: sqrtg
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: bder
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: hcovar
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: hctrvr
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: hcurl
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: bcovar_s_hat_der
    !
    CALL neo_magfie_a( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl )
    !
    ! Compute the derivatives of the radial covariant 
    ! B-field component (Note: cgs-units used)
    bcovar_s_hat_der(1) = 0.0_dp ! not implemented at the moment
    bcovar_s_hat_der(3) = dbcovar_s_dtheta / bmod0
    bcovar_s_hat_der(2) = dbcovar_s_dphi / bmod0
    bcovar_s_hat_der=bcovar_s_hat_der * 1.d2 ! conversion to cgs-units
    !
  END SUBROUTINE neo_magfie_b
  !! End Modifications by Andreas F. Martitsch (11.03.2014)

  !! Modifications by Andreas F. Martitsch (13.11.2014)
  ! Optional output for NTV output
  SUBROUTINE neo_magfie_c( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl, bcovar_s_hat_der, R, Z )
    ! input / output
    REAL(dp), DIMENSION(:),       INTENT(in)  :: x
    REAL(dp),                     INTENT(out) :: bmod
    REAL(dp),                     INTENT(out) :: sqrtg
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: bder
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: hcovar
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: hctrvr
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: hcurl
    REAL(dp), DIMENSION(SIZE(x)), INTENT(out) :: bcovar_s_hat_der
    REAL(dp),                     INTENT(out) :: R
    REAL(dp),                     INTENT(out) :: Z
    !
    CALL neo_magfie_b( x, bmod, sqrtg, bder, hcovar, hctrvr, hcurl, bcovar_s_hat_der )
    !
    R = r_val * 1.d2 ! conversion to cgs-units
    Z = z_val * 1.d2 ! conversion to cgs-units
    !
  END SUBROUTINE neo_magfie_c
  !! End Modifications by Andreas F. Martitsch (13.11.2014)

END MODULE neo_magfie_mod

