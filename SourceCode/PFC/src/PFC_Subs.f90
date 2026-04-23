! Name:   		PFC (Primary Frequency Control) DLL.
! Authors: 		Julius Preuschoff, David Schlipf from Flensburg University of Applied Sciences, funded by ABBA -- Adaptive operating strategies for existing wind turbines.   
! Target: 		This code aims to provide a simple case of primary frequency control in conjuction with lidar assisted control for the community.   
! Function: 	The DLL chain is designed to make the PFC processing and other algorithms more independent from the feedback controller and modified OpenFAST versions. 
! Reference:	The subroutines rely on the legacy Bladed style data interface. See the Bladed manual for more detail.    
! 				The code is written based on the source code of the WRAPPER. https://github.com/MSCA-LIKE/Baseline-Lidar-assisted-Controller, 2022.
! License: 		MIT License
! Copyright (c) 2026 Flensburg University of Applied Sciences, WETI
! -------------------------------------------------------------------------------------------
      
!=======================================================================
MODULE PFC_Subs
  USE, INTRINSIC :: ISO_C_BINDING
  USE PFC_Helpers
  IMPLICIT NONE

CONTAINS

  SUBROUTINE parse_PFC_infile(pfile, inIdx, outIdx, H, S, f0, omegaRated, ierr, err)
    CHARACTER(*), INTENT(IN)  :: pfile
    INTEGER,      INTENT(OUT) :: inIdx
    INTEGER,      INTENT(OUT) :: outIdx
    REAL(8),      INTENT(OUT) :: H, S, f0, omegaRated
    INTEGER,      INTENT(OUT) :: ierr
    CHARACTER(*), INTENT(OUT) :: err

    INTEGER :: u, ios
    CHARACTER(512) :: line
    CHARACTER(:), ALLOCATABLE :: key, val
    LOGICAL :: haveIn, haveOut

    ierr = 0
    err  = ''

    inIdx      = -1
    outIdx     = -1
    H          = 0.0D0
    S          = 0.0D0
    f0         = 0.0D0
    omegaRated = 0.0D0

    haveIn  = .FALSE.
    haveOut = .FALSE.

    OPEN(NEWUNIT=u, FILE=TRIM(pfile), STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
      ierr = 1
      err  = 'Could not open parameter file: '//TRIM(pfile)
      RETURN
    END IF

    DO
      READ(u,'(A)',IOSTAT=ios) line
      IF (ios /= 0) EXIT

      CALL strip_comment_and_trim(line)
      IF (LEN_TRIM(line) == 0) CYCLE
      IF (.NOT. split_key_value(line, key, val)) CYCLE

      CALL to_lower_inplace(key)

      SELECT CASE (TRIM(key))
      CASE ('inputswapindex')
        READ(val, *, IOSTAT=ios) inIdx
        IF (ios /= 0 .OR. inIdx < 1) THEN
          ierr = 2
          err  = 'InputSwapIndex must be an integer >= 1.'
          CLOSE(u)
          RETURN
        END IF
        haveIn = .TRUE.

      CASE ('outputswapindex')
        READ(val, *, IOSTAT=ios) outIdx
        IF (ios /= 0 .OR. outIdx < 1) THEN
          ierr = 3
          err  = 'OutputSwapIndex must be an integer >= 1.'
          CLOSE(u)
          RETURN
        END IF
        haveOut = .TRUE.

      CASE ('h')
        READ(val, *, IOSTAT=ios) H
        IF (ios /= 0) THEN
          ierr = 8
          err  = 'H must be a valid real number.'
          CLOSE(u)
          RETURN
        END IF

      CASE ('s')
        READ(val, *, IOSTAT=ios) S
        IF (ios /= 0) THEN
          ierr = 9
          err  = 'S must be a valid real number.'
          CLOSE(u)
          RETURN
        END IF

      CASE ('f0')
        READ(val, *, IOSTAT=ios) f0
        IF (ios /= 0) THEN
          ierr = 10
          err  = 'f0 must be a valid real number.'
          CLOSE(u)
          RETURN
        END IF

      CASE ('omegarated')
        READ(val, *, IOSTAT=ios) omegaRated
        IF (ios /= 0) THEN
          ierr = 11
          err  = 'OmegaRated must be a valid real number.'
          CLOSE(u)
          RETURN
        END IF

      CASE DEFAULT
        ! ignore unknown keys
      END SELECT
    END DO

    CLOSE(u)

    IF (.NOT. haveIn) THEN
      ierr = 12
      err  = 'Missing required key: InputSwapIndex'
      RETURN
    END IF

    IF (.NOT. haveOut) THEN
      ierr = 13
      err  = 'Missing required key: OutputSwapIndex'
      RETURN
    END IF

  END SUBROUTINE parse_PFC_infile

!-------------------------------------------------------------------------------
  SUBROUTINE ensure_sorted_by_time(t, v, n, ierr, err)
    REAL(8), INTENT(IN) :: t(:), v(:)
    INTEGER, INTENT(IN) :: n
    INTEGER, INTENT(OUT) :: ierr
    CHARACTER(*), INTENT(OUT) :: err
    INTEGER :: i

    ierr = 0
    err  = ''

    DO i = 2, n
      IF (t(i) < t(i-1)) THEN
        ierr = 10
        err  = 'CSV timestamps must be nondecreasing. Sort by time.'
        RETURN
      END IF
    END DO
  END SUBROUTINE ensure_sorted_by_time

!-------------------------------------------------------------------------------
SUBROUTINE compute_delta_MgFF(x, dxdt, H, S, f0, omegaRated, delta_MgFF)
    REAL(8), INTENT(IN)  :: x, dxdt
    REAL(8), INTENT(IN)  :: H, S, f0, omegaRated
    REAL(8), INTENT(OUT) :: delta_MgFF

    REAL(8) :: T, delta_P

    T = -2.0D0 * S * H / f0
    delta_P = T * dxdt
    delta_MgFF = delta_P * (1.0D0 / omegaRated)
END SUBROUTINE compute_delta_MgFF

!-------------------------------------------------------------------------------
SUBROUTINE differentiate_signal(t_now, x_now, first_step, t_prev, x_prev, dxdt)
  REAL(8), INTENT(IN)    :: t_now, x_now
  LOGICAL, INTENT(INOUT) :: first_step
  REAL(8), INTENT(INOUT) :: t_prev, x_prev
  REAL(8), INTENT(OUT)   :: dxdt

  REAL(8) :: dt

  IF (first_step) THEN
    dxdt = 0.0D0
    first_step = .FALSE.
    t_prev = t_now
    x_prev = x_now
    RETURN
  END IF

  dt = t_now - t_prev

  IF (dt <= 0.0D0) THEN
    dxdt = 0.0D0
    RETURN
  END IF

  dxdt = (x_now - x_prev) / dt

  t_prev = t_now
  x_prev = x_now
END SUBROUTINE differentiate_signal

END MODULE PFC_Subs