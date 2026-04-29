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

  SUBROUTINE parse_PFC_infile(pfile, outIdx, H, S, f0, omegaRated, avgWindowSize, ierr, err)
    CHARACTER(*), INTENT(IN)  :: pfile
    INTEGER,      INTENT(OUT) :: outIdx
    INTEGER,      INTENT(OUT) :: avgWindowSize
    REAL(8),      INTENT(OUT) :: H, S, f0, omegaRated
    INTEGER,      INTENT(OUT) :: ierr
    CHARACTER(*), INTENT(OUT) :: err

    INTEGER :: u, ios
    CHARACTER(512) :: line
    CHARACTER(:), ALLOCATABLE :: key, val
    LOGICAL :: haveOut

    ierr = 0
    err  = ''

    outIdx          = -1
    H               = 0.0D0
    S               = 0.0D0
    f0              = 0.0D0
    omegaRated      = 0.0D0
    avgWindowSize   = 1

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
        
        CASE ('avgwindowsize')
            READ(val, *, IOSTAT=ios) avgWindowSize
            IF (ios /= 0 .OR. avgWindowSize < 1) THEN
              ierr = 14
              err  = 'AvgWindowSize must be an integer >= 1.'
              CLOSE(u)
              RETURN
            END IF
        
          CASE DEFAULT
            ! ignore unknown keys
      END SELECT
    END DO

    CLOSE(u)

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

!-------------------------------------------------------------------------------

SUBROUTINE moving_average(x_now, windowSize, buffer, count, index, sumVal, x_avg)
  REAL(8), INTENT(IN)    :: x_now
  INTEGER, INTENT(IN)    :: windowSize
  REAL(8), INTENT(INOUT) :: buffer(:)
  INTEGER, INTENT(INOUT) :: count
  INTEGER, INTENT(INOUT) :: index
  REAL(8), INTENT(INOUT) :: sumVal
  REAL(8), INTENT(OUT)   :: x_avg

  IF (windowSize <= 1) THEN
    x_avg = x_now
    RETURN
  END IF

  IF (count < windowSize) THEN
    count = count + 1
    buffer(index) = x_now
    sumVal = sumVal + x_now
  ELSE
    sumVal = sumVal - buffer(index)
    buffer(index) = x_now
    sumVal = sumVal + x_now
  END IF

  x_avg = sumVal / REAL(count, KIND=8)

  index = index + 1
  IF (index > windowSize) index = 1

END SUBROUTINE moving_average

END MODULE PFC_Subs