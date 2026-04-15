! Name:   		SignalProvider DLL.
! Authors: 		Julius Preuschoff, David Schlipf from Flensburg University of Applied Sciences, funded by ABBA -- Adaptive operating strategies for existing wind turbines.   
! Target: 		This code aims to provide a reference Lidar-assisted control package for the community.   
! Function: 	The DLL chain is designed to make the lidar data processing or other algorithms more independent from the feedback controller and modiefied OpenFAST versions. 
! Reference:	The subroutines rely on the legacy Bladed style data interface. See the Bladed manual for more detail.    
! 				The code is written based on the source code of the WRAPPER. https://github.com/MSCA-LIKE/Baseline-Lidar-assisted-Controller, 2022.
! License: 		MIT License
! Copyright (c) 2026 Flensburg University of Applied Sciences, WETI
! -------------------------------------------------------------------------------------------
      
!=======================================================================
MODULE SignalProvider_Subs
  USE, INTRINSIC :: ISO_C_BINDING
  USE SignalProvider_Helpers
  IMPLICIT NONE

CONTAINS

  SUBROUTINE parse_SignalProviderCsv_infile(pfile, csvOut, swapIdx, interp, preview, timeCol, valueCol, ierr, err)
    CHARACTER(*), INTENT(IN)  :: pfile
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: csvOut
    INTEGER,      INTENT(OUT) :: swapIdx
    LOGICAL,      INTENT(OUT) :: interp
    INTEGER,      INTENT(OUT) :: ierr
    CHARACTER(*), INTENT(OUT) :: err
    REAL(8),      INTENT(OUT) :: preview
    INTEGER,      INTENT(OUT) :: timeCol
    INTEGER,      INTENT(OUT) :: valueCol

    INTEGER :: u, ios
    CHARACTER(512) :: line
    CHARACTER(:), ALLOCATABLE :: key, val
    LOGICAL :: haveCsv, haveSwap, haveInterp, havePreview, haveTimeCol, haveValueCol
    CHARACTER(:), ALLOCATABLE :: baseDir, csvRaw

    ierr = 0
    err  = ''
    swapIdx = -1
    preview = 0.0D0
    timeCol  = -1
    valueCol = -1
    interp  = .TRUE.

    haveCsv      = .FALSE.
    haveSwap     = .FALSE.
    haveInterp   = .FALSE.
    havePreview  = .FALSE.
    haveTimeCol  = .FALSE.
    haveValueCol = .FALSE.

    baseDir = dirname_of_path(TRIM(pfile))

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
      CASE ('csvfilepath')
        csvRaw = TRIM(val)
        haveCsv = .TRUE.

      CASE ('swapindex')
        READ(val, *, IOSTAT=ios) swapIdx
        IF (ios /= 0) THEN
          ierr = 2
          err  = 'SwapIndex is not a valid integer.'
          CLOSE(u)
          RETURN
        END IF
        haveSwap = .TRUE.

      CASE ('interpolatebetweenvalues')
        interp = parse_bool(val, ios)
        IF (ios /= 0) THEN
          ierr = 3
          err  = 'InterpolateBetweenValues must be true/false.'
          CLOSE(u)
          RETURN
        END IF
        haveInterp = .TRUE.

      CASE ('previewtime')
        READ(val, *, IOSTAT=ios) preview
        IF (ios /= 0) THEN
          ierr = 6
          err  = 'PreviewTime is not a valid real number.'
          CLOSE(u)
          RETURN
        END IF
        havePreview = .TRUE.

      CASE ('timecolumnindex')
        READ(val, *, IOSTAT=ios) timeCol
        IF (ios /= 0 .OR. timeCol < 1) THEN
          ierr = 7
          err  = 'TimeColumn must be an integer >= 1.'
          CLOSE(u)
          RETURN
        END IF
        haveTimeCol = .TRUE.

      CASE ('valuecolumnindex')
        READ(val, *, IOSTAT=ios) valueCol
        IF (ios /= 0 .OR. valueCol < 1) THEN
          ierr = 8
          err  = 'ValueColumn must be an integer >= 1.'
          CLOSE(u)
          RETURN
        END IF
        haveValueCol = .TRUE.

      CASE DEFAULT
        ! ignore unknown keys
      END SELECT
    END DO

    CLOSE(u)

    IF (.NOT. haveCsv) THEN
      ierr = 4
      err  = 'Missing required key: CsvFilePath'
      RETURN
    END IF

    IF (.NOT. haveSwap) THEN
      ierr = 5
      err  = 'Missing required key: SwapIndex'
      RETURN
    END IF

    IF (.NOT. haveTimeCol) THEN
      ierr = 9
      err  = 'Missing required key: TimeColumnIndex'
      RETURN
    END IF

    IF (.NOT. haveValueCol) THEN
      ierr = 10
      err  = 'Missing required key: ValueColumnIndex'
      RETURN
    END IF

    csvOut = resolve_relative_path(baseDir, csvRaw)
  END SUBROUTINE parse_SignalProviderCsv_infile


  SUBROUTINE load_delimited_columns(path, timeCol, valueCol, t, v, n, ierr, err)
    CHARACTER(*), INTENT(IN) :: path
    INTEGER,      INTENT(IN) :: timeCol, valueCol
    REAL(8), ALLOCATABLE, INTENT(OUT) :: t(:), v(:)
    INTEGER, INTENT(OUT) :: n, ierr
    CHARACTER(*), INTENT(OUT) :: err

    INTEGER :: u, ios, count, i
    CHARACTER(2048) :: line
    REAL(8) :: tt, vv

    ierr = 0
    err  = ''
    n    = 0

    IF (timeCol < 1 .OR. valueCol < 1) THEN
      ierr = 1
      err  = 'TimeColumn and ValueColumn must be >= 1.'
      RETURN
    END IF

    OPEN(NEWUNIT=u, FILE=TRIM(path), STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
      ierr = 2
      err  = 'Could not open comma-separated file: '//TRIM(path)
      RETURN
    END IF

    count = 0
    DO
      READ(u,'(A)',IOSTAT=ios) line
      IF (ios /= 0) EXIT
      IF (is_blank_or_comment(line)) CYCLE
      IF (try_parse_selected_columns(line, timeCol, valueCol, tt, vv)) count = count + 1
    END DO
    CLOSE(u)

    IF (count < 2) THEN
      ierr = 3
      err  = 'File has fewer than 2 valid numeric rows for the selected TimeColumn and ValueColumn.'
      RETURN
    END IF

    ALLOCATE(t(count), v(count))
    n = count

    OPEN(NEWUNIT=u, FILE=TRIM(path), STATUS='OLD', ACTION='READ', IOSTAT=ios)
    IF (ios /= 0) THEN
      ierr = 4
      err  = 'Could not re-open comma-separated file: '//TRIM(path)
      RETURN
    END IF

    i = 0
    DO
      READ(u,'(A)',IOSTAT=ios) line
      IF (ios /= 0) EXIT
      IF (is_blank_or_comment(line)) CYCLE
      IF (try_parse_selected_columns(line, timeCol, valueCol, tt, vv)) THEN
        i = i + 1
        t(i) = tt
        v(i) = vv
      END IF
    END DO
    CLOSE(u)

    CALL ensure_sorted_by_time(t, v, n, ierr, err)
  END SUBROUTINE load_delimited_columns


  LOGICAL FUNCTION try_parse_selected_columns(line, timeCol, valueCol, tval, vval)
    CHARACTER(*), INTENT(IN) :: line
    INTEGER,      INTENT(IN) :: timeCol, valueCol
    REAL(8),      INTENT(OUT) :: tval, vval

    CHARACTER(:), ALLOCATABLE :: f1, f2
    INTEGER :: ios1, ios2

    try_parse_selected_columns = .FALSE.

    f1 = get_csv_field(line, timeCol)
    f2 = get_csv_field(line, valueCol)

    IF (.NOT. ALLOCATED(f1)) RETURN
    IF (.NOT. ALLOCATED(f2)) RETURN
    IF (LEN_TRIM(f1) == 0 .OR. LEN_TRIM(f2) == 0) RETURN

    READ(f1, *, IOSTAT=ios1) tval
    READ(f2, *, IOSTAT=ios2) vval

    IF (ios1 == 0 .AND. ios2 == 0) THEN
      try_parse_selected_columns = .TRUE.
    END IF
  END FUNCTION try_parse_selected_columns


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

END MODULE SignalProvider_Subs