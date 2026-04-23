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
MODULE PFC_Helpers
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

CONTAINS

  FUNCTION c_char_array_to_string(ca) RESULT(s)
    CHARACTER(KIND=C_CHAR), INTENT(IN) :: ca(:)
    CHARACTER(:), ALLOCATABLE :: s
    INTEGER :: i, n, nch

    n = SIZE(ca)

	nch = 0
    DO i = 1, n
      IF (ca(i) == C_NULL_CHAR) EXIT
      nch = nch + 1
    END DO

    IF (nch <= 0) THEN
      s = ''
      RETURN
    END IF

    ALLOCATE(CHARACTER(LEN=nch) :: s)

    DO i = 1, nch
      s(i:i) = ACHAR(IACHAR(ca(i)))
    END DO
  END FUNCTION c_char_array_to_string


  SUBROUTINE set_discon_message(msgArray, text)
    CHARACTER(KIND=C_CHAR), INTENT(INOUT) :: msgArray(:)
    CHARACTER(*),           INTENT(IN)    :: text
    INTEGER :: i, n

    n = SIZE(msgArray)
    msgArray = C_NULL_CHAR

    DO i = 1, MIN(LEN_TRIM(text), n-1)
      msgArray(i) = text(i:i)
    END DO

    msgArray(MIN(LEN_TRIM(text)+1, n)) = C_NULL_CHAR
  END SUBROUTINE set_discon_message


  FUNCTION trim_whitespace(s) RESULT(out)
    CHARACTER(*), INTENT(IN) :: s
    CHARACTER(:), ALLOCATABLE :: out
    INTEGER :: i1, i2

    i1 = 1
    i2 = LEN(s)

    DO WHILE (i1 <= i2 .AND. (s(i1:i1) == ' ' .OR. s(i1:i1) == ACHAR(9)))
      i1 = i1 + 1
    END DO

    DO WHILE (i2 >= i1 .AND. (s(i2:i2) == ' ' .OR. s(i2:i2) == ACHAR(9)))
      i2 = i2 - 1
    END DO

    IF (i2 < i1) THEN
      out = ''
    ELSE
      out = s(i1:i2)
    END IF
  END FUNCTION trim_whitespace


  SUBROUTINE tabs_to_spaces(s)
    CHARACTER(*), INTENT(INOUT) :: s
    INTEGER :: i

    DO i = 1, LEN(s)
      IF (s(i:i) == ACHAR(9)) s(i:i) = ' '
    END DO
  END SUBROUTINE tabs_to_spaces


  SUBROUTINE strip_comment_and_trim(s)
    CHARACTER(*), INTENT(INOUT) :: s
    INTEGER :: p

    CALL tabs_to_spaces(s)
    p = INDEX(s, '#')
    IF (p > 0) s = s(1:p-1)
    s = trim_whitespace(s)
  END SUBROUTINE strip_comment_and_trim


  LOGICAL FUNCTION split_key_value(line, key, val)
    CHARACTER(*), INTENT(IN) :: line
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: key, val
    INTEGER :: p

    p = INDEX(line, ':')
    IF (p <= 0) THEN
      split_key_value = .FALSE.
      RETURN
    END IF

    key = trim_whitespace(line(1:p-1))
    val = trim_whitespace(line(p+1:))

    IF (LEN(val) >= 2) THEN
      IF ((val(1:1) == '"' .AND. val(LEN(val):LEN(val)) == '"') .OR. &
          (val(1:1) == "'" .AND. val(LEN(val):LEN(val)) == "'")) THEN
        val = val(2:LEN(val)-1)
      END IF
    END IF

    split_key_value = .TRUE.
  END FUNCTION split_key_value


  SUBROUTINE to_lower_inplace(s)
    CHARACTER(:), ALLOCATABLE, INTENT(INOUT) :: s
    INTEGER :: i, c

    DO i = 1, LEN(s)
      c = IACHAR(s(i:i))
      IF (c >= IACHAR('A') .AND. c <= IACHAR('Z')) THEN
        s(i:i) = ACHAR(c + 32)
      END IF
    END DO
  END SUBROUTINE to_lower_inplace


  LOGICAL FUNCTION parse_bool(txt, ios)
    CHARACTER(*), INTENT(IN) :: txt
    INTEGER, INTENT(OUT) :: ios
    CHARACTER(:), ALLOCATABLE :: t

    ios = 0
    t = trim_whitespace(txt)
    CALL to_lower_inplace(t)

    SELECT CASE (TRIM(t))
    CASE ('true','t','1','yes','y')
      parse_bool = .TRUE.
    CASE ('false','f','0','no','n')
      parse_bool = .FALSE.
    CASE DEFAULT
      ios = 1
      parse_bool = .TRUE.
    END SELECT
  END FUNCTION parse_bool


  FUNCTION dirname_of_path(p) RESULT(d)
    CHARACTER(*), INTENT(IN) :: p
    CHARACTER(:), ALLOCATABLE :: d
    INTEGER :: i

    d = '.'
    DO i = LEN_TRIM(p), 1, -1
      IF (p(i:i) == '/' .OR. p(i:i) == '\') THEN
        IF (i > 1) THEN
          d = p(1:i-1)
        ELSE
          d = p(1:1)
        END IF
        RETURN
      END IF
    END DO
  END FUNCTION dirname_of_path


  FUNCTION resolve_relative_path(baseDir, rel) RESULT(full)
    CHARACTER(*), INTENT(IN) :: baseDir, rel
    CHARACTER(:), ALLOCATABLE :: full

    IF (LEN_TRIM(rel) >= 1) THEN
      IF (rel(1:1) == '/' .OR. rel(1:1) == '\') THEN
        full = TRIM(rel)
        RETURN
      END IF

      IF (LEN_TRIM(rel) >= 2) THEN
        IF (rel(2:2) == ':') THEN
          full = TRIM(rel)
          RETURN
        END IF
      END IF
    END IF

    full = TRIM(baseDir)//'/'//TRIM(rel)
  END FUNCTION resolve_relative_path


  LOGICAL FUNCTION is_blank_or_comment(line)
    CHARACTER(*), INTENT(IN) :: line
    CHARACTER(:), ALLOCATABLE :: s

    s = ADJUSTL(line)

    IF (LEN_TRIM(s) == 0) THEN
      is_blank_or_comment = .TRUE.
    ELSE
      is_blank_or_comment = (s(1:1) == '#') .OR. (s(1:1) == '!')
    END IF
  END FUNCTION is_blank_or_comment


  FUNCTION strip_optional_quotes(s) RESULT(out)
    CHARACTER(*), INTENT(IN) :: s
    CHARACTER(:), ALLOCATABLE :: out
    CHARACTER(:), ALLOCATABLE :: tmp
    INTEGER :: n

    tmp = trim_whitespace(s)
    n = LEN(tmp)

    IF (n >= 2) THEN
      IF ((tmp(1:1) == '"' .AND. tmp(n:n) == '"') .OR. &
          (tmp(1:1) == "'" .AND. tmp(n:n) == "'")) THEN
        out = tmp(2:n-1)
        RETURN
      END IF
    END IF

    out = tmp
  END FUNCTION strip_optional_quotes

END MODULE PFC_Helpers