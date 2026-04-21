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
SUBROUTINE DISCON(avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG) BIND (C, NAME='DISCON')
! DO NOT REMOVE or MODIFY LINES starting with "!DEC$" or "!GCC$"

  USE, INTRINSIC :: ISO_C_BINDING
  USE SignalProvider_Types
  USE SignalProvider_Helpers
  USE SignalProvider_Subs
  IMPLICIT NONE

#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: DISCON
!GCC$ ATTRIBUTES DLLEXPORT :: DISCON
#endif

  REAL(C_FLOAT),          INTENT(INOUT) :: avrSWAP(*)
  INTEGER(C_INT),         INTENT(INOUT) :: aviFAIL
  CHARACTER(KIND=C_CHAR), INTENT(IN   ) :: accINFILE(NINT(avrSWAP(50)))
  CHARACTER(KIND=C_CHAR), INTENT(IN   ) :: avcOUTNAME(NINT(avrSWAP(51)))
  CHARACTER(KIND=C_CHAR), INTENT(INOUT) :: avcMSG(NINT(avrSWAP(49)))

  CHARACTER(SIZE(avcMSG)-1) :: ErrMsg

  INTEGER :: ierr
  INTEGER :: avrSWAP_Status
  REAL(8) :: t_now, t_eval, y_now
  CHARACTER(:), ALLOCATABLE :: inFileStr

  CHARACTER(*), PARAMETER :: RoutineName = 'SignalProviderCSV'

  aviFAIL = 0
  ErrMsg  = ''

  inFileStr = c_char_array_to_string(accINFILE)

  IF (.NOT. SPState%initialized) THEN
    SPState%param_path = TRIM(inFileStr)
    
    print *, '--------------------------------------------------------------------'
	print *, 'A signal provider for OpenFAST - v1.0'
	print *, 'Developed by Flensburg University of Applied Sciences, Germany'
	print *, '--------------------------------------------------------------------'
    
    CALL parse_SignalProviderCsv_infile( &
      SPState%param_path, &
      SPState%csv_path, &
      SPState%swap_out_index, &
      SPState%do_interp, &
      SPState%preview_time, &
      SPState%time_col, &
      SPState%value_col, &
      ierr, ErrMsg )

    IF (ierr /= 0) THEN
      aviFAIL = -1
      CALL set_discon_message(avcMSG, RoutineName//': '//TRIM(ErrMsg))
      RETURN
    END IF

    CALL load_delimited_columns( &
      SPState%csv_path, &
      SPState%time_col, &
      SPState%value_col, &
      SPState%t_csv, &
      SPState%v_csv, &
      SPState%npts, &
      ierr, ErrMsg )

    IF (ierr /= 0) THEN
      aviFAIL = -1
      CALL set_discon_message(avcMSG, RoutineName//': '//TRIM(ErrMsg))
      RETURN
    END IF

    IF (SPState%swap_out_index < 1) THEN
      aviFAIL = -1
      CALL set_discon_message(avcMSG, RoutineName//': SwapIndex must be >= 1.')
      RETURN
    END IF

    SPState%initialized = .TRUE.
  END IF

  IF (SPState%npts < 2) THEN
    aviFAIL = -1
    CALL set_discon_message(avcMSG, RoutineName//': CSV has too few points.')
    RETURN
  END IF

  t_now = REAL(avrSWAP(2), KIND=8)

  IF (ABS(SPState%preview_time) > 0.0D0) THEN
    t_eval = wrap_time_periodic(t_now + SPState%preview_time, SPState%t_csv(1), SPState%t_csv(SPState%npts))
  ELSE
    t_eval = t_now
  END IF

  IF (SPState%do_interp) THEN
    IF (ABS(SPState%preview_time) > 0.0D0) THEN
      y_now = interp_linear_periodic(SPState%t_csv, SPState%v_csv, SPState%npts, t_eval)
    ELSE
      y_now = interp_linear_clamped(SPState%t_csv, SPState%v_csv, SPState%npts, t_eval)
    END IF
  ELSE
    IF (ABS(SPState%preview_time) > 0.0D0) THEN
      y_now = sample_hold_previous_periodic(SPState%t_csv, SPState%v_csv, SPState%npts, t_eval)
    ELSE
      y_now = sample_hold_previous(SPState%t_csv, SPState%v_csv, SPState%npts, t_eval)
    END IF
  END IF

  avrSWAP_Status = NINT(avrSWAP(1))

  IF (avrSWAP_Status >= 0) THEN
    avrSWAP(SPState%swap_out_index) = REAL(y_now, KIND=C_FLOAT)
  END IF

  CALL set_discon_message(avcMSG, '')
  aviFAIL = 0

  RETURN
END SUBROUTINE DISCON