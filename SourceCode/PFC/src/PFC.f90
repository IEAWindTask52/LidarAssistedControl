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
SUBROUTINE DISCON(avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG) BIND (C, NAME='DISCON')
! DO NOT REMOVE or MODIFY LINES starting with "!DEC$" or "!GCC$"

  USE, INTRINSIC :: ISO_C_BINDING
  USE PFC_Types
  USE PFC_Helpers
  USE PFC_Subs
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
  REAL(8) :: t_now, x_now, dxdt_now, y_now
  CHARACTER(:), ALLOCATABLE :: inFileStr

  CHARACTER(*), PARAMETER :: RoutineName = 'PFC'

  aviFAIL = 0
  ErrMsg  = ''

  inFileStr = c_char_array_to_string(accINFILE)

  IF (.NOT. SPState%initialized) THEN
    SPState%param_path = TRIM(inFileStr)
    
    print *, '--------------------------------------------------------------------'
	print *, 'A PFC module for OpenFAST - v1.0'
	print *, 'Developed by Flensburg University of Applied Sciences, Germany'
	print *, '--------------------------------------------------------------------'
    
    CALL parse_PFC_infile( &
      SPState%param_path, &
      SPState%swap_in_index, &
      SPState%swap_out_index, &
      SPState%H, &
      SPState%S, &
      SPState%f0, &
      SPState%omega_rated, &
      ierr, ErrMsg )

    IF (ierr /= 0) THEN
      aviFAIL = -1
      CALL set_discon_message(avcMSG, RoutineName//': '//TRIM(ErrMsg))
      RETURN
    END IF
    
    SPState%first_step  = .TRUE.
    SPState%t_prev      = 0.0D0
    SPState%x_prev      = 0.0D0
    SPState%initialized = .TRUE.
  END IF

avrSWAP_Status = NINT(avrSWAP(1))
  IF (avrSWAP_Status < 0) THEN
    CALL set_discon_message(avcMSG, '')
    aviFAIL = 0
    RETURN
  END IF

  t_now = REAL(avrSWAP(2), KIND=8)

  IF (SPState%swap_in_index < 1) THEN
    aviFAIL = -1
    CALL set_discon_message(avcMSG, RoutineName//': InputSwapIndex must be >= 1.')
    RETURN
  END IF

  IF (SPState%swap_out_index < 1) THEN
    aviFAIL = -1
    CALL set_discon_message(avcMSG, RoutineName//': OutputSwapIndex must be >= 1.')
    RETURN
  END IF

  x_now = REAL(avrSWAP(SPState%swap_in_index), KIND=8)

  CALL differentiate_signal( &
    t_now, x_now, &
    SPState%first_step, &
    SPState%t_prev, &
    SPState%x_prev, &
    dxdt_now )

  CALL compute_delta_MgFF( &
    x_now, dxdt_now, &
    SPState%H, &
    SPState%S, &
    SPState%f0, &
    SPState%omega_rated, &
    y_now )


  IF (avrSWAP_Status >= 0) THEN
    avrSWAP(SPState%swap_out_index) = REAL(y_now, KIND=C_FLOAT)
  END IF

  CALL set_discon_message(avcMSG, '')
  aviFAIL = 0

  RETURN
END SUBROUTINE DISCON