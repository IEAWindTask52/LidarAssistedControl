! Name:   		Baseline lidar data processing (LDP) DLL for lidar-assisted feedforward pitch control.
! Authors: 		Feng Guo, David Schlipf from Flensburg University of Applied Sciences, funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358.   
! Target: 		This code aims to provide a reference Lidar-assisted control package for the community. Please cite the following paper if this code is helpful for your research:
! 				Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci. Discuss.
! 				[preprint], https://doi.org/10.5194/wes-2022-62, in review, 2022.    
! Function: 	The LDP module read in LOS measurements from lidar and estimate the rotor effective wind speed which will eventually be written to the avrSWAP array.
! 				See https://doi.org/10.5194/wes-2022-62 for the definition of "rotor effective wind speed".   
! Reference:	The subroutines rely on the legacy Bladed style data interface. See the Bladed manual for more detail.    
! 				The code is written based on the source code of ROSCO. Version 2.4.1, https://github.com/NREL/ROSCO, 2021. by NREL.
! License: 		MIT License
! Copyright (c) 2022 Flensburg University of Applied Sciences, WETI
! -------------------------------------------------------------------------------------------

!=======================================================================
SUBROUTINE DISCON(avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG) BIND (C, NAME='DISCON')
! DO NOT REMOVE or MODIFY LINES starting with "!DEC$" or "!GCC$"
! !DEC$ specifies attributes for IVF and !GCC$ specifies attributes for gfortran

USE, INTRINSIC  :: ISO_C_Binding
USE             :: LDP_Types
USE             :: LDP_Subs


IMPLICIT NONE
! Enable .dll export
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: DISCON
!GCC$ ATTRIBUTES DLLEXPORT :: DISCON
#endif

!------------------------------------------------------------------------------------------------------------------------------
! Variable declaration and initialization
!------------------------------------------------------------------------------------------------------------------------------

REAL(C_FLOAT),                  INTENT(INOUT)   :: avrSWAP(*)                       ! The swap array, used to pass data to, and receive data from, the DLL controller.
INTEGER(C_INT),                 INTENT(INOUT)   :: aviFAIL                          ! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.
CHARACTER(KIND=C_CHAR),         INTENT(IN   )   :: accINFILE(NINT(avrSWAP(50)))     ! The name of the parameter input file
CHARACTER(KIND=C_CHAR),         INTENT(IN   )   :: avcOUTNAME(NINT(avrSWAP(51)))    ! OUTNAME (Simulation RootName)
CHARACTER(KIND=C_CHAR),         INTENT(INOUT)   :: avcMSG(NINT(avrSWAP(49)))        ! MESSAGE (Message from DLL to simulation code [ErrMsg])  The message which will be displayed by the calling program if aviFAIL <> 0.
CHARACTER(SIZE(avcMSG)-1)                       :: ErrMsg                           ! a FORTRAN version of the C string argument (not considered an array here) [subtract 1 for the C null-character]

TYPE(LidarVariables),           SAVE            :: LidarVar                         ! lidar related variable type
TYPE(LidarErrorVariables),      SAVE            :: ErrVar                           ! lidar related error type

CHARACTER(*),                   PARAMETER       :: RoutineName = 'LDP'

ErrVar%aviFAIL 			= 0 ! Set ErrVar%aviFAIL to 0
!------------------------------------------------------------------------------------------------------------------------------
! Main Processing
!------------------------------------------------------------------------------------------------------------------------------

! Read avrSWAP array into derived types/variables
CALL ReadAvrSWAP(avrSWAP, LidarVar)

! Tasks which need to be done only in the first call
IF (LidarVar%iStatus == 0) THEN	
	CALL SetLidarParameters(avrSWAP, accINFILE, SIZE(avcMSG), LidarVar, ErrVar)
ENDIF

! If there is a new measurement perform wind field reconstruction
IF (LidarVar%NewMeasurementFlag == 1) THEN         
	CALL WindFieldReconstruction(LidarVar) 
END IF

! Assign the current REWS to the avrSWAP array
avrSWAP(LIDARVAR%AvrIndex_REWS) = LidarVar%REWS 

! Add RoutineName to error message
IF (ErrVar%aviFAIL < 0) THEN
    ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
    print * , TRIM(ErrVar%ErrMsg)
ENDIF
ErrMsg = ErrVar%ErrMsg
avcMSG = TRANSFER(TRIM(ErrVar%ErrMsg)//C_NULL_CHAR, avcMSG, SIZE(avcMSG))
aviFAIL = ErrVar%aviFAIL

RETURN
END SUBROUTINE DISCON
