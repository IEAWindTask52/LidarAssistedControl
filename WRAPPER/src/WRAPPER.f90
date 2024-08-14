! Name:   		Master (wrapper) DLL.
! Authors: 		Feng Guo, David Schlipf from Flensburg University of Applied Sciences, funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358.   
! Target: 		This code aims to provide a reference Lidar-assisted control package for the community. Please cite the following paper if this code is helpful for your research:
! 				Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci. Discuss.
! 				[preprint], https://doi.org/10.5194/wes-2022-62, in review, 2022.    
! Function: 	The DLL chain is designed to make the lidar data processing or other algorithms more independent from the feedback controller. 
! 				It allows a more flexible design of additional algorithms which meet the requirement of a "smart lidar" concept (https://zenodo.org/record/5004524#.Yevsp_7MKUk)
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
USE             :: WRAPPER_Subs
USE             :: WRAPPER_Types
USE                IFWINTY,     ONLY : HANDLE,LPVOID
USE                kernel32,    ONLY : LoadLibrary,GetProcAddress

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
CHARACTER(SIZE(avcOUTNAME)-1)                   :: RootName                         ! a Fortran version of the input C string (not considered an array here)    [subtract 1 for the C null-character]
CHARACTER(SIZE(avcMSG)-1)                       :: ErrMsg                           ! a Fortran version of the C string argument (not considered an array here) [subtract 1 for the C null-character]
INTEGER(4)                                      :: iDLL       	! Counter
INTEGER(HANDLE)                                 :: FileAddr    	! The address of file FileName.         (RETURN value from LoadLibrary in kernel32.f90)
INTEGER(LPVOID)                                 :: ProcAddr    	! The address of procedure ProcName.    (RETURN value from GetProcAddress in kernel32.f90)   

TYPE(LidarVariables),           SAVE            :: LidarVar                         ! lidar related variable type
TYPE(LidarErrorVariables),      SAVE            :: ErrVar                           ! lidar related error type

CHARACTER(*),                   PARAMETER      	:: RoutineName = 'DISCON'


ABSTRACT INTERFACE
      SUBROUTINE BladedDLL_Legacy_Procedure ( avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG )  BIND(C)
         USE, INTRINSIC :: ISO_C_Binding

         REAL(C_FLOAT),          INTENT(INOUT) :: avrSWAP   (*)  !< DATA
         INTEGER(C_INT),         INTENT(INOUT) :: aviFAIL        !< FLAG  (Status set in DLL and returned to simulation code)
         CHARACTER(KIND=C_CHAR), INTENT(IN   ) :: accINFILE (*)  !< INFILE
         CHARACTER(KIND=C_CHAR), INTENT(IN   ) :: avcOUTNAME(*)  !< OUTNAME (in:Simulation RootName; out:Name:Units; of logging channels)
         CHARACTER(KIND=C_CHAR), INTENT(INOUT) :: avcMSG    (*)  !< MESSAGE (Message from DLL to simulation code [ErrMsg])
      END SUBROUTINE BladedDLL_Legacy_Procedure

END INTERFACE

PROCEDURE(BladedDLL_Legacy_Procedure), POINTER :: DLL_Legacy_Subroutine          ! The address of the (legacy DISCON) procedure in the Bladed DLL


RootName = TRANSFER(avcOUTNAME, RootName)

! Get the sub DLL information from the DISCON.IN, check whether the DISCON is correct
CALL SetDLLParameters(avrSWAP, accINFILE, SIZE(avcMSG), LidarVar, ErrVar)

IF (ErrVar%aviFAIL < 0) THEN  ! Check whether error occurs in the last step, DLLs setting up
    
    ! If error occurs, return
    ErrMsg = ErrVar%ErrMsg
    avcMSG = TRANSFER(TRIM(ErrMsg)//C_NULL_CHAR, avcMSG, SIZE(avcMSG))
    aviFAIL = -1
    RETURN
    
    ELSE  ! Loop over to get the sub DLL procedure addresses, and execute the sub DLLs  
        
    DO iDLL=1, LidarVar%NumberOfsubDLLs

        FileAddr = LoadLibrary( TRIM(LidarVar%DLLFILENAME(iDLL))//C_NULL_CHAR ) ! get the file address of the DLL
        ProcAddr = GetProcAddress( FileAddr,'DISCON') ! get the procedure address of the DLL
        LidarVar%PROCADDR(iDLL) = TRANSFER(ProcAddr, LidarVar%PROCADDR(iDLL)) ! transfer the address to the pointer type
        IF(.NOT. C_ASSOCIATED(LidarVar%PROCADDR(iDLL))) THEN ! if the DLL is not found, return
                ErrMsg = RoutineName//':'//'Error loading the sub DLLs by DISCON.dll: the DLL procedure '//TRIM(LidarVar%DLLFILENAME(iDLL))//' could not be loaded.'
                print * , TRIM(ErrMsg)
                avcMSG = TRANSFER(TRIM(ErrMsg)//C_NULL_CHAR, avcMSG, SIZE(avcMSG))
                aviFAIL = -1
                RETURN
        ELSE      ! if the address is correctly associated, then we can run the DLL
            CALL C_F_PROCPOINTER(LidarVar%PROCADDR(iDLL), DLL_Legacy_Subroutine)
            CALL DLL_Legacy_Subroutine (avrSWAP, aviFAIL, TRIM(LidarVar%DLLINPUTFILENAME(iDLL))//C_NULL_CHAR, avcOUTNAME, avcMSG )
        END IF
    END DO

END IF



!! Add RoutineName to error message
IF (aviFAIL < 0) THEN
    avcMSG = RoutineName//':'//TRIM(ErrMsg)
ENDIF

RETURN
END SUBROUTINE DISCON