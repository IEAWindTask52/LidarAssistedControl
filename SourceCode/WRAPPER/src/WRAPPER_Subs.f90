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

MODULE WRAPPER_Subs
!...............................................................................................................................
    !USE Constants   
    USE :: WRAPPER_Types
	USE :: WRAPPER_Helpers
    
    IMPLICIT NONE
	
CONTAINS
	! -----------------------------------------------------------------------------------
    ! Get the sub DLL information 
    SUBROUTINE SetDLLParameters(avrSWAP, accINFILE, size_avcMSG, LidarVar, ErrVar)
        USE WRAPPER_Types
        
        REAL(C_FLOAT),              INTENT(INOUT)    		:: avrSWAP(*)          ! The swap array, used to pass data to, and receive data from, the DLL controller.
        CHARACTER(C_CHAR),          INTENT(IN   )    		:: accINFILE(NINT(avrSWAP(50)))     ! The name of the parameter input file

        INTEGER(4),                 INTENT(IN   )   		:: size_avcMSG
        TYPE(LidarErrorVariables),	INTENT(INOUT) 			:: ErrVar
        TYPE(LidarVariables),     	INTENT(INOUT)   		:: LidarVar

        
        INTEGER(4)                              			:: iStatus ! the status of the DLL chain calling, 0 means first call
        
        CHARACTER(*),               PARAMETER       		:: RoutineName = 'SetDLLParameters'

        
        iStatus            = NINT(avrSWAP(1))

        ! Set ErrVar%aviFAIL to 0 in each iteration:
        ErrVar%aviFAIL = 0
        ! ALLOCATE(ErrVar%ErrMsg(size_avcMSG-1))
        ErrVar%size_avcMSG  = size_avcMSG
        
        
        ! Read any External DLL Parameters specified in the User Interface
        !   and initialize variables:
        IF (iStatus == 0) THEN ! .TRUE. if we're on the first call to the DLL
            
            
            ! Description:
            print *, '--------------------------------------------------------------------'
            print *, 'A DLL chain for developing lidar-assisted control - v1.0'
            print *, 'Developed by Flensburg University of Applied Sciences, Germany'
            print *, '--------------------------------------------------------------------'    

            CALL ReadDLLParameterFileSub(LidarVar,accINFILE, NINT(avrSWAP(50)),ErrVar)
            
            ! If there's been an file reading error, don't continue
            ! Add RoutineName to error message
            IF (ErrVar%aviFAIL < 0) THEN
                ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
                RETURN
            ENDIF

            

        ENDIF
    END SUBROUTINE SetDLLParameters
    
    ! -----------------------------------------------------------------------------------
    ! Read all constant control parameters from DISCON.IN parameter file
    SUBROUTINE ReadDLLParameterFileSub(LidarVar,accINFILE, accINFILE_size,ErrVar)!, accINFILE_size)
        USE, INTRINSIC :: ISO_C_Binding
        USE WRAPPER_Types, ONLY : LidarErrorVariables
        
        INTEGER(4)                                      :: accINFILE_size               ! size of DISCON input filename
        CHARACTER(accINFILE_size),  INTENT(IN   )       :: accINFILE(accINFILE_size)    ! DISCON input filename
        TYPE(LidarVariables),    	INTENT(INOUT)       :: LidarVar
        TYPE(LidarErrorVariables), 	INTENT(INOUT)       :: ErrVar             	!

        INTEGER(4),                 PARAMETER           :: UnControllerParameters = 89  ! Unit number to open file
        INTEGER(4)                                      :: CurLine 
        INTEGER(4)                                      :: ppos             			! index to remove file extension
        INTEGER(4)                                      :: stringlength     			! length of a string
        INTEGER(4)                                      :: iDLL                 		! counter  
 
        CHARACTER(*),               PARAMETER           :: RoutineName = 'ReadDLLParameterFileSub'

        CurLine = 1
       

        OPEN(unit=UnControllerParameters, file=accINFILE(1), status='old', action='read')
        
        !----------------------- HEADER ------------------------
        CALL ReadEmptyLine(UnControllerParameters,CurLine)
        CALL ReadEmptyLine(UnControllerParameters,CurLine)
        CALL ReadEmptyLine(UnControllerParameters,CurLine)

        !----------------------- NumberOfsubDLLs ------------------------
        CALL ParseInput(UnControllerParameters,CurLine,'NumberofSubDLLs',accINFILE(1),LidarVar%NumberOfsubDLLs,ErrVar)
        
        
        IF (.not. allocated(LidarVar%PROCADDR)) THEN 
            ALLOCATE(LidarVar%PROCADDR(LidarVar%NumberOfsubDLLs))
        END IF
        
        IF (.not. allocated(LidarVar%DLLFILENAME)) THEN 
            ALLOCATE(LidarVar%DLLFILENAME(LidarVar%NumberOfsubDLLs))
        END IF
        
        IF (.not. allocated(LidarVar%DLLINPUTFILENAME)) THEN 
            ALLOCATE(LidarVar%DLLINPUTFILENAME(LidarVar%NumberOfsubDLLs))
        END IF
        
        ! LOOP to get all DLL names and DLL input names
        DO iDLL=1, LidarVar%NumberOfsubDLLs
			CALL ParseInput(UnControllerParameters,CurLine,'SubDLLName',     accINFILE(1),LidarVar%DLLFILENAME(iDLL),     ErrVar)
			CALL ParseInput(UnControllerParameters,CurLine,'SubDLLInputName',accINFILE(1),LidarVar%DLLINPUTFILENAME(iDLL),ErrVar)
	      
            IF (len_trim( LidarVar%DLLINPUTFILENAME(iDLL))==0) THEN
                ErrVar%ErrMsg = RoutineName//':'//'Error reading the DISCON.IN, the number of sub DLLs does not match with the lines in the DISCON.IN file, check the DISCON.IN is correctly set up.'
                print * , TRIM(ErrVar%ErrMsg)
                ErrVar%aviFAIL = -1
            RETURN
            END IF

        END DO    
        
        
        ! Close Input File
        CLOSE(UnControllerParameters)

        

        ! Add RoutineName to error message
        IF (ErrVar%aviFAIL < 0) THEN
            ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
        ENDIF

    END SUBROUTINE ReadDLLParameterFileSub
    ! -----------------------------------------------------------------------------------
END MODULE WRAPPER_Subs
