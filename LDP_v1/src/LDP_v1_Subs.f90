! Name:   		Baseline lidar data processing (LDP) DLL for lidar-assisted feedforward pitch control.
! Authors: 		Feng Guo, David Schlipf from Flensburg University of Applied Sciences, funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358.   
! Target: 		This code aims to provide a reference Lidar-assisted control package for the community. Please cite the following paper if this code is helpful for your research:
! 				Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci., 8, 149–171, https://doi.org/10.5194/wes-8-149-2023, 2023.   
! Function: 	The LDP module reads in LOS measurements from a lidar and estimates the rotor-effective wind speed which will eventually be written to the avrSWAP array.
! Reference:	The subroutines rely on the legacy Bladed style data interface. See the Bladed manual for more detail.    
! 				The code is written based on the source code of ROSCO. Version 2.4.1, https://github.com/NREL/ROSCO, 2021. by NREL.
! License: 		MIT License
! Copyright (c) 2022 Flensburg University of Applied Sciences, WETI
! -------------------------------------------------------------------------------------------

!=======================================================================
MODULE LDP_Subs
!...............................................................................................................................
    !USE Constants   
    USE :: LDP_Types
	USE :: LDP_Helpers
    
    IMPLICIT NONE
	
CONTAINS

   	! -----------------------------------------------------------------------------------
    ! Read avrSWAP array into the local Lidar Variables
    SUBROUTINE ReadAvrSWAP(avrSWAP, LidarVar)
	
        USE LDP_Types, ONLY : LidarVariables

        REAL(C_FLOAT), INTENT(INOUT) 		:: avrSWAP(*)	! Swap array
        TYPE(LidarVariables), INTENT(INOUT) :: LidarVar 	! Lidar related variables
        INTEGER(4)                      	:: L           	! Index in the array where the lidar related data begins
        INTEGER(4)                         	:: iGate        ! Index for range gates
        
        ! Load variables from calling program (See Appendix A of Bladed User's Guide):
        LidarVar%iStatus            = NINT(avrSWAP(1))      ! Status of simulation [-]
        L                           = NINT(avrSWAP(63))     
        LidarVar%NewMeasurementFlag = NINT(avrSWAP(L))      ! New measurement (flag) [-]
       	LidarVar%BeamID             = NINT(avrSWAP(L+1))    ! Beam ID of current lidar data [-]
        LidarVar%GatesPerBeam    	= NINT(avrSWAP(L + 2)) 	! Number of gates in each lidar beam [-]
		! Allocates v_los and read in values from swap array
		IF (.not. allocated(LidarVar%v_los)) THEN
			Allocate(LidarVar%v_los(LidarVar%GatesPerBeam))
		END IF
		DO iGate = 1,LidarVar%GatesPerBeam,1
		   LidarVar%v_los(iGate)  	= avrSWAP(L + 2 + iGate) 							! Line-of-sight wind speeds measurements [m/s]
		END DO
        LidarVar%AvrIndexREWS   	= L + 2 + (LidarVar%GatesPerBeam) + 7            	! Index for REWS [-]

    END SUBROUTINE ReadAvrSWAP    
 	! -----------------------------------------------------------------------------------
   
	! -----------------------------------------------------------------------------------
	! Set all initial lidar variables 
    SUBROUTINE SetLidarParameters(avrSWAP, accINFILE, avcMSG_size, LidarVar, ErrVar)
	
        USE LDP_Types, ONLY : LidarErrorVariables, LidarVariables
        
        REAL(C_FLOAT),              INTENT(INOUT)  	:: avrSWAP(*)          				! Swap array
        CHARACTER(C_CHAR),          INTENT(IN   ) 	:: accINFILE(NINT(avrSWAP(50)))  	! Name of the parameter input file
        INTEGER(4),                 INTENT(IN   )   :: avcMSG_size 				 		! Size of error message		
        TYPE(LidarErrorVariables),  INTENT(INOUT)   :: ErrVar 							! Lidar related error variables
        TYPE(LidarVariables),       INTENT(INOUT)   :: LidarVar     					! Lidar related variables
        INTEGER(4)                              	:: iBuffer              			! Index for buffer           
        CHARACTER(*),               PARAMETER       :: RoutineName = 'SetLidarParameters'
 
        ! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF	

		! Description:
		print *, '--------------------------------------------------------------------'
		print *, 'A baseline lidar data processing algorithm - v1.4'
		print *, 'Developed by Flensburg University of Applied Sciences, Germany'
		print *, '--------------------------------------------------------------------'

		! Read all constant parameters from *.IN parameter file			
		CALL ReadLidarParameterFileSub(LidarVar, accINFILE, NINT(avrSWAP(50)), ErrVar)
		
		! Allocates buffer and set it to the error code
		IF (.not. allocated(LidarVar%u_est_Buffer)) THEN
			Allocate(LidarVar%u_est_Buffer(LidarVar%NumberOfBeams))
		END IF		
		DO iBuffer = 1,LidarVar%NumberOfBeams,1
		   LidarVar%u_est_Buffer(iBuffer) = LidarVar%ErrorCode
		END DO   		
		
		! Add RoutineName to error message
		IF (ErrVar%aviFAIL < 0) THEN
			ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
			RETURN
		ENDIF

    END SUBROUTINE SetLidarParameters
 	! -----------------------------------------------------------------------------------

    ! -----------------------------------------------------------------------------------
    ! Read all constant parameters from *.IN parameter file
    SUBROUTINE ReadLidarParameterFileSub(LidarVar, accINFILE, accINFILE_size, ErrVar)
	
        USE, INTRINSIC :: ISO_C_Binding
        USE LDP_Types, ONLY : LidarErrorVariables, LidarVariables

        INTEGER(4)                                      :: accINFILE_size               ! Size of DISCON input filename
        CHARACTER(accINFILE_size),  INTENT(IN   )       :: accINFILE(accINFILE_size)    ! DISCON input filename
        TYPE(LidarErrorVariables),  INTENT(INOUT)       :: ErrVar 						! Lidar related error variables
        TYPE(LidarVariables),       INTENT(INOUT)       :: LidarVar 					! Lidar related variables
        INTEGER(4),                 PARAMETER           :: UnControllerParameters = 89  ! Unit number to open file
        INTEGER(4)                                      :: CurLine 
        CHARACTER(*),               PARAMETER           :: RoutineName = 'ReadLidarParameterFileSub'

        ! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF	

		! Open file and set current line to first line	
		OPEN(unit=UnControllerParameters, file=accINFILE(1), status='old', action='read')
		CurLine = 1  
   
        !------- Header ----------------------------------------
        CALL ReadEmptyLine(UnControllerParameters,CurLine)
        CALL ReadEmptyLine(UnControllerParameters,CurLine)
        CALL ReadEmptyLine(UnControllerParameters,CurLine)

        !------- Lidar trajectory ------------------------------
        CALL ParseInput(UnControllerParameters,CurLine,'NumberOfBeams',  		accINFILE(1),LidarVar%NumberOfBeams,		ErrVar)
        CALL ParseInput(UnControllerParameters,CurLine,'AngleToCenterline',  	accINFILE(1),LidarVar%AngleToCenterline,	ErrVar)		
        CALL ParseInput(UnControllerParameters,CurLine,'IndexGate',  			accINFILE(1),LidarVar%IndexGate,			ErrVar)				
           
        ! Close Input File
        CLOSE(UnControllerParameters)
        
        ! Add RoutineName to error message
        IF (ErrVar%aviFAIL < 0) THEN
            ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
        ENDIF

    END SUBROUTINE ReadLidarParameterFileSub
    ! -----------------------------------------------------------------------------------
		
	!------------------------------------------------------------------------------------
	! Reconstruct rotor-effective wind speed based on lidar line-of-sight measurements
    SUBROUTINE WindFieldReconstruction(LidarVar)
	
        USE LDP_Types, ONLY : LidarVariables
        
        TYPE(LidarVariables), INTENT(INOUT)       	:: LidarVar 			! Lidar related variables
        INTEGER(4)                               	:: iBuffer           	! Index for buffer
		INTEGER(4)                                	:: Counter       		! Counter to determine how many LOS will be used for REWS estimation
		REAL(8)                                   	:: u_est_sum      		! Sum of estimated u components

        ! Estimate u component assuming perfect alignment
        LidarVar%u_est 		= LidarVar%v_los(LidarVar%IndexGate)/COSD(LidarVar%AngleToCenterline)
		
        ! First-in-last-out buffer for estimated u component (we need a fixed buffer length for compilation)		
        DO iBuffer = LidarVar%NumberOfBeams, 2, -1
           LidarVar%u_est_Buffer(iBuffer) 	= LidarVar%u_est_Buffer(iBuffer-1)
        END DO
		LidarVar%u_est_Buffer(1) 			= LidarVar%u_est 		
		
		! Loop over the last full scan in u_est_buffer to sum up all values 
		Counter 			= 0	
		u_est_sum 			= 0		
        DO iBuffer  = 1, LidarVar%NumberOfBeams, 1
			IF (LidarVar%u_est_Buffer(iBuffer) /= LidarVar%ErrorCode) THEN
				u_est_sum 	= u_est_sum + LidarVar%u_est_Buffer(iBuffer)
				Counter 	= Counter+1;
			END IF	
        END DO  
            
        ! Calculate REWS from mean over all estimated u components
        IF (Counter /=0) THEN
			LidarVar%REWS 	= u_est_sum/Counter
        ELSE
			LidarVar%REWS  	= LidarVar%ErrorCode
        END IF	

    END SUBROUTINE WindFieldReconstruction
	! -----------------------------------------------------------------------------------
	
   
END MODULE LDP_Subs
