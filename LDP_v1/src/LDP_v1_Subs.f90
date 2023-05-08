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

        REAL(C_FLOAT), INTENT(INOUT) 		:: avrSWAP(*)	! The swap array, used to pass data to, and receive data from, the DLL controller.
        TYPE(LidarVariables), INTENT(INOUT) :: LidarVar
        INTEGER(4)                      	:: L           	! Index number in the avrSWAP array for the start of lidar data  
        
        ! Load variables from calling program (See Appendix A of Bladed User's Guide):
        LidarVar%iStatus            = NINT(avrSWAP(1))      ! Initialization status       
        
        ! --- read and set the lidar variables
        L                           = NINT(avrSWAP(63))     ! The index in the array where the lidar related data begins 
        
        !> NewData
        LidarVar%NewMeasurementFlag = NINT(avrSWAP(L))      ! Flag whether the current measurement is a new one
       
        !> BeamID
       	LidarVar%BeamID             = NINT(avrSWAP(L+1))    ! Lidar beam number of the current lidar measurement
       
        !> Gates per beam
        LidarVar%GatesPerBeam    	= NINT(avrSWAP(L + 2)) 	! Number of range gate, for this reference version, only one range gate is supported 
                                                             
        !> Lidar line-of-sight speed measurement
        LidarVar%v_los              = avrSWAP(L + 2 + 1 )  
      
        !> Index for LDP outputs
        LidarVar%AvrIndex_REWS   	= L + 2 + (LidarVar%GatesPerBeam) + 7            ! Rotor effective wind speed

    END SUBROUTINE ReadAvrSWAP    
 	! -----------------------------------------------------------------------------------
   
	! -----------------------------------------------------------------------------------
	! Set all initial lidar variables 
    SUBROUTINE SetLidarParameters(avrSWAP, accINFILE, size_avcMSG, LidarVar, ErrVar)
	
        USE LDP_Types, ONLY : LidarErrorVariables, LidarVariables
        
        REAL(C_FLOAT),              INTENT(INOUT)  	:: avrSWAP(*)          				! The swap array, used to pass data to, and receive data from, the DLL controller.
        CHARACTER(C_CHAR),          INTENT(IN   ) 	:: accINFILE(NINT(avrSWAP(50)))  	! The name of the parameter input file
        INTEGER(4),                 INTENT(IN   )   :: size_avcMSG
        TYPE(LidarErrorVariables),  INTENT(INOUT)   :: ErrVar
        TYPE(LidarVariables),       INTENT(INOUT)   :: LidarVar    
        INTEGER(4)                              	:: iBuffer              			! The index for buffer           
        CHARACTER(*),               PARAMETER       :: RoutineName = 'SetLidarParameters'
 
        ! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF	

		! Description:
		print *, '--------------------------------------------------------------------'
		print *, 'A baseline lidar data processing algorithm - v1.2'
		print *, 'Developed by Flensburg University of Applied Sciences, Germany'
		print *, '--------------------------------------------------------------------'

		! Read all constant parameters from *.IN parameter file			
		CALL ReadLidarParameterFileSub(LidarVar, accINFILE, NINT(avrSWAP(50)), ErrVar)
		
		! Calculate the laser beam vector(s) in the lidar coordinate system
		! They are fix and will not change during simulation!
		CALL CalculateLaserBeamVectorLidarCS(LidarVar)

		! Allocates buffer and set it to the error code
		IF (.not. allocated(LidarVar%u_est_Buffer)) THEN
			Allocate(LidarVar%u_est_Buffer(LidarVar%nBuffer))
		END IF		
		DO iBuffer = 1,LidarVar%nBuffer,1
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

        INTEGER(4)                                      :: accINFILE_size               ! size of DISCON input filename
        CHARACTER(accINFILE_size),  INTENT(IN   )       :: accINFILE(accINFILE_size)    ! DISCON input filename
        TYPE(LidarErrorVariables),  INTENT(INOUT)       :: ErrVar
        TYPE(LidarVariables),       INTENT(INOUT)       :: LidarVar
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
        CALL ParseInput(UnControllerParameters,CurLine,'NumberOfBeams',  	accINFILE(1),LidarVar%NumberOfBeams,	ErrVar)
        CALL ParseAry  (UnControllerParameters,CurLine,'Lidar_Azimuth',   	LidarVar%Lidar_Azimuth,LidarVar%NumberOfBeams, 	accINFILE(1),ErrVar)
        CALL ParseAry  (UnControllerParameters,CurLine,'Lidar_Elevation', 	LidarVar%Lidar_Elevation,LidarVar%NumberOfBeams,accINFILE(1),ErrVar)
              
        ! Close Input File
        CLOSE(UnControllerParameters)
        
        ! Add RoutineName to error message
        IF (ErrVar%aviFAIL < 0) THEN
            ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
        ENDIF

    END SUBROUTINE ReadLidarParameterFileSub
    ! -----------------------------------------------------------------------------------
	
	! -----------------------------------------------------------------------------------
	! Calculate the laser beam vector(s) in the lidar coordinate system
    SUBROUTINE CalculateLaserBeamVectorLidarCS(LidarVar)
	
		USE LDP_Types, ONLY : LidarVariables
			
		TYPE(LidarVariables),       INTENT(INOUT)   	:: LidarVar
		INTEGER(4)                                      :: iBeam        ! counter for looping through the coordinate data
		
		! Allocation of vectors 
		IF (.not. allocated(LidarVar%X_n_L)) THEN
			Allocate(LidarVar%X_n_L(LidarVar%NumberOfBeams))
		END IF
		IF (.not. allocated(LidarVar%Y_n_L)) THEN
			Allocate(LidarVar%Y_n_L(LidarVar%NumberOfBeams))
		END IF
		IF (.not. allocated(LidarVar%Z_n_L)) THEN
			Allocate(LidarVar%Z_n_L(LidarVar%NumberOfBeams))
		END IF
		 
		!  Loop over all beams to calculate unit vectors  
		DO iBeam=1,LidarVar%NumberOfBeams
			LidarVar%X_n_L(iBeam) = COSD(LidarVar%Lidar_Elevation(iBeam))*COSD(LidarVar%Lidar_Azimuth(iBeam))
			LidarVar%Y_n_L(iBeam) = COSD(LidarVar%Lidar_Elevation(iBeam))*SIND(LidarVar%Lidar_Azimuth(iBeam))
			LidarVar%Z_n_L(iBeam) = SIND(LidarVar%Lidar_Elevation(iBeam))			
		END DO

    END SUBROUTINE CalculateLaserBeamVectorLidarCS
   	! -----------------------------------------------------------------------------------
	
	!------------------------------------------------------------------------------------
	! Reconstruct rotor-effective wind speed based on lidar line-of-sight measurements
    SUBROUTINE WindFieldReconstruction(LidarVar)
	
        USE LDP_Types, ONLY : LidarVariables
        
        TYPE(LidarVariables), INTENT(INOUT)       	:: LidarVar
        INTEGER(4)                               	:: iBuffer           	! The index for buffer
        INTEGER(4)                               	:: iBeam        		! The index for buffer
		INTEGER(4)                                	:: Counter       		! Counter to determine how many LOS will be used for REWS estimation
		REAL(8)                                   	:: u_est_sum      		! sum of u_est

        ! calculate the estimated u component
        LidarVar%u_est 		= LidarVar%v_los/LidarVar%X_n_L(LidarVar%BeamID+1)
		
        ! first-in-last-out buffer for estimated u component		
        DO iBuffer = LidarVar%nBuffer, 2, -1
           LidarVar%u_est_Buffer(iBuffer) 	= LidarVar%u_est_Buffer(iBuffer-1)
        END DO
		LidarVar%u_est_Buffer(1) 			= LidarVar%u_est 		
		
		! loop over the last full scan in u_est_buffer to sum up all values 
		Counter 			= 0	
		u_est_sum 			= 0		
        DO iBeam  = 1, LidarVar%NumberOfBeams
			IF (LidarVar%u_est_Buffer(iBeam) /= LidarVar%ErrorCode) THEN
				u_est_sum 	= u_est_sum + LidarVar%u_est_Buffer(iBeam)
				Counter 	= Counter+1;
			END IF	
        END DO  
            
        ! now get the rotor-effective wind speed
        IF (Counter /=0) THEN
			LidarVar%REWS 	= u_est_sum/Counter
        ELSE
			LidarVar%REWS  	= LidarVar%ErrorCode
        END IF	

    END SUBROUTINE WindFieldReconstruction
	! -----------------------------------------------------------------------------------
	
   
END MODULE LDP_Subs
