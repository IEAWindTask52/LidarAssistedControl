! Name:   		Baseline feedforward pitch (FFP) DLL for lidar-assisted feedforward pitch control.
! Authors: 		Feng Guo, David Schlipf, Julius Preuschoff from Flensburg University of Applied Sciences, funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358.   
! Target: 		This code aims to provide a reference Lidar-assisted control package for the community. Please cite the following paper if this code is helpful for your research:
! 				Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci. Discuss.
! 				[preprint], https://doi.org/10.5194/wes-2022-62, in review, 2022.    
! Function: 	The FFP module reads in the rotor-effective wind speed from the avrSWAP array.
! 				It eventually returns the feedforward pitch time derivative (rate), which is written into the avrSWAP array. 				
! Reference:	The subroutines rely on the legacy Bladed style data interface. See the Bladed manual for more detail.    
! 				The code is written based on the source code of ROSCO. Version 2.4.1, https://github.com/NREL/ROSCO, 2021. by NREL.
! License: 		MIT License
! Copyright (c) 2022 Flensburg University of Applied Sciences, WETI
! -------------------------------------------------------------------------------------------  

!=======================================================================
MODULE FFP_Subs
!...............................................................................................................................
    !USE Constants   ! the constants from the ROSCO will be used
    USE :: FFP_Types
	USE :: FFP_Helpers
    
    IMPLICIT NONE
	
CONTAINS
    
	! -----------------------------------------------------------------------------------
    ! Read avrSWAP array into the local Lidar Variables
    SUBROUTINE ReadAvrSWAP(avrSWAP, LidarVar)
	
        USE FFP_Types, ONLY : LidarVariables

        REAL(C_FLOAT), INTENT(INOUT) 		:: avrSWAP(*)   ! The swap array, used to pass data to, and receive data from, the DLL controller.
        TYPE(LidarVariables), INTENT(INOUT) :: LidarVar
        INTEGER(4)                      	:: L           	! Record number for start of Lidar data

		! Load variables from calling program (See Appendix A of Bladed User's Guide):
        LidarVar%iStatus            = NINT(avrSWAP(1))      ! Initialization status  
        LidarVar%DT                 = avrSWAP(3)            ! Time step
         
        ! --- read and set the lidar variables
        L                           = NINT(avrSWAP(63))     ! The index in the array where the lidar related data begins 
                                                                            
        !> Index for LDP and FFP outputs
        LidarVar%AvrIndex_REWS     	= L + 2 + 7            
		LIDARVAR%AvrIndex_FFrate   	= L + 2 + 9
		
		!> load rotor-effective wind speed
		LidarVar%REWS            	= avrSWAP(LIDARVAR%AvrIndex_REWS)

    END SUBROUTINE ReadAvrSWAP
	! -----------------------------------------------------------------------------------    
    	
	! -----------------------------------------------------------------------------------
    ! Get the sub DLL information 
    SUBROUTINE SetLidarParameters(avrSWAP, accINFILE, size_avcMSG, LidarVar, ErrVar)
        
		USE FFP_Types, ONLY : LidarErrorVariables, LidarVariables

        REAL(C_FLOAT),              INTENT(INOUT)	:: avrSWAP(*)          			! The swap array, used to pass data to, and receive data from, the DLL controller.
        CHARACTER(C_CHAR),          INTENT(IN   )	:: accINFILE(NINT(avrSWAP(50))) ! The name of the parameter input file
        INTEGER(4),                 INTENT(IN   )	:: size_avcMSG
        TYPE(LidarErrorVariables),  INTENT(INOUT)   :: ErrVar
        TYPE(LidarVariables),       INTENT(INOUT)   :: LidarVar
        CHARACTER(*),               PARAMETER       :: RoutineName = 'SetLidarParameters'
		
        ! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF		
		
		! Description:
		print *, '--------------------------------------------------------------------'
		print *, 'A baseline pitch forward controller - v0.1'
		print *, 'Developed by Flensburg University of Applied Sciences, Germany'
		print *, '--------------------------------------------------------------------'
		
		! Read the DLL Parameters specified in the User Interface
		CALL ReadLidarParameterFileSub(LidarVar, accINFILE, NINT(avrSWAP(50)), ErrVar)	
        
		! Add RoutineName to error message
		IF (ErrVar%aviFAIL < 0) THEN
			ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
			RETURN
		ENDIF
      
    END SUBROUTINE SetLidarParameters
	! -----------------------------------------------------------------------------------

    ! -----------------------------------------------------------------------------------
    ! Read all constant control parameters from DISCON.IN parameter file
    SUBROUTINE ReadLidarParameterFileSub(LidarVar, accINFILE, accINFILE_size, ErrVar)
        
        USE, INTRINSIC :: ISO_C_Binding
        USE FFP_Types, ONLY : LidarErrorVariables,LidarVariables

        INTEGER(4)                                      :: accINFILE_size               ! size of DISCON input filename, INTENT(IN) here??
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
              
        !------- Static pitch curve ----------------------------
        CALL ParseInput(UnControllerParameters,CurLine,'n_StaticPitchCurve',accINFILE(1), LidarVar%n_StaticPitchCurve,ErrVar)
        CALL ParseAry(  UnControllerParameters,CurLine,'StaticWind', LidarVar%StaticWind, LidarVar%n_StaticPitchCurve,accINFILE(1),ErrVar)
        CALL ParseAry(  UnControllerParameters,CurLine,'StaticPitch',LidarVar%StaticPitch,LidarVar%n_StaticPitchCurve,accINFILE(1),ErrVar)
       
        ! Close Input File
        CLOSE(UnControllerParameters)
        
        ! Add RoutineName to error message
        IF (ErrVar%aviFAIL < 0) THEN
            ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
        ENDIF

    END SUBROUTINE ReadLidarParameterFileSub
    ! -----------------------------------------------------------------------------------
    	
    ! -----------------------------------------------------------------------------------
	! Calculate the feedforward pitch rate
    SUBROUTINE CalculateFeedForwardPitchRate(avrSWAP, LidarVar, ErrVar)
    
        USE FFP_Types, ONLY : LidarErrorVariables, LidarVariables
        
        TYPE(LidarErrorVariables),  INTENT(INOUT)       :: ErrVar
		TYPE(LidarVariables), INTENT(INOUT)          	:: LidarVar
        REAL(C_FLOAT), INTENT(INOUT)                    :: avrSWAP(*)   ! The swap array, used to pass data to, and receive data from, the DLL controller.
		CHARACTER(*),               PARAMETER           :: RoutineName = 'CalculateFeedForwardPitchRate'
		    
		! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF			
		
		! Calculate feedforward pitch angle
		LidarVar%FF_Pitch 			= interp1d(LidarVar%StaticWind,LidarVar%StaticPitch,LidarVar%REWS,ErrVar)	
		
        ! Calculate feedforward pitch rate
        IF (LidarVar%iStatus == 0) THEN ! not initialized yet
			LidarVar%FF_Pitch_old 	= LidarVar%FF_Pitch
			LidarVar%FF_PitchRate 	= 0
        ELSE 
			LidarVar%FF_PitchRate 	= (LidarVar%FF_Pitch-LidarVar%FF_Pitch_old)/LidarVar%DT 
			LidarVar%FF_Pitch_old 	= LidarVar%FF_Pitch
        END IF
        
		! Add RoutineName to error message
        IF (ErrVar%aviFAIL < 0) THEN
            ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
        ENDIF		
				
    END SUBROUTINE CalculateFeedForwardPitchRate  
    ! -----------------------------------------------------------------------------------
	
    ! -----------------------------------------------------------------------------------
	! Interpolation interp1d from ROSCO
	REAL FUNCTION interp1d(xData, yData, xq, ErrVar)    
        
        USE FFP_Types, ONLY : LidarErrorVariables
        IMPLICIT NONE

        ! Inputs
        REAL(8), DIMENSION(:), INTENT(IN)       	:: xData        ! Provided x data (vector), to be interpolated
        REAL(8), DIMENSION(:), INTENT(IN)       	:: yData        ! Provided y data (vector), to be interpolated
        REAL(8), INTENT(IN)                     	:: xq           ! x-value for which the y value has to be interpolated
        INTEGER(4)                              	:: I            ! Iteration index

        ! Error Catching
        TYPE(LidarErrorVariables), INTENT(INOUT)	:: ErrVar
        INTEGER(4)                              	:: I_DIFF

        CHARACTER(*), PARAMETER                 	:: RoutineName = 'interp1d'

		! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF
		
        ! Catch Errors
        ! Are xData and yData the same size?
        IF (SIZE(xData) .NE. SIZE(yData)) THEN
            ErrVar%aviFAIL = -1
            ErrVar%ErrMsg  = ' xData and yData are not the same size'
            WRITE(ErrVar%ErrMsg,"(A,I2,A,I2,A)") " SIZE(xData) =", SIZE(xData), & 
            ' and SIZE(yData) =', SIZE(yData),' are not the same'
        END IF

        ! Is xData non decreasing
        DO I_DIFF = 1, size(xData) - 1
            IF (xData(I_DIFF + 1) - xData(I_DIFF) <= 0) THEN
                ErrVar%aviFAIL = -1
                ErrVar%ErrMsg  = ' xData is not strictly increasing'
                EXIT 
            END IF
        END DO
        
        ! Interpolate
        IF (xq <= MINVAL(xData)) THEN
            interp1d = yData(1)
        ELSEIF (xq >= MAXVAL(xData)) THEN
            interp1d = yData(SIZE(xData))
        ELSE
            DO I = 1, SIZE(xData)
                IF (xq <= xData(I)) THEN
                    interp1d = yData(I-1) + (yData(I) - yData(I-1))/(xData(I) - xData(I-1))*(xq - xData(I-1))
                    EXIT
                ELSE
                    CONTINUE
                END IF
            END DO
        END IF

        ! Add RoutineName to error message
        IF (ErrVar%aviFAIL < 0) THEN
            ErrVar%ErrMsg = RoutineName//':'//TRIM(ErrVar%ErrMsg)
        ENDIF
        
    END FUNCTION interp1d
    ! -----------------------------------------------------------------------------------
	
END MODULE FFP_Subs