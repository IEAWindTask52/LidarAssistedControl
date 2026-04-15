! Name:   		Baseline feedforward pitch (FFP) DLL for lidar-assisted feedforward pitch control.
! Authors: 		Feng Guo, David Schlipf from Flensburg University of Applied Sciences, funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358.   
! Target: 		This code aims to provide a reference Lidar-assisted control package for the community. Please cite the following paper if this code is helpful for your research:
! 				Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci. Discuss.
! 				[preprint], https://doi.org/10.5194/wes-2022-62, in review, 2022.    
! Function: 	The FFP module reads in the rotor-effective wind speed from the avrSWAP array, then filters the signal and shifts it in time.
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
               
        !> Gates per beam
        LidarVar%GatesPerBeam    	= NINT(avrSWAP(L + 2)) 	! Number of range gate, for this reference version, only one range gate is supported                                                             

        !> Index for LDP and FFP outputs
        LidarVar%AvrIndex_REWS     	= L + 2 + (LidarVar%GatesPerBeam) + 7            
		LIDARVAR%AvrIndex_REWS_b  	= L + 2 + (LidarVar%GatesPerBeam) + 8
		LIDARVAR%AvrIndex_FFrate   	= L + 2 + (LidarVar%GatesPerBeam) + 9
		LIDARVAR%AvrIndex_REWS_f    = L + 2 + (LidarVar%GatesPerBeam) + 10
		
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
		INTEGER(4)                              	:: iBuffer  
        CHARACTER(*),               PARAMETER       :: RoutineName = 'SetLidarParameters'
		
        ! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF		
		
		! Description:
		print *, '--------------------------------------------------------------------'
		print *, 'A baseline pitch forward controller - v1.1'
		print *, 'Developed by Flensburg University of Applied Sciences, Germany'
		print *, '--------------------------------------------------------------------'
		
		! Read the DLL Parameters specified in the User Interface
		CALL ReadLidarParameterFileSub(LidarVar, accINFILE, NINT(avrSWAP(50)), ErrVar)
		
		! Allocates buffer and initialize it
        IF (.not. allocated(LidarVar%REWS_f_buffer)) THEN
			Allocate(LidarVar%REWS_f_buffer(LidarVar%nBuffer))	
        END IF
		DO iBuffer = 1,LidarVar%nBuffer,1
		   LidarVar%REWS_f_buffer(iBuffer) = LidarVar%REWS
		END DO 		
        
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
        
        !------- Filter and timing -----------------------------
        CALL ParseInput(UnControllerParameters,CurLine,'FlagLPF', accINFILE(1),LidarVar%FlagLPF, ErrVar)
		CALL ParseInput(UnControllerParameters,CurLine,'f_cutoff',accINFILE(1),LidarVar%f_cutoff,ErrVar)
        CALL ParseInput(UnControllerParameters,CurLine,'T_buffer',accINFILE(1),LidarVar%T_buffer,ErrVar)        
        CALL ReadEmptyLine(UnControllerParameters,CurLine)
              
        !------- Static pitch curve ----------------------------
        CALL ReadEmptyLine(UnControllerParameters,CurLine)
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
        INTEGER(4)                                   	:: LPF_inst
        INTEGER(4)                                   	:: iBuffer
		INTEGER(4) 										:: Idx
        REAL(C_FLOAT), INTENT(INOUT) :: avrSWAP(*)   ! The swap array, used to pass data to, and receive data from, the DLL controller.
		CHARACTER(*),               PARAMETER           :: RoutineName = 'CalculateFeedForwardPitchRate'
		    
		! Nothing is done in case of an error
        IF (ErrVar%aviFAIL < 0) THEN
            return
        ENDIF			
        
        ! Low pass filter the REWS
		IF (LidarVar%FlagLPF == 1) THEN
			LPF_inst = 1
			LidarVar%REWS_f      	= LPFilter(LidarVar%REWS, LidarVar%DT, LidarVar%f_cutoff, LidarVar%iStatus, .FALSE., LPF_inst)
		ELSE
			LidarVar%REWS_f      	= LidarVar%REWS
		END IF

        ! first-in-last-out buffer for filtered REWS (in case of Error, last values is repeated)
        DO iBuffer = LidarVar%nBuffer, 2, -1
			LidarVar%REWS_f_Buffer(iBuffer) 	= LidarVar%REWS_f_Buffer(iBuffer-1)
        END DO			
		IF (LidarVar%REWS_f /= LidarVar%ErrorCode) THEN 		
			LidarVar%REWS_f_Buffer(1)  			= LidarVar%REWS_f 
		END IF
	        
		! Index for entry at T_buffer, minimum 1, maximum nBuffer
		Idx = min(max(INT(LidarVar%T_buffer/LidarVar%DT),1),LidarVar%nBuffer)
		
		! Get buffered and filtered REWS from buffer
		LidarVar%REWS_b 			= LidarVar%REWS_f_Buffer(Idx)
		
		! Calculate feedforward pitch angle
		LidarVar%FF_Pitch 			= interp1d(LidarVar%StaticWind,LidarVar%StaticPitch,LidarVar%REWS_b,ErrVar)	
		
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
    ! Low pass filter taken from ROSCO, modified with return in case of an error
    REAL FUNCTION LPFilter(InputSignal, DT, CornerFreq, iStatus, reset, inst)
    ! Discrete time Low-Pass Filter of the form:
    !                               Continuous Time Form:   H(s) = CornerFreq/(1 + CornerFreq)
    !                               Discrete Time Form:     H(z) = (b1z + b0) / (a1*z + a0)
    !
        REAL(8), INTENT(IN)         :: InputSignal
        REAL(8), INTENT(IN)         :: DT                       ! time step [s]
        REAL(8), INTENT(IN)         :: CornerFreq               ! corner frequency [rad/s]
        INTEGER(4), INTENT(IN)      :: iStatus                  ! A status flag set by the simulation as follows: 0 if this is the first call, 1 for all subsequent time steps, -1 if this is the final call at the end of the simulation.
        INTEGER(4), INTENT(INOUT)   :: inst                     ! Instance number. Every instance of this function needs to have an unique instance number to ensure instances don't influence each other.
        LOGICAL(4), INTENT(IN)      :: reset                    ! Reset the filter to the input signal

            ! Local
        REAL(8), DIMENSION(99), SAVE    :: a1                   ! Denominator coefficient 1
        REAL(8), DIMENSION(99), SAVE    :: a0                   ! Denominator coefficient 0
        REAL(8), DIMENSION(99), SAVE    :: b1                    ! Numerator coefficient 1
        REAL(8), DIMENSION(99), SAVE    :: b0                    ! Numerator coefficient 0 

        REAL(8), DIMENSION(99), SAVE    :: InputSignalLast      ! Input signal the last time this filter was called. Supports 99 separate instances.
        REAL(8), DIMENSION(99), SAVE    :: OutputSignalLast ! Output signal the last time this filter was called. Supports 99 separate instances.

            ! Initialization
        IF ((iStatus == 0) .OR. reset) THEN   
            OutputSignalLast(inst) = InputSignal
            InputSignalLast(inst) = InputSignal
            a1(inst) = 2 + CornerFreq*DT
            a0(inst) = CornerFreq*DT - 2
            b1(inst) = CornerFreq*DT
            b0(inst) = CornerFreq*DT
        ENDIF

        ! Define coefficients

        ! Filter
        LPFilter = 1.0/a1(inst) * (-a0(inst)*OutputSignalLast(inst) + b1(inst)*InputSignal + b0(inst)*InputSignalLast(inst))

        ! Save signals for next time step
        InputSignalLast(inst)  = InputSignal
        OutputSignalLast(inst) = LPFilter
        inst = inst + 1

    END FUNCTION LPFilter    
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