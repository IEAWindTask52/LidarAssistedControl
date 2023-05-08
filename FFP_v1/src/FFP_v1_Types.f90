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
MODULE FFP_Types
! Define Types
USE, INTRINSIC  :: ISO_C_Binding
IMPLICIT NONE

TYPE, PUBLIC :: LidarVariables
	
    ! ---------- variables from avrSWAP ----------
    INTEGER(4)                          :: iStatus 						! Status of simulation [-]
    REAL(8)                             :: DT 							! Time Step of simulation [s]
    REAL(8)                             :: GatesPerBeam              	! number of gates in each lidar beam [-]
	REAL(8)                             :: REWS                         ! Rotor effective wind speed,  estimated by lidar for feedforward control [m/s]

	! ---------- variables from IN file ----------
    REAL(8)                             :: FlagLPF                     	! Enable low-pass filter (flag) [0/1]
    REAL(8)                             :: f_cutoff              		! Corner frequency (-3dB) of the low-pass filter [rad/s]
    REAL(8)                             :: T_buffer                		! Buffer time for filtered REWS signal [s]
	INTEGER(4)                          :: n_StaticPitchCurve           ! Number of points in static pitch curve [-]
    REAL(8), DIMENSION(:), ALLOCATABLE  :: StaticPitch                  ! Pitch angle values in static pitch curve [rad]
    REAL(8), DIMENSION(:), ALLOCATABLE  :: StaticWind                  	! Wind speed  values in static pitch curve [m/s]
	

	! ---------- internal variables ----------	
    REAL(8)                             :: REWS_f                       ! Low pass filtered rotor effective wind speed,  estimated by lidar for feed-forward control, with a buffer size [m/s]
    REAL(8), DIMENSION(:), ALLOCATABLE  :: REWS_f_buffer                ! Buffer of filtered rotor-effective wind speed [m/s]    
    REAL(8)                             :: REWS_b                       ! Buffered and filtered rotor effective wind speed [m/s]
    REAL(8)                             :: FF_Pitch                     ! Feedforward pitch angle [rad]
    REAL(8)                             :: FF_Pitch_old                 ! Previous feedforward pitch angle [rad]
    REAL(8)                             :: FF_PitchRate                 ! Feedforward pitch rate [rad/s]
     
    ! ---------- internal constants ----------
    INTEGER(4)                          :: nBuffer = 2000           	! Size of REWS_f_buffer, 25 seconds at 80 Hz  [-]  
	INTEGER(4)                          :: ErrorCode = 999        		! Error code [-]

	! ---------- indices for avrSWAP ----------
    INTEGER(4)                          :: AvrIndex_REWS
    INTEGER(4)                          :: AvrIndex_REWS_b
    INTEGER(4)                          :: AvrIndex_REWS_f
    INTEGER(4)                          :: AvrIndex_FFrate

	
END TYPE LidarVariables    

TYPE, PUBLIC :: LidarErrorVariables
    ! Error Catching
    INTEGER(4)                      :: size_avcMSG
    INTEGER(C_INT)                  :: aviFAIL             ! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.
    CHARACTER(:), ALLOCATABLE       :: ErrMsg              ! a FORTRAN version of the C string argument (not considered an array here) [subtract 1 for the C null-character]
END TYPE LidarErrorVariables

END MODULE FFP_Types