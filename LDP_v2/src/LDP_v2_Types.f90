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
MODULE LDP_Types
! Define Types
USE, INTRINSIC  :: ISO_C_Binding
IMPLICIT NONE

TYPE, PUBLIC :: LidarVariables

    ! ---------- variables from avrSWAP ----------
    INTEGER(4)                          :: iStatus 						! Status of simulation [-]
	INTEGER(4)                          :: NewMeasurementFlag          	! new measurement (flag) [-]
    INTEGER(4)                          :: BeamID                       ! Beam ID of current lidar data [-]
	INTEGER(4)                          :: GatesPerBeam                 ! number of gates in each lidar beam [-]
	REAL(8)                             :: v_los                        ! Line-of-sight wind speeds measurement [m/s]
    REAL(8)                             :: Lidar_Roll                   ! Lidar roll angle [deg]
    REAL(8)                             :: Lidar_Pitch                  ! Lidar pitch angle [deg]
    REAL(8)                             :: Lidar_Yaw                    ! Lidar yaw angle [deg]
    REAL(8)                             :: Lidar_Xd                     ! Lidar velocity in X direction [m/s]
    REAL(8)                             :: Lidar_Yd                     ! Lidar velocity in Y direction [m/s]
    REAL(8)                             :: Lidar_Zd                     ! Lidar velocity in Z direction [m/s]	

	! ---------- variables from IN file ----------
    INTEGER(4)                          :: NumberOfBeams                ! Number of beams measuring at different directions	[-]
    REAL(8), DIMENSION(:), ALLOCATABLE  :: Lidar_Azimuth                ! Lidar beam azimuth angles [rad]
    REAL(8), DIMENSION(:), ALLOCATABLE  :: Lidar_Elevation              ! Lidar beam elevation angles [rad]
	INTEGER(4)                          :: MC_Mode                		! Motion Compensation mode {0: no MC, 1: full MC)	[-]     

	! ---------- internal variables ----------	
    REAL(8), DIMENSION(:), ALLOCATABLE 	:: X_n_L                		! Normalized lidar beam vector in lidar coordinate system, x-component [-]
    REAL(8), DIMENSION(:), ALLOCATABLE 	:: Y_n_L                		! Normalized lidar beam vector in lidar coordinate system, y-component [-]
    REAL(8), DIMENSION(:), ALLOCATABLE 	:: Z_n_L                		! Normalized lidar beam vector in lidar coordinate system, z-component [-]	
    REAL(8)                             :: REWS                         ! Rotor-effective wind speed,  estimated by lidar for feedforward control [m/s]
    REAL(8)                             :: u_est                        ! Estimated u component [m/s]
	REAL(8), DIMENSION(:), ALLOCATABLE 	:: u_est_Buffer             	! Buffer of estimated u component [m/s]
     
    ! ---------- internal constants ----------
    INTEGER(4)                          :: nBuffer = 50           		! Size of u_est_Buffer [-]
	INTEGER(4)                          :: ErrorCode = 999        		! Error code [-]

	! ---------- indices for avrSWAP ----------
	INTEGER(4)                          :: AvrIndex_REWS


    
END TYPE LidarVariables    

TYPE, PUBLIC :: LidarErrorVariables
    ! Error Catching
    INTEGER(4)                      :: size_avcMSG
    INTEGER(C_INT)                  :: aviFAIL             				! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.
    CHARACTER(:), ALLOCATABLE       :: ErrMsg              				! a FORTRAN version of the C string argument (not considered an array here) [subtract 1 for the C null-character]
END TYPE LidarErrorVariables

END MODULE LDP_Types