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
MODULE LDP_Types
! Define Types
USE, INTRINSIC  :: ISO_C_Binding
IMPLICIT NONE

TYPE, PUBLIC :: LidarVariables

    ! ---------- variables from avrSWAP ----------
    INTEGER(4)                          :: iStatus 						! Status of simulation [-]
	INTEGER(4)                          :: NewMeasurementFlag          	! New measurement (flag) [-]
    INTEGER(4)                          :: BeamID                       ! Beam ID of current lidar data [-]
	INTEGER(4)                          :: GatesPerBeam                 ! Number of range gates in each lidar beam [-]
	REAL(8), DIMENSION(:), ALLOCATABLE 	:: v_los                        ! Line-of-sight wind speeds measurements [m/s]

	! ---------- variables from IN file ----------
    INTEGER(4)                          :: NumberOfBeams                ! Number of beams measuring at different directions	[-]
    REAL(8)  							:: AngleToCenterline            ! Angle to centerline [deg]
	INTEGER(4)  						:: IndexGate           			! Index of range gates used in the LDP [-]

	! ---------- internal variables ----------	
    REAL(8)                             :: REWS                         ! Rotor-effective wind speed,  estimated by lidar for feedforward control [m/s]
    REAL(8)                             :: u_est                        ! Estimated u component [m/s]
	REAL(8), DIMENSION(:), ALLOCATABLE 	:: u_est_Buffer             	! Buffer of estimated u component [m/s]
     
    ! ---------- internal constants ----------
	INTEGER(4)                          :: ErrorCode = 999        		! Error code [-]

	! ---------- indices for avrSWAP ----------
	INTEGER(4)                          :: AvrIndexREWS 				! Index for REWS [-]

    
END TYPE LidarVariables    

TYPE, PUBLIC :: LidarErrorVariables
    ! Error Catching
    INTEGER(4)                      :: size_avcMSG
    INTEGER(C_INT)                  :: aviFAIL             				! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.
    CHARACTER(:), ALLOCATABLE       :: ErrMsg              				! A FORTRAN version of the C string argument (not considered an array here) [subtract 1 for the C null-character]
END TYPE LidarErrorVariables

END MODULE LDP_Types