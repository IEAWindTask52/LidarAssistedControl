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
   
MODULE WRAPPER_Types
! Define Types
USE, INTRINSIC  :: ISO_C_Binding
IMPLICIT NONE
    
    TYPE LidarVariables
      INTEGER(4)                                    :: NumberOfsubDLLs
      INTEGER(C_INTPTR_T)                           :: FileAddr                   	!< The address of file FileName.         (RETURN value from LoadLibrary ) [Windows]
      TYPE(C_PTR)                                   :: FileAddrX = C_NULL_PTR      	!< The address of file FileName.         (RETURN value from dlopen ) [Linux]
      TYPE(C_FUNPTR), DIMENSION(:), ALLOCATABLE     :: ProcAddr  					!< The address of procedure ProcName.    (RETURN value from GetProcAddress or dlsym) [initialized to Null for pack/unpack] C_NULL_FUNPTR

      CHARACTER(1024), DIMENSION(:), ALLOCATABLE    :: DLLFileName  				!< The name of the DLL file including the full path to the current working directory.
      CHARACTER(1024), DIMENSION(:), ALLOCATABLE    :: DLLInputFileName 			!< The name of the DLL input file including the full path to the current working directory.
      
    END TYPE LidarVariables
    
    TYPE, PUBLIC :: LidarErrorVariables
    ! Error Catching
    INTEGER(4)                      :: size_avcMSG
    INTEGER(C_INT)                  :: aviFAIL             ! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.
    CHARACTER(:), ALLOCATABLE       :: ErrMsg              ! a Fortran version of the C string argument (not considered an array here) [subtract 1 for the C null-character]
    END TYPE LidarErrorVariables
    
END MODULE WRAPPER_Types