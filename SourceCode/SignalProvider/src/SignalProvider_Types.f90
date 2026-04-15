! Name:   		SignalProvider DLL.
! Authors: 		Julius Preuschoff, David Schlipf from Flensburg University of Applied Sciences, funded by ABBA -- Adaptive operating strategies for existing wind turbines.   
! Target: 		This code aims to provide a reference Lidar-assisted control package for the community.   
! Function: 	The DLL chain is designed to make the lidar data processing or other algorithms more independent from the feedback controller and modiefied OpenFAST versions. 
! Reference:	The subroutines rely on the legacy Bladed style data interface. See the Bladed manual for more detail.    
! 				The code is written based on the source code of the WRAPPER. https://github.com/MSCA-LIKE/Baseline-Lidar-assisted-Controller, 2022.
! License: 		MIT License
! Copyright (c) 2026 Flensburg University of Applied Sciences, WETI
! -------------------------------------------------------------------------------------------
      
!=======================================================================
MODULE SignalProvider_Types
	USE, INTRINSIC :: ISO_C_BINDING
	IMPLICIT NONE

	TYPE :: SignalProviderStateType
		LOGICAL :: initialized = .FALSE.
		INTEGER :: swap_out_index = -1
		LOGICAL :: do_interp = .TRUE.
		REAL(8) :: preview_time = 0.0D0
		
		INTEGER :: npts = 0
		INTEGER :: time_col = -1
		INTEGER :: value_col = -1
		
		REAL(8), ALLOCATABLE :: t_csv(:)
		REAL(8), ALLOCATABLE :: v_csv(:)
		
		CHARACTER(:), ALLOCATABLE :: param_path
		CHARACTER(:), ALLOCATABLE :: csv_path
	END TYPE SignalProviderStateType

	TYPE(SignalProviderStateType), SAVE :: SPState

END MODULE SignalProvider_Types