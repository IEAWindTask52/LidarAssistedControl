! Name:   		PFC (Primary Frequency Control) DLL.
! Authors: 		Julius Preuschoff, David Schlipf from Flensburg University of Applied Sciences, funded by ABBA -- Adaptive operating strategies for existing wind turbines.   
! Target: 		This code aims to provide a simple case of primary frequency control in conjuction with lidar assisted control for the community.   
! Function: 	The DLL chain is designed to make the PFC processing and other algorithms more independent from the feedback controller and modified OpenFAST versions. 
! Reference:	The subroutines rely on the legacy Bladed style data interface. See the Bladed manual for more detail.    
! 				The code is written based on the source code of the WRAPPER. https://github.com/MSCA-LIKE/Baseline-Lidar-assisted-Controller, 2022.
! License: 		MIT License
! Copyright (c) 2026 Flensburg University of Applied Sciences, WETI
! -------------------------------------------------------------------------------------------
      
!=======================================================================
MODULE PFC_Types
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  TYPE :: PFCStateType
    LOGICAL :: initialized = .FALSE.

    ! swap array indices
    INTEGER :: swap_in_index  = -1
    INTEGER :: swap_out_index = -1

    ! calculation parameters
    REAL(8) :: gain         = 0.0D0
    REAL(8) :: H            = 0.0D0
    REAL(8) :: S            = 0.0D0
    REAL(8) :: f0           = 0.0D0
    REAL(8) :: omega_rated  = 0.0D0

    ! runtime memory for differentiation
    LOGICAL :: first_step = .TRUE.
    REAL(8) :: t_prev     = 0.0D0
    REAL(8) :: x_prev     = 0.0D0

    ! bookkeeping
    CHARACTER(:), ALLOCATABLE :: param_path
  END TYPE PFCStateType

  TYPE(PFCStateType), SAVE :: SPState

END MODULE PFC_Types