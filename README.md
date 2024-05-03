# LAC SummerGames 2024
The LAC SummerGames 2024 were launched to encourage professionals and particularly students in the art of designing and deploying lidar data processing algorithms and lidar-assisted controllers. Participants will be given the opportunity to increase their knowledge in LAC and by that, the SummerGames will trigger creativity and motivate the development of new concepts.

All the necessary information regarding the three different disciplines and the general timeline can be found in the official document: https://zenodo.org/records/10931629.

The best way to get started would be to familiarize yourself with the code of the given examples and reproduce the results presented in the official document.

# Lidar-Assisted-Control 
A lidar-assisted wind turbine control code environment for the IEA Wind Task 52 (Lidar) community. 

This code is an extention of 
https://github.com/MSCA-LIKE/Baseline-Lidar-assisted-Controller, which has been used in the research funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358, see Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci., 8, 149–171, https://doi.org/10.5194/wes-8-149-2023, 2023. 

The code uses the Bladed style interface, which is responsible for exchanging variables between the OpenFAST executable and the external controllers compiled as
Dynamic Link Library(DLL).  The functions of each DLL is described below:

- WRAPPER.dll: The master (wrapper) DLL.  It calls sub-DLLs by a specific sequence specified in the "WRAPPER.IN" input file.
- LDP.dll:     The LDP module reads in LOS measurements from lidar and estimates the rotor effective wind speed, which will eventually be written to the avrSWAP array.
- FFP.dll:     The FFP module reads in the rotor effective wind speed from the avrSWAP array, then filter the signal and shifts it in time.  It eventually returns the feed-forward pitch time derivative (rate), which is written into the avrSWAP array.     
- ROSCO.dll:   The ROSCO.  Version 2.6.0 controller, https://github.com/NREL/ROSCO  by NREL.  The pitch controller is modified to accept the pitch forward rate signal.

Authors: Feng Guo and David Schlipf 
Flensburg University of Applied Sciences, sowento GmbH

Please cite:
- Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci., 8, 149–171, https://doi.org/10.5194/wes-8-149-2023, 2023. 
- Schlipf, D., Guo, F., Raach, S., Lemmer, F.: A Tutorial on Lidar-Assisted Control for Floating Offshore Wind Turbines, accepted at the American Control Conference 2023.

! License: MIT License
! Copyright (c) 2022 Flensburg University of Applied Sciences, WETI

# How to Compile
We recommend to use "Cmake"+"Visual Studio"+"Intel Fortran Compiler".

The "Cmake" GUI-based version is freely available from: https://cmake.org/download/

- Step1: Use Cmake to generate Visual Studio project, remember to ensure "CMakeLists.txt" and the "src" folder are in the same folder. See the tutorial here: https://cmake.org/runningcmake/
- Step2: Compile using visual studio. 

Currently, the following combinations have been tested:

- Visual Studio 2015 Community+Intel Fortran Compiler 2019.
- Visual Studio 2017 Community+Intel Fortran Compiler 2019(available here: https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran).

The pre-compiled DLLs in the "Release" folder are compiled using Visual Studio 2015 and Fortran compiler 2019.  If your system reports the error that the DLL file could not be found, installing Visual Studio 2015 or a newer version may solve the issue. 

# Examples
We added the following examples:
- IEA15MW_01: IEA 15 MW monopile + perfect wind preview from a single point lidar system.
- IEA15MW_02: IEA 15 MW floating + perfect wind preview from a single point lidar system.
- IEA15MW_03: IEA 15 MW monopile + realistic wind preview from a nacelle-based lidar system, single wind speed.
- IEA15MW_04: IEA 15 MW floating + realistic wind preview from a nacelle-based lidar system, single wind speed.

# For collaborators
When you make new DLLs or modify one of the DLLs, and you want to push the source code, please only push the "CMakeLists.txt" file and the "src" folder containing all source codes.

