# LAC SummerGames 2025
The LAC SummerGames 2025 were launched as follow-up to the successful LAC Summer Games 24 to encourage professionals and particularly students in the art of designing and deploying lidar data processing algorithms and lidar-assisted controllers. Participants will be given the opportunity to increase their knowledge in LAC and by that, the SummerGames will trigger creativity and motivate the development of new concepts.

All the necessary information regarding the three different disciplines and the general timeline can be found in the official document: (to be uploaded).

The best way to get started would be to familiarize yourself with the code of the given examples and reproduce the results presented in the official document.

This code is an extention of 
https://github.com/MSCA-LIKE/Baseline-Lidar-assisted-Controller, which has been used in the research funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358. 

Please cite:
- Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci., 8, 149-171, https://doi.org/10.5194/wes-8-149-2023, 2023. 

! License: MIT License
! Copyright (c) 2022 Flensburg University of Applied Sciences, WETI

# How to compile?
The sources for the listed DLLs can be found inside the *SourceCode* folder:
- *FFP_v0.dll*
- *WRAPPER.dll*
- *SignalProvider.dll*

We recommend to use "Cmake"+"Visual Studio"+"Intel Fortran Compiler".

The "Cmake" GUI-based version is freely available from: https://cmake.org/download/

- Step1: Use Cmake to generate Visual Studio project, remember to ensure "CMakeLists.txt" and the "src" folder are in the same folder. See the tutorial here: https://cmake.org/runningcmake/
- Step2: Compile using visual studio. 

Currently, the following combinations have been tested:

- Visual Studio Community 2026 (Version: 18.2.1) + Intel Fortran Compiler IntelLLVM 2025.3.2

The latest version of the compiler is available here: https://www.intel.com/content/www/us/en/developer/tools/oneapi/fortran-compiler-download.html