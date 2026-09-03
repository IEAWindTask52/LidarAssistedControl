# LAC SummerGames 2026
The LAC SummerGames 2026 were launched as follow-up to the successful LAC Summer Games 24 and 25 to encourage professionals and particularly students in the art of designing and deploying lidar data processing algorithms and lidar-assisted controllers. Participants will be given the opportunity to increase their knowledge in LAC and by that, the SummerGames will trigger creativity and motivate the development of new concepts.

All the necessary information regarding the two different disciplines and the general timeline can be found in the official [document](https://zenodo.org/records/21728616).

The best way to get started would be to familiarize yourself with the code of the given examples and reproduce the results presented in the official [document](https://zenodo.org/records/21728616).

This code is an extension of 
https://github.com/MSCA-LIKE/Baseline-Lidar-assisted-Controller, which has been used in the research funded by LIKE -- Lidar Knowledge Europe, grant agreement No. 858358. 

Please cite:
- Guo, F., Schlipf, D., and Cheng, P. W.: Evaluation of lidar-assisted wind turbine control under various turbulence characteristics, Wind Energ. Sci., 8, 149-171, https://doi.org/10.5194/wes-8-149-2023, 2023. 

! License: MIT License
! Copyright (c) 2022 Flensburg University of Applied Sciences, WETI

# How to get started?
You are already in the right repository and branch, you can clone this right away using git, GithubDesktop or by downloading the *.zip file. 

In this repository, we provide 2 scripts for LAC one Matlab- and one Python-script doing the same. Inside the UltraMarathon folder, the shared *data* folder holds the measurement data used by both, while the Matlab and Python versions each live in their own self-contained subfolder.
- To get started with the Matlab script you need to navigate to the UltraMarathon/matlab folder and then you can reproduce the results as in the description document via *RunUltraMarathon.m* script. The script is using functions that are provided in the functions folder and the data is loaded from the ../data folder, both folders are added to your path.
- To get started with Python scripts unlike Matlab users need to install various modules like numpy, scipy, h5py, matplotlib and rainflow. To make it simpler we have provided a *setup/setup_python.py* script (inside UltraMarathon/python) that installs all the necessary modules into a local virtual environment, in addition it gives a short introduction how setup to run the script in the bash or IDE's (VS Code and PyCharm). Then the *RunUltraMarathon.py* works similar to the *.m* version. 

You can now freely implement your own controller or lidar data processing algorithms. In the main program you are only allowed to work between the symbols >  < as shown in the below Matlab example. Inside the block you have the measurements `y_ThisStep`, the lidar signals and your own past values available, but neither the full turbine state nor the true wind speed:
````
    % >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    % First discipline (Preview Quality): Provide v_0L(i_t)  only based on current (i_t) or past lidar signals: isValid, beamID, lineOfSightWindSpeed
    % Second discipline (Load Reduction): Provide u_ThisStep only based on current (i_t) or past lidar signals and turbine signals from y_ThisStep

    % simple lidar data processing
    v_0L(i_t)               = LDP_v3(isValid(i_t,IndexGate),beamID(i_t),lineOfSightWindSpeed(i_t,IndexGate),dt,LDP);

    % calculate combined feedback-feedforward controller
    WindAcceleration        = (v_0L(i_t)-v_0L(max(i_t-1,1)))/dt;
    u_FF_ThisStep           = WindAcceleration*GradientStaticPitch; % simple collective pitch feedforward controller
    y_ThisStep              = y_LA(i_t,:);
    u_ThisStep              = FBController(y_ThisStep,u_FF_ThisStep,dt,Parameter);
    % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
````

# Support
If you have questions regarding the Summer Games in general or regarding the code which might be interesting for others as well, please use our [Forum](https://github.com/IEAWindTask52/LidarAssistedControl/discussions) on GitHub.

If you require further support, please don't hesitate to contact our support team via email:
- [Felix Lehmann](mailto:felix.lehmann@hs-flensburg.de)
- [Simon Weich](mailto:simon.weich@hs-flensburg.de)
- [David Schlipf](mailto:david.schlipf@hs-flensburg.de)
