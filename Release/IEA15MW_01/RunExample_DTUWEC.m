% IEA15MW_01: IEA 15 MW monopile + perfect wind preview from a single point 
% lidar system. The DTU controller is used instead of the ROSCO controller. 
% Origin and changes in files: see ChangeLog.txt.
% Purpose:
% Here, we use a perfect wind preview to demonstrate that the collective
% pitch feedforward controller (designed with SLOW) is able to reduce
% significantly the rotor speed variation when OpenFAST is disturbed by an
% Extreme Operating Gust. Here, only the rotational GenDOF is enabled.
% Result:       
% Change in rotor over speed:  -96.9 %
% Authors: 		
% Alan Wai Hou Lio, David Schlipf, Feng Guo

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Copy the adequate OpenFAST version to the example folder
FASTexeFile     = 'openfast_x64.exe';
FASTmapFile     = 'MAP_x64.dll';
SimulationName  = 'IEA-15-240-RWT-Monopile_DTUWEC';
copyfile(['..\OpenFAST\',FASTexeFile],FASTexeFile)

%% Run FB
ManipulateTXTFile('DTUWEC.IN','constant    96 1.0;','constant    96 0.0;');     % disable LAC
dos([FASTexeFile,' ',SimulationName,'.fst']);                       % run OpenFAST
movefile([SimulationName,'.outb'],[SimulationName,'_FB.outb'])      % store results

%% Run FBFF  
ManipulateTXTFile('DTUWEC.IN','constant    96 0.0;','constant    96 1.0;');     % enable LAC
dos([FASTexeFile,' ',SimulationName,'.fst']);                       % run OpenFAST
movefile([SimulationName,'.outb'],[SimulationName,'_FBFF.outb'])    % store results

%% Clean up
delete(FASTexeFile)

%% Comparison
% read in data
FB              = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF            = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF.outb']);

% Plot 
figure('Name','Simulation results')

subplot(4,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(FBFF.Time,     FBFF.VLOS01LI);
legend('Hub height wind speed','Vlos')
ylabel('[m/s]');
legend('Wind1VelX','VLOS01LI')

subplot(4,1,2);
hold on; grid on; box on
plot(FB.Time,       FB.BldPitch1);
plot(FBFF.Time,     FBFF.BldPitch1);
ylabel({'BldPitch1'; '[deg]'});
legend('feedback only','feedback-feedforward')

subplot(4,1,3);
hold on; grid on; box on
plot(FB.Time,       FB.RotSpeed);
plot(FBFF.Time,     FBFF.RotSpeed);
ylabel({'RotSpeed';'[rpm]'});

subplot(4,1,4);
hold on; grid on; box on
plot(FB.Time,       FB.TwrBsMyt/1e3);
plot(FBFF.Time,     FBFF.TwrBsMyt/1e3);
ylabel({'TwrBsMyt';'[MNm]'});

xlabel('time [s]')
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([100 130])

% display results
RatedRotorSpeed = 7.56; % [rpm]
t_Start         = 100;  % [s]
fprintf('Change in rotor over speed:  %4.1f %%\n',...
    (max(abs(FBFF.RotSpeed(FBFF.Time>=t_Start)-RatedRotorSpeed))/...
     max(abs(FB.RotSpeed  (FB.Time  >=t_Start)-RatedRotorSpeed))-1)*100)