% make sure the OpenFAST directory where the FAST_SFunc.mex* file is located
% is in the MATLAB path (also make sure any other OpenFAST library files that

% Feng: How to run?  Put all files in this folder into the OpenFAST input folder
% e.g. "\LidarAssistedControl\Release\IEA15MW_01\"

% these variables are defined in the OpenLoop model's FAST_SFunc block:

% IEA15MW_01: IEA 15 MW monopile + perfect wind preview from a single point 
% lidar system.
% Origin and changes in files: see ChangeLog.txt.
% Purpose:
% An example showing how to run the OpenFAST simulink version. 
% The simulink results are compared to executable results. 
% Feng Guo @ Shanghai Jiao Tong University


%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Copy the adequate OpenFAST version to the example folder
FASTexeFile     = 'openfast_x64.exe';
SimulationName  = 'IEA-15-240-RWT-Monopile';
copyfile(['..\OpenFAST\',FASTexeFile],FASTexeFile)

% copy files needed for simulink sim
copyfile(['..\OpenFAST-Simulink\'])

%% Run FB with exe
ManipulateTXTFile('ROSCO_v2d6.IN','1 ! FlagLAC','0 ! FlagLAC');     % disable LAC
dos([FASTexeFile,' ',SimulationName,'.fst']);                       % run OpenFAST
movefile([SimulationName,'.outb'],[SimulationName,'_FB.outb'])      % store results

%% Run FB with simulink
FAST_InputFileName = 'IEA-15-240-RWT-Monopile.fst';
TMax               = 30; % seconds
sim('OpenLoop.mdl',[0,TMax]);

movefile([SimulationName,'.SFunc.outb'],[SimulationName,'_SFunc_FB.outb'])      % store results


%% Clean up
delete(FASTexeFile)
delete('FAST_SFunc.mexw64')
delete('OpenLoop.mdl')
delete('OpenLoop.slxc')
delete('OpenFAST-Simulink_x64.dll')
delete('MAP_x64.dll')


%% Comparison
% read in data
FBexe              = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBsimulink         = ReadFASTbinaryIntoStruct([SimulationName,'_SFunc_FB.outb']);


% Plot 
figure('Name','Simulation results')

subplot(4,1,1);
hold on; grid on; box on
plot(FBexe.Time,       FBexe.Wind1VelX);
plot(FBsimulink.Time,  FBsimulink.Wind1VelX,'--');
legend('exe','simulink')
ylabel('[m/s]');

subplot(4,1,2);
hold on; grid on; box on
plot(FBexe.Time,       FBexe.BldPitch1);
plot(FBsimulink.Time,     FBsimulink.BldPitch1,'--');
ylabel({'BldPitch1'; '[deg]'});

subplot(4,1,3);
hold on; grid on; box on
plot(FBexe.Time,       FBexe.RotSpeed);
plot(FBsimulink.Time,     FBsimulink.RotSpeed,'--');
ylabel({'RotSpeed';'[rpm]'});

subplot(4,1,4);
hold on; grid on; box on
plot(FBexe.Time,       FBexe.TwrBsMyt/1e3);
plot(FBsimulink.Time,     FBsimulink.TwrBsMyt/1e3,'--');
ylabel({'TwrBsMyt';'[MNm]'});

xlabel('time [s]')
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([0 30])
