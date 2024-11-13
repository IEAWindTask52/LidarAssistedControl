% IEA15MW_02: IEA 15 MW floating + perfect wind preview from a single point 
% lidar system.
% Origin and changes in files: see ChangeLog.txt.
% Purpose:
% Here, we use a perfect wind preview to demonstrate that the collective
% pitch feedforward controller (designed with SLOW) together with a motion
% compensation (MC) is able to reduce significantly the rotor speed 
% variation when OpenFAST is disturbed by an Extreme Operating Gust. 
% Here, all DOFs are enabled. If no MC is applied, the system is instable.
% Result:       
% Change in platform pitch amplitude (max-min) from FB to FBFF:  128.5 %
% Change in platform pitch amplitude (max-min) from FB to FBFFMC:  -71.5 %
% Authors: 		
% David Schlipf, Feng Guo, Frank Lemmer

%% Setup
clearvars; 
close all; 
clc;
addpath('..\MatlabFunctions')

% Copy the adequate OpenFAST version to the example folder
FASTexeFile     = 'openfast_x64.exe';
SimulationName  = 'IEA-15-240-RWT-UMaineSemi';
copyfile(['..\OpenFAST\',FASTexeFile],FASTexeFile)

%% Run FB
ManipulateTXTFile('ROSCO_v2d6.IN','1 ! FlagLAC','0 ! FlagLAC');     % disable LAC
dos([FASTexeFile,' ',SimulationName,'.fst']);                       % run OpenFAST
movefile([SimulationName,'.outb'],[SimulationName,'_FB.outb'])      % store results

%% Run FBFF without motion compensation 
ManipulateTXTFile('ROSCO_v2d6.IN','0 ! FlagLAC','1 ! FlagLAC');     % enable LAC
ManipulateTXTFile('LDP_v2.IN',    '1 ! MC_Mode','0 ! MC_Mode');     % disable MC
dos([FASTexeFile,' ',SimulationName,'.fst']);                       % run OpenFAST
movefile([SimulationName,'.outb'],[SimulationName,'_FBFF.outb'])    % store results
movefile([SimulationName,'.RO.dbg'],[SimulationName,'_FBFF.dbg'])   % store rosco output file

%% Run FBFF with motion compensation 
ManipulateTXTFile('LDP_v2.IN',    '0 ! MC_Mode','1 ! MC_Mode');     % enable MC
dos([FASTexeFile,' ',SimulationName,'.fst']);                       % run OpenFAST
movefile([SimulationName,'.outb'],[SimulationName,'_FBFFMC.outb']) 	% store results
movefile([SimulationName,'.RO.dbg'],[SimulationName,'_FBFFMC.dbg']) % store rosco output file

%% Clean up
delete(FASTexeFile)

%% Comparison
% read in data
FB              = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF            = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF.outb']);
FBFFMC          = ReadFASTbinaryIntoStruct([SimulationName,'_FBFFMC.outb']);
R_FBFF          = ReadROSCOtextIntoStruct([SimulationName,'_FBFF.dbg']);
R_FBFFMC        = ReadROSCOtextIntoStruct([SimulationName,'_FBFFMC.dbg']);

% Plot  
figure('Name','Simulation results');
n = 4;

MyAxes(1) = subplot(n,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(R_FBFF.Time, 	R_FBFF.REWS);
plot(R_FBFFMC.Time,	R_FBFFMC.REWS);
legend('Hub height wind speed','REWS feedback-feedforward','REWS feedback-feedforward with MC')
ylabel('[m/s]');

MyAxes(3) = subplot(n,1,2);
hold on; grid on; box on
plot(FB.Time,       FB.BldPitch1);
plot(FBFF.Time,     FBFF.BldPitch1);
plot(FBFFMC.Time,	FBFFMC.BldPitch1);
ylabel({'BldPitch1'; ' [deg]'});
legend('feedback only','feedback-feedforward','feedback-feedforward with MC')

MyAxes(2) = subplot(n,1,3);
hold on; grid on; box on
plot(FB.Time,       FB.RotSpeed);
plot(FBFF.Time,     FBFF.RotSpeed);
plot(FBFFMC.Time,	FBFFMC.RotSpeed);
ylabel({'RotSpeed '; '[rpm]'});

MyAxes(4) = subplot(n,1,4);
hold on; grid on; box on
plot(FB.Time,       FB.PtfmPitch);
plot(FBFF.Time,     FBFF.PtfmPitch);
plot(FBFFMC.Time,	FBFFMC.PtfmPitch);
ylabel({'PtfmPitch'; '[deg]'});

xlabel('time [s]')
linkaxes(MyAxes,'x');
xlim([10 150])

%% display results
fprintf('Change in platform pitch amplitude (max-min) from FB to FBFF:  %4.1f %%\n',...
    ((max(FBFF.PtfmPitch)   -min(FBFF.PtfmPitch))/...
     (max(FB.PtfmPitch)     -min(FB.PtfmPitch))-1)*100)
fprintf('Change in platform pitch amplitude (max-min) from FB to FBFFMC:  %4.1f %%\n',...
    ((max(FBFFMC.PtfmPitch) -min(FBFFMC.PtfmPitch))/...
     (max(FB.PtfmPitch)     -min(FB.PtfmPitch))-1)*100) 