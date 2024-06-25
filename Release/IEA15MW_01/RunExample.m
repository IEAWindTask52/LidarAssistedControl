% IEA15MW_01: IEA 15 MW monopile + perfect wind preview from a single point 
% lidar system.
% Origin and changes in files: see ChangeLog.txt.
% Purpose:
% Here, we use a perfect wind preview to demonstrate that the collective
% pitch feedforward controller (designed with SLOW) is able to reduce
% significantly the rotor speed variation when OpenFAST is disturbed by an
% Extreme Operating Gust. Here, only the rotor motion and tower motion 
% (GenDOF and TwFADOF1) are enabled.  
% Result:       
% Cost for Summer Games 2024 ("30 s sprint"):  0.849094

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Copy the adequate OpenFAST version to the example folder
FASTexeFile     = 'openfast_x64.exe';
SimulationName  = 'IEA-15-240-RWT-Monopile';
copyfile(['..\OpenFAST\',FASTexeFile],FASTexeFile)

%% Run FB
ManipulateTXTFile('ROSCO_v2d6.IN','1 ! FlagLAC','0 ! FlagLAC');     % disable LAC
dos([FASTexeFile,' ',SimulationName,'.fst']);                       % run OpenFAST
movefile([SimulationName,'.outb'],[SimulationName,'_FB.outb'])      % store results

%% Run FBFF  
ManipulateTXTFile('ROSCO_v2d6.IN','0 ! FlagLAC','1 ! FlagLAC');     % enable LAC
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
xlim([0 30])

% display results
RotSpeed_0  = 7.56;     % [rpm]
TwrBsMyt_0  = 158.3e3;  % [kNm]
t_Start     = 0;        % [s]

Cost = (max(abs(FBFF.RotSpeed(FBFF.Time>=t_Start)-RotSpeed_0))) / RotSpeed_0 ...
     + (max(abs(FBFF.TwrBsMyt(FBFF.Time>=t_Start)-TwrBsMyt_0))) / TwrBsMyt_0;

fprintf('Cost for Summer Games 2024 ("30 s sprint"):  %f \n',Cost);