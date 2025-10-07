% Sprint: IEA 15 MW monopile + perfect wind preview from a single point 
% lidar system.
% Origin and changes in files: see ChangeLog.txt.
% Purpose:
% Here, we use a perfect wind preview to demonstrate that the collective
% pitch feedforward controller (designed with SLOW) is able to reduce
% significantly the rotor speed variation when OpenFAST is disturbed by an
% Extreme Operating Gust. Here, only the rotor motion and tower motion 
% (GenDOF and TwFADOF1) are enabled.  
% Result:       
% Cost for Summer Games 2025 ("30 s sprint"):  0.849094

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Copy the adequate OpenFAST version to the example folder
FASTexeFile     = '';
SimulationName  = 'IEA-15-240-RWT-Monopile';

%% Run FB
dos(['openfast_x64.exe ',SimulationName,'_FB.fst']);                    % run OpenFAST

%% Run FBFF with perfect preview from single point lidar
dos(['openfast_x64.exe ',SimulationName,'_FBFF_SinglePoint.fst']);      % run OpenFAST

%% Run FBFF with perfect preview from 4-beam pulsed lidar
dos(['openfast_x64.exe ',SimulationName,'_FBFF_4BeamPulsed.fst']);      % run OpenFAST

%% Run FBFF with perfect preview from cicular continous wave lidar
dos(['openfast_x64.exe ',SimulationName,'_FBFF_CircularCW.fst']);       % run OpenFAST

%% Comparison
% read in data
FB                  = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);

FBFF_SinglePoint    = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF_SinglePoint.outb']);
FBFF_SinglePoint_R  = ReadROSCOtextIntoStruct([SimulationName,'_FBFF_SinglePoint.RO.dbg']);

FBFF_4BeamPulsed    = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF_4BeamPulsed.outb']);
FBFF_4BeamPulsed_R  = ReadROSCOtextIntoStruct([SimulationName,'_FBFF_4BeamPulsed.RO.dbg']);

FBFF_CircularCW     = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF_CircularCW.outb']);
FBFF_CircularCW_R   = ReadROSCOtextIntoStruct([SimulationName,'_FBFF_CircularCW.RO.dbg']);

% Plot 
figure('Name','Simulation results')

subplot(4,1,1);
hold on; grid on; box on
plot(FB.Time,                   FB.Wind1VelX);
plot(FBFF_SinglePoint.Time,     FBFF_SinglePoint_R.REWS_b);
plot(FBFF_4BeamPulsed.Time,     FBFF_4BeamPulsed_R.REWS_b);
plot(FBFF_CircularCW.Time,      FBFF_CircularCW_R.REWS_b);
legend('Hub height wind speed','Vlos')
ylabel('[m/s]');
legend('Wind1VelX','SinglePoint_REWS_b','4BeamPulsed_REWS_b','CircularCW_REWS_b','Interpreter','none')

subplot(4,1,2);
hold on; grid on; box on
plot(FB.Time,                   FB.BldPitch1);
plot(FBFF_SinglePoint.Time,     FBFF_SinglePoint.BldPitch1);
plot(FBFF_4BeamPulsed.Time,     FBFF_4BeamPulsed.BldPitch1);
plot(FBFF_CircularCW.Time,      FBFF_CircularCW.BldPitch1);
ylabel({'BldPitch1'; '[deg]'});
legend('feedback only','FF single-point','FF 4-beam pulsed','FF circular cw')

subplot(4,1,3);
hold on; grid on; box on
plot(FB.Time,                   FB.RotSpeed);
plot(FBFF_SinglePoint.Time,     FBFF_SinglePoint.RotSpeed);
plot(FBFF_4BeamPulsed.Time,     FBFF_4BeamPulsed.RotSpeed);
plot(FBFF_CircularCW.Time,      FBFF_CircularCW.RotSpeed);
ylabel({'RotSpeed';'[rpm]'});

subplot(4,1,4);
hold on; grid on; box on
plot(FB.Time,                   FB.TwrBsMyt/1e3);
plot(FBFF_SinglePoint.Time,     FBFF_SinglePoint.TwrBsMyt/1e3);
plot(FBFF_4BeamPulsed.Time,     FBFF_4BeamPulsed.TwrBsMyt/1e3);
plot(FBFF_CircularCW.Time,      FBFF_CircularCW.TwrBsMyt/1e3);
ylabel({'TwrBsMyt';'[MNm]'});

xlabel('time [s]')
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([20 50])

% display results
RotSpeed_0  = 7.56;     % [rpm]
TwrBsMyt_0  = 158.3e3;  % [kNm]
t_Start     = 0;        % [s]

Cost = (max(abs(FBFF_4BeamPulsed.RotSpeed(FBFF_4BeamPulsed.Time>=t_Start)-RotSpeed_0))) / RotSpeed_0 ...
     + (max(abs(FBFF_4BeamPulsed.TwrBsMyt(FBFF_4BeamPulsed.Time>=t_Start)-TwrBsMyt_0))) / TwrBsMyt_0;

fprintf('Cost for Summer Games 2025 ("30 s sprint"):  %f \n',Cost);