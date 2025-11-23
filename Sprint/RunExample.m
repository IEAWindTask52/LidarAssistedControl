% Sprint: DLC 1.4 for IEA 15 MW monopile with and without LAC. 
% Purpose:
% We want to learn how to simulate a DLC 1.4 with an "Extreme coherent gust 
% with direction change (ECD)" with lidar-assisted control (LAC) and how
% LAC can reduce the ultimate tower loads. 
% Here, only the rotor motion and tower motion (GenDOF, TwFADOF1, TwSSDOF1) 
% are enabled for simplicity.
% Result:       
% Cost for Summer Games 2025 ("30 s sprint"):  0.739948 (4BeamPulsed)
% Cost for Summer Games 2025 ("30 s sprint"):  1.219835 (CircularCW)

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% select simulated lidar
LidarType       = '4BeamPulsed'; % [4BeamPulsed/CircularCW]

% define FAST input file
SimulationName  = 'IEA-15-240-RWT-Monopile';

%% Run FB and FF simulation
dos(['openfast_x64.exe ',SimulationName,'_FB.fst']); 
dos(['openfast_x64.exe ',SimulationName,'_FBFF_',LidarType,'.fst']);

%% Comparison
% read in data
FB                  = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF                = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF_',LidarType,'.outb']);
FBFF_R              = ReadROSCOtextIntoStruct( [SimulationName,'_FBFF_',LidarType,'.RO.dbg']);

% Plot 
figure('Name','Simulation results')

subplot(4,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(FBFF_R.Time,   FBFF_R.REWS_b);
ylabel('[m/s]');
legend('Wind1VelX','REWS_b','Interpreter','none','Location','best')

subplot(4,1,2);
hold on; grid on; box on
plot(FB.Time,       FB.BldPitch1);
plot(FBFF.Time,     FBFF.BldPitch1);
ylabel({'BldPitch1'; '[deg]'});
legend('feedback only','feedback-feedforward','Location','best')

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
xlim([20 50])

% display results
RotSpeed_0  = 7.56;     % [rpm]
TwrBsMyt_0  = 158.3e3;  % [kNm]
t_Start     = 0;        % [s]

Cost = (max(abs(FBFF.RotSpeed(FBFF.Time>=t_Start)-RotSpeed_0))) / RotSpeed_0 ...
     + (max(abs(FBFF.TwrBsMyt(FBFF.Time>=t_Start)-TwrBsMyt_0))) / TwrBsMyt_0;

fprintf('Cost for Summer Games 2025 ("30 s sprint"):  %f \n',Cost);