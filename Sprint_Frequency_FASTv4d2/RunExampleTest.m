% IEA 15 MW monopile + perfect wind preview.
% Origin and changes in files: see ChangeLogOpenFAST.txt.
% Purpose:
% Here, we use a perfect wind preview to demonstrate that the collective
% pitch feedforward controller (designed with SLOW) is able to reduce
% significantly the rotor speed variation when OpenFAST is disturbed by an
% Extreme Operating Gust. Here, only the rotor motion and tower motion 
% (GenDOF and TwFADOF1) are enabled.  
% Result:       
% Cost ("30 s sprint"):  0.952481

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% define FAST input file
SimulationName  = 'IEA-15-240-RWT-Monopile';

%% Run FB and FF simulation
% dos(['openfast_x64_v4d2.exe ',SimulationName,'_FB.fst']); 
dos(['openfast_x64_v4d2.exe ',SimulationName,'_FBFF.fst']);

%% Comparison
% read in data
FB                  = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF                = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF.outb']);
FBFF_R              = ReadROSCOtextIntoStruct([SimulationName,'_FBFF.RO.dbg']);

% Plot 
figure('Name','Simulation results')

subplot(4,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(FBFF_R.Time,   FBFF_R.REWS);
ylabel('[m/s]');
legend('Wind1VelX','REWS preview','Interpreter','none','Location','best')

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
xlim([0 30])

% display results
RotSpeed_0  = 7.56;     % [rpm]
TwrBsMyt_0  = 158.3e3;  % [kNm]
t_Start     = 0;        % [s]

Cost = (max(abs(FBFF.RotSpeed(FBFF.Time>=t_Start)-RotSpeed_0))) / RotSpeed_0 ...
     + (max(abs(FBFF.TwrBsMyt(FBFF.Time>=t_Start)-TwrBsMyt_0))) / TwrBsMyt_0;

fprintf('Cost ("30 s sprint"):  %f \n',Cost);
%% 
% Plot 
figure('Name','Simulation torque update')

subplot(4,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(FBFF_R.Time,   FBFF_R.REWS);
ylabel('[m/s]');
legend('Wind1VelX','REWS preview','Interpreter','none','Location','best')

subplot(4,1,2);
hold on; grid on; box on
plot(FB.Time,       FB.GenTq/1e3);
plot(FBFF.Time,     FBFF.GenTq/1e3);
ylabel({'GenTq'; '[MNm]'});
legend('feedback only','feedback-feedforward','Location','best')

subplot(4,1,3);
hold on; grid on; box on
plot(FB.Time,       FB.RotSpeed);
plot(FBFF.Time,     FBFF.RotSpeed);
ylabel({'RotSpeed';'[rpm]'});

subplot(4,1,4);
hold on; grid on; box on
% plot(FB.Time,       FB.TwrBsMyt/1e3);
plot(FBFF_R.Time,     FBFF_R.FF_TorqueUpdate);
ylabel({'FF_TorqueUpdate';'[Nm]'});

xlabel('time [s]')
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([0 30])

%% Plot swap array contents:
swapContents = readmatrix("TestBench_SwapLog.txt");
figure("Name","avrSWAPcontents")
hold on
plot(swapContents(1:end-1,1),swapContents(1:end-1,2))
legend("swapAVR contents")
grid; box;

%% Plot input/output frequency
dt = 0.01; % [s]
f0 = 50; % [Hz] grid frequency
% params for delta torque calculation
H = 1; % [s] system equivalent inertia
S = 15e6; % [VA] rated power
T = 2 * S * H / f0; % 
Omega_rated = rpm2radPs(7.56); % [rpm]

RoCoF = (FBFF_R.FF_TorqueUpdate * Omega_rated * f0)/(2*S*H);

f_integrated = cumtrapz(RoCoF);

input_f = readmatrix("Frequency\FrequencySignalTest.csv");
figure("Name","frequency comparison")
subplot(311)
hold on; grid on; box on
plot(input_f(1:3001,1),input_f(1:3001,2)-50)
ylabel("f [Hz]")

subplot(312)
hold on; grid on; box on
plot(FBFF_R.Time,f_integrated)
ylabel("f integrated from torque")

subplot(313)
hold on; grid on; box on
plot(FBFF_R.Time,input_f(1:3001,2)-50 + f_integrated)

fprintf("std f grid %f\n", std(input_f(1:3001,2)));
fprintf("std f integrated %f\n", std(f_integrated));
fprintf("std f added %f\n", std(input_f(1:3001,2)-50 + f_integrated));