% IEA 15 MW monopile + perfect wind preview + PFC. 
% Origin and changes in files: see ChangeLogOpenFAST.txt. 
% Purpose: Here, we use perfect wind preview + a PFC module to demonstrate 
% in a simple example how feedforward torque updates can be utilized for 
% PFC. 
% Here, only the rotor motion and tower motion (GenDOF and TwFADOF1) 
% are enabled. 
% Result: Cost ("30 s sprint"):  0.952510

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% define FAST input file
SimulationName  = 'IEA-15-240-RWT-Monopile';

%% Run FB and FF simulation
dos(['openfast_x64_v4d2.exe ',SimulationName,'_FB.fst']);
dos(['openfast_x64_v4d2.exe ',SimulationName,'_FBFF_NoPFC.fst']);
dos(['openfast_x64_v4d2.exe ',SimulationName,'_FBFF.fst']);

%% Comparison
% read in data
FB                  = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF                = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF.outb']);
FBFF_R              = ReadROSCOtextIntoStruct([SimulationName,'_FBFF.RO.dbg']);

FBFF_NoPFC          = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF_NoPFC.outb']);
FBFF_NoPFC_R        = ReadROSCOtextIntoStruct([SimulationName,'_FBFF_NoPFC.RO.dbg']);

f                   = readmatrix("Frequency\FrequencySignal.csv"); % import frequency signal
%% Plot 
figure('Name','Simulation results FBFF + PFC')

subplot(7,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(FBFF_R.Time,   FBFF_R.REWS);
ylabel('[m/s]');
legend('Wind1VelX','REWS preview','Interpreter','none','Location','best')

subplot(7,1,2);
hold on; grid on; box on
plot(FBFF.Time,f(:,2))
ylabel({'f','[Hz]'})
legend("f_{grid}")

subplot(7,1,3);
hold on; grid on; box on
plot(FBFF_R.Time, FBFF_R.FF_TorqueUpdate/1e6);
ylabel({'[MNm]'});
legend("FF_{TorqueUpdate}")

subplot(7,1,4);
hold on; grid on; box on
plot(FB.Time,       FB.GenTq/1e3);
plot(FBFF.Time,     FBFF.GenTq/1e3);
ylabel({'GenTq'; '[MNm]'});
legend('feedback only','feedback-feedforward','Location','best')

subplot(7,1,5);
hold on; grid on; box on
plot(FB.Time,       FB.BldPitch1);
plot(FBFF.Time,     FBFF.BldPitch1);
ylabel({'BldPitch1'; '[deg]'});
legend('feedback only','feedback-feedforward','Location','best')

subplot(7,1,6);
hold on; grid on; box on
plot(FB.Time,       FB.RotSpeed);
plot(FBFF.Time,     FBFF.RotSpeed);
ylabel({'RotSpeed';'[rpm]'});

subplot(7,1,7);
hold on; grid on; box on
plot(FB.Time,       FB.TwrBsMyt/1e3);
plot(FBFF.Time,     FBFF.TwrBsMyt/1e3);
ylabel({'TwrBsMyt';'[MNm]'});

xlabel('time [s]')
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([1 30])

% display results
RotSpeed_0  = 7.56;     % [rpm]
TwrBsMyt_0  = 158.3e3;  % [kNm]
t_Start     = 0;        % [s]

Cost = (max(abs(FBFF.RotSpeed(FBFF.Time>=t_Start)-RotSpeed_0))) / RotSpeed_0 ...
     + (max(abs(FBFF.TwrBsMyt(FBFF.Time>=t_Start)-TwrBsMyt_0))) / TwrBsMyt_0;

fprintf('Cost ("30 s sprint"):  %f \n',Cost);
%
% Plot 
figure('Name','FBFF vs. FBFF+PFC')

subplot(7,1,1);
hold on; grid on; box on
plot(FBFF_NoPFC_R.Time,   FBFF_NoPFC_R.REWS);
plot(FBFF_R.Time,   FBFF_R.REWS);
ylabel('[m/s]');
legend('FBFF','FBFF + PFC','Interpreter','none','Location','best')

subplot(7,1,2);
hold on; grid on; box on
plot(FBFF.Time,f(:,2))
ylabel({'f','[Hz]'})
legend("f_{grid}")

subplot(7,1,3);
hold on; grid on; box on
plot(FBFF_NoPFC_R.Time,     FBFF_NoPFC_R.FF_TorqueUpdate/1e6);
plot(FBFF_R.Time,     FBFF_R.FF_TorqueUpdate/1e6);
ylabel({'[MNm]';'TorqueUpdate'});
legend('FBFF','FBFF + PFC','Interpreter','none','Location','best')

subplot(7,1,4);
hold on; grid on; box on
plot(FBFF_NoPFC.Time,     FBFF_NoPFC.GenTq/1e3);
plot(FBFF.Time,     FBFF.GenTq/1e3);
ylabel({'GenTq'; '[MNm]'});
legend('FBFF','FBFF + PFC','Interpreter','none','Location','best')

subplot(7,1,5);
hold on; grid on; box on
plot(FBFF_NoPFC.Time,     FBFF_NoPFC.BldPitch1);
plot(FBFF.Time,     FBFF.BldPitch1);
ylabel({'BldPitch1'; '[deg]'});
legend('FBFF','FBFF + PFC','Interpreter','none','Location','best')

subplot(7,1,6);
hold on; grid on; box on
plot(FBFF_NoPFC.Time,     FBFF_NoPFC.RotSpeed);
plot(FBFF.Time,     FBFF.RotSpeed);
ylabel({'RotSpeed';'[rpm]'});
legend('FBFF','FBFF + PFC','Interpreter','none','Location','best')

subplot(7,1,7);
hold on; grid on; box on
plot(FBFF_NoPFC.Time,     FBFF_NoPFC.TwrBsMyt/1e3);
plot(FBFF.Time,     FBFF.TwrBsMyt/1e3);
ylabel({'TwrBsMyt';'[MNm]'});
legend('FBFF','FBFF + PFC','Interpreter','none','Location','best')

xlabel('time [s]')
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([1 30])