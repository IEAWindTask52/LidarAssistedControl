% -------------------------------------------------------------------------
%
%   [Description]
%   This script displays and saves the results of the LAC SummerGames 2025
%   30s sprint submitted by students.
%
% -------------------------------------------------------------------------
%% Setup

clearvars; close all; clc
addpath(genpath('..\..\WetiMatlabFunctions'))
addpath(genpath('ResultsStudents'))

SimulationName  = 'IEA-15-240-RWT-Monopile';

%% Read/Plot/Save Data
% Read Data
FB              = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF            = ReadROSCOtextIntoStruct([SimulationName,'_FBFF_4BeamPulsed.RO.dbg']);
LoongEcho       = ReadFASTbinaryIntoStruct([SimulationName,'_Simulink_4BeamPulsed_LoongEcho.outb']);
Kurunji         = ReadFASTbinaryIntoStruct([SimulationName,'_Simulink_4BeamPulsed_Kurunji.outb']);
VectorBased     = ReadFASTbinaryIntoStruct([SimulationName,'_Simulink_4BeamPulsed_VectorBased.outb']);

% Plot Data
nSubplots       = 4;
FontSize        = 6;

figure
subplot(nSubplots, 1, 1)
hold on; grid on; box on;
plot(FB.Time,       FB.Wind1VelX)
plot(FBFF.Time,     FBFF.REWS_b)
legend('Wind1VelX','REWS_b','Fontsize',FontSize,'Interpreter','none')
ylabel({'WindSpeed', '[m/s]'},'Fontsize',FontSize);
set(gca,'Fontsize',FontSize);

subplot(nSubplots, 1, 2)
hold on; grid on; box on;
plot(FB.Time,       FB.BldPitch1)        
plot(LoongEcho.Time,      LoongEcho.BldPitch1)
plot(Kurunji.Time, Kurunji.BldPitch1)
plot(VectorBased.Time,     VectorBased.BldPitch1)
ylabel({'BldPitch1'; '[deg]'},'Fontsize', FontSize);
set(gca,'Fontsize', FontSize);
ylim([3.5 18])

subplot(nSubplots, 1, 3)
hold on; grid on; box on;
plot(FB.Time,       FB.RotSpeed)
plot(LoongEcho.Time, LoongEcho.RotSpeed)
plot(Kurunji.Time, Kurunji.RotSpeed)
plot(VectorBased.Time,     VectorBased.RotSpeed)

legend('FB', 'LoongEcho', 'Kurunji', 'VectorBased','NumColumns', 2,location='northwest')
ylabel({'RotSpeed';'[rpm]'},'Fontsize',FontSize);
set(gca,'Fontsize',FontSize);
ylim([6.5 9.5])

subplot(nSubplots, 1, 4)
hold on; grid on; box on;
plot(FB.Time,       FB.TwrBsMyt/1e3)
plot(LoongEcho.Time, LoongEcho.TwrBsMyt/1e3)
plot(Kurunji.Time, Kurunji.TwrBsMyt/1e3)
plot(VectorBased.Time,     VectorBased.TwrBsMyt/1e3)
ylabel({'TwrBsMyt';'[MNm]'},'Fontsize',FontSize);
set(gca,'Fontsize',FontSize);

xlabel('time [s]','Fontsize',FontSize);
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([20 50])

% Save Plot
ResizeAndSaveFigure(12,10,'SprintStudents3.pdf')
