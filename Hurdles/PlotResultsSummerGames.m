% -------------------------------------------------------------------------
%
%   [Description]
%   This script displays and saves the results of the LAC SummerGames 2025 
%   18 m/s Hurdles submitted by students.
%
% -------------------------------------------------------------------------
%% Setup
clearvars;close all;clc;
addpath(genpath('..\..\WetiMatlabFunctions'))


%%
% Parameters postprocessing (can be adjusted, but will provide different results)
R                   = 120;                      % [m]  	rotor radius to calculate REWS
t_start             = 60;                       % [s] 	ignore data before for STD and spectra
DT                  = 0.0125;                   % [s]   time step, same as in *.fst

% Parameter for Cost (Summer Games 2024)
tau                 = 2;                        % [s]   time to overcome pitch actuator, from Example 1: tau = T_Taylor - T_buffer, since there T_filter = T_scan = 0

% Load data
KurunjiResultFile           = 'ResultsStudents\SimulationResults_4BeamPulsed_Kurunji\URef_18_Seed_1801_REWS_v8.csv';
Kurunji                     = readtable(KurunjiResultFile);
GustGuardiansResultsFile    = 'ResultsStudents\SimulationResults_4BeamPulsed_GustGuradians\Hurdles_Result_withUpdateInterval_seed1.mat';
GustGuardians               = load(GustGuardiansResultsFile);
LoongSightResultsFile       = 'ResultsStudents\SimulationResults_4BeamPulsed_LoongSight\Hurdles_Result_withUpdateInterval_seed1.mat';    
LoongSight                  = load(LoongSightResultsFile);

% Get REWS from wind field
Seed                = 1801;
WindFileName        = ['URef_18_Seed_',num2str(Seed,'%04d')];
TurbSimResultFile                 	= ['TurbulentWind\',WindFileName,'.wnd'];   
[REWS_WindField,Time_WindField]  	= CalculateREWSfromWindField(TurbSimResultFile,R,2);
REWS_WindField_Fs                   = interp1(Time_WindField,REWS_WindField,Kurunji.time);
REWS_WindField_Fs_shifted           = interp1(Time_WindField-tau,REWS_WindField,Kurunji.time);

% Calculate Error
Error_Kurunji           = REWS_WindField_Fs_shifted-Kurunji.REWS_b;
Error_GustGuardians     = REWS_WindField_Fs_shifted-GustGuardians.Result.REWS_b;
Error_LoongSight        = REWS_WindField_Fs_shifted-LoongSight.Result.REWS_b;

% Plot REWS for absolute error
figure('Name',['REWS seed ',num2str(Seed)])
subplot(211)
hold on; grid on; box on
plot(Kurunji.time,      Kurunji.REWS_b);
plot(Kurunji.time,   GustGuardians.Result.REWS_b);
plot(Kurunji.time,   LoongSight.Result.REWS_b);
plot(Kurunji.time,   REWS_WindField_Fs_shifted,'k');
xlim([0 600]+t_start)
xticks([0:60:600]+t_start)
ylabel('REWS [m/s]');
legend('Kurunji','GustGuardians','LoongSight','wind field')
subplot(212)
hold on; grid on; box on
plot(Kurunji.time,   Error_Kurunji);
plot(Kurunji.time,   Error_GustGuardians);
plot(Kurunji.time,   Error_LoongSight);
xlim([0 600]+t_start)
xticks([0:60:600]+t_start)
ylabel('error [m/s]');
xlabel('time [s]')   
legend('Kurunji','GustGuardians','LoongSight')

% Save Plot
ResizeAndSaveFigure(12,10,'HurdlesStudents3.pdf')
