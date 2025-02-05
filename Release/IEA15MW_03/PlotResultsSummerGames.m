% -------------------------------------------------------------------------
%
%   [Description]
%   This script displays and saves the results of the LAC SummerGames 2024 
%   18 m/s Hurdles submitted by students.
%
% -------------------------------------------------------------------------
%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Parameters postprocessing (can be adjusted, but will provide different results)
R                   = 120;                      % [m]  	rotor radius to calculate REWS
t_start             = 60;                       % [s] 	ignore data before for STD and spectra
DT                  = 0.0125;                   % [s]   time step, same as in *.fst

% Parameter for Cost (Summer Games 2024)
tau                 = 2;                        % [s]   time to overcome pitch actuator, from Example 1: tau = T_Taylor - T_buffer, since there T_filter = T_scan = 0

% Load data
ROSCOresultFile     = 'DTU/URef_18_Seed_1806_FlagLAC_1_theta_31_xdist_133.808_tbuffer_1.6.dbg';
DTU                 = ReadROSCOtextIntoStruct(ROSCOresultFile);
UDELAR              = readtable('UDELAR\REWS_seed6.csv');
Time                = t_start+[DT:DT:599]';
UDELAR              = addvars(UDELAR,Time);
load('UniNorte/URef_18_Seed_1806_FlagLAC_1','R_FBFF');
UniNorte            = R_FBFF;

% Get REWS from wind field
Seed                = 1806;
WindFileName        = ['URef_18_Seed_',num2str(Seed,'%04d')];
TurbSimResultFile                 	= ['TurbulentWind\',WindFileName,'.wnd'];   
[REWS_WindField,Time_WindField]  	= CalculateREWSfromWindField(TurbSimResultFile,R,2);
REWS_WindField_Fs                   = interp1(Time_WindField,REWS_WindField,DTU.Time);
REWS_WindField_Fs_shifted   = interp1(Time_WindField-tau,REWS_WindField,DTU.Time); % shift the REWS from wind field by tau (intented prediction time) into the future (lower times)

% Calculate Error
Error_DTU                   = REWS_WindField_Fs_shifted-DTU.REWS_b; % error is  REWS from wind field shifted minus REWS from lidar filtered and buffered.
Error_UDELAR                = UDELAR.test-UDELAR.prediccion;
Error_UniNorte              = REWS_WindField_Fs_shifted-UniNorte.REWS_b; % error is  REWS from wind field shifted minus REWS from lidar filtered and buffered.

% Plot REWS for absolute error
figure('Name',['REWS seed ',num2str(Seed)])
subplot(211)
hold on; grid on; box on
plot(UDELAR.Time,UDELAR.prediccion);
plot(DTU.Time,   DTU.REWS_b);
plot(UniNorte.Time,   UniNorte.REWS_b);
plot(DTU.Time,   REWS_WindField_Fs_shifted,'k');
xlim([0 600]+t_start)
xticks([0:60:600]+t_start)
ylabel('REWS [m/s]');
legend('Udelar','DTU','UniNorte','wind field')
subplot(212)
hold on; grid on; box on
plot(UDELAR.Time,   Error_UDELAR);
plot(DTU.Time,   Error_DTU);
plot(DTU.Time,   Error_UniNorte);
xlim([0 600]+t_start)
xticks([0:60:600]+t_start)
ylabel('error [m/s]');
xlabel('time [s]')   
legend('Udelar','DTU','UniNorte')

% Save Plot
ResizeAndSaveFigure(12,6,'HurdlesStudents.pdf')
