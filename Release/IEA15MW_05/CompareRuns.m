% Script to compare DLC 1.2 for "FeedbackOnly", "LAC_CircularCW", and "LAC_4BeamPulsed". 

% setup
clearvars;close all;clc
addpath(genpath('..\WetiMatlabFunctions'))

%% Calculate life-time weighted DEL and mean

% Config
WindSpeedBins       = 4:2:24;
WindSpeedChannel    = 'mean_Wind1VelX';

% FeedbackOnly
load('Statistics_FeedbackOnly','Statistics');
ProcessResults_FeedbackOnly     = DataFilterWindSpeedBins(Statistics,WindSpeedBins,WindSpeedChannel);
ProcessResults_FeedbackOnly     = CalculateLifeTimeWeightedDEL(ProcessResults_FeedbackOnly,Statistics,WindSpeedBins,'DEL_TwrBsMyt');
ProcessResults_FeedbackOnly     = CalculateLifeTimeWeightedMean(ProcessResults_FeedbackOnly,Statistics,WindSpeedBins,'mean_GenPwr');

% LAC_CircularCW
load('Statistics_LAC_CircularCW','Statistics');
ProcessResults_LAC_CircularCW   = DataFilterWindSpeedBins(Statistics,WindSpeedBins,WindSpeedChannel);
ProcessResults_LAC_CircularCW   = CalculateLifeTimeWeightedDEL(ProcessResults_LAC_CircularCW,Statistics,WindSpeedBins,'DEL_TwrBsMyt');
ProcessResults_LAC_CircularCW   = CalculateLifeTimeWeightedMean(ProcessResults_LAC_CircularCW,Statistics,WindSpeedBins,'mean_GenPwr');

% LAC_4BeamPulsed
load('Statistics_LAC_4BeamPulsed','Statistics');
ProcessResults_LAC_4BeamPulsed  = DataFilterWindSpeedBins(Statistics,WindSpeedBins,WindSpeedChannel);
ProcessResults_LAC_4BeamPulsed  = CalculateLifeTimeWeightedDEL(ProcessResults_LAC_4BeamPulsed,Statistics,WindSpeedBins,'DEL_TwrBsMyt');
ProcessResults_LAC_4BeamPulsed  = CalculateLifeTimeWeightedMean(ProcessResults_LAC_4BeamPulsed,Statistics,WindSpeedBins,'mean_GenPwr');

%% Plots
figure('Name','Life-time weighted tower DEL')
hold on;grid on; box on
title('Life-time weighted tower DEL')
plot(ProcessResults_FeedbackOnly.WindSpeedBins,   ProcessResults_FeedbackOnly.LTW_DEL_TwrBsMyt_PerBin/1e3,'o-')
plot(ProcessResults_LAC_CircularCW.WindSpeedBins, ProcessResults_LAC_CircularCW.LTW_DEL_TwrBsMyt_PerBin/1e3,'.-')
plot(ProcessResults_LAC_4BeamPulsed.WindSpeedBins,ProcessResults_LAC_4BeamPulsed.LTW_DEL_TwrBsMyt_PerBin/1e3,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
xlabel('wind speed [m/s]')
ylabel('Tower DEL [MNm]')

figure('Name','Life-time weighted electrical power')
hold on;grid on; box on
title('Life-time weighted electrical power')
plot(ProcessResults_FeedbackOnly.WindSpeedBins,   ProcessResults_FeedbackOnly.LTW_mean_GenPwr_PerBin/1e3,'o-')
plot(ProcessResults_LAC_CircularCW.WindSpeedBins, ProcessResults_LAC_CircularCW.LTW_mean_GenPwr_PerBin/1e3,'.-')
plot(ProcessResults_LAC_4BeamPulsed.WindSpeedBins,ProcessResults_LAC_4BeamPulsed.LTW_mean_GenPwr_PerBin/1e3,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
xlabel('wind speed [m/s]')
ylabel('Electrical power [MW]')

% Display results
fprintf('-------------------------------------------------\n')
fprintf('Improvements by LAC_CircularCW over FeedbackOnly:\n')
fprintf('Tower load reduction: %4.1f %%\n',(ProcessResults_LAC_CircularCW.LTW_DEL_TwrBsMyt/ProcessResults_FeedbackOnly.LTW_DEL_TwrBsMyt-1)*100)
fprintf('Energy increase: %4.2f %%\n',(ProcessResults_LAC_CircularCW.LTW_mean_GenPwr/ProcessResults_FeedbackOnly.LTW_mean_GenPwr-1)*100)
fprintf('-------------------------------------------------\n')
fprintf('Improvements by 4BeamPulsed over FeedbackOnly:\n')
fprintf('Tower load reduction: %4.1f %%\n',(ProcessResults_LAC_4BeamPulsed.LTW_DEL_TwrBsMyt/ProcessResults_FeedbackOnly.LTW_DEL_TwrBsMyt-1)*100)
fprintf('Energy increase: %4.2f %%\n',(ProcessResults_LAC_4BeamPulsed.LTW_mean_GenPwr/ProcessResults_FeedbackOnly.LTW_mean_GenPwr-1)*100)
fprintf('-------------------------------------------------\n')