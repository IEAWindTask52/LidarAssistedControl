% Script to compare DLC 1.2 for "FeedbackOnly", "LAC_CircularCW", and "LAC_4BeamPulsed". 

% setup
clearvars;close all;clc
addpath(genpath('..\WetiMatlabFunctions'))

%% Calculate life-time weighted DEL and mean

% Config
WindSpeedBins       = 4:2:24;
WindSpeedChannel    = 'mean_Wind1VelX';
FrequencyResults    = struct();
PostProcessingConfig.CalculateProcessResults = {
    @(ProcessResults,FrequencyResults,Statistics) DataFilterWindSpeedBins(Statistics,WindSpeedBins,WindSpeedChannel) 
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,WindSpeedBins,'DEL_4_TwrBsMyt','WoehlerExponent',4);
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,WindSpeedBins,'DEL_4_RotTorq','WoehlerExponent',4);
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,WindSpeedBins,'DEL_10_RootMyb1','WoehlerExponent',10);
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedMean(ProcessResults,Statistics,WindSpeedBins,'mean_GenPwr')
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedMean(ProcessResults,Statistics,WindSpeedBins,'Travel_BldPitch1')    
    @(ProcessResults,FrequencyResults,Statistics) CalculateMaxValuesPerBin(ProcessResults,Statistics,'Overshoot_GenSpeed')
    };

% FeedbackOnly
load('Statistics_FeedbackOnly','Statistics');
ProcessResults_FeedbackOnly     = CalculateProcessResults(FrequencyResults,Statistics,PostProcessingConfig);

% LAC_CircularCW
load('Statistics_LAC_CircularCW','Statistics');
ProcessResults_LAC_CircularCW   = CalculateProcessResults(FrequencyResults,Statistics,PostProcessingConfig);

% LAC_4BeamPulsed
load('Statistics_LAC_4BeamPulsed','Statistics');
ProcessResults_LAC_4BeamPulsed  = CalculateProcessResults(FrequencyResults,Statistics,PostProcessingConfig);

%% Plots
figure('Name','Life-time weighted DEL')
subplot(311)
hold on;grid on;box on
title('Life-time weighted tower DEL')
plot(WindSpeedBins,ProcessResults_FeedbackOnly.LTW_DEL_4_TwrBsMyt_PerBin/1e3,'o-')
plot(WindSpeedBins,ProcessResults_LAC_CircularCW.LTW_DEL_4_TwrBsMyt_PerBin/1e3,'.-')
plot(WindSpeedBins,ProcessResults_LAC_4BeamPulsed.LTW_DEL_4_TwrBsMyt_PerBin/1e3,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
ylabel('Tower DEL [MNm]')
subplot(312)
hold on;grid on;box on
title('Life-time weighted shaft DEL')
plot(WindSpeedBins,ProcessResults_FeedbackOnly.LTW_DEL_4_RotTorq_PerBin/1e3,'o-')
plot(WindSpeedBins,ProcessResults_LAC_CircularCW.LTW_DEL_4_RotTorq_PerBin/1e3,'.-')
plot(WindSpeedBins,ProcessResults_LAC_4BeamPulsed.LTW_DEL_4_RotTorq_PerBin/1e3,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
ylabel('Shaft DEL [MNm]')
subplot(313)
hold on;grid on;box on
title('Life-time weighted blade DEL')
plot(WindSpeedBins,ProcessResults_FeedbackOnly.LTW_DEL_10_RootMyb1_PerBin/1e3,'o-')
plot(WindSpeedBins,ProcessResults_LAC_CircularCW.LTW_DEL_10_RootMyb1_PerBin/1e3,'.-')
plot(WindSpeedBins,ProcessResults_LAC_4BeamPulsed.LTW_DEL_10_RootMyb1_PerBin/1e3,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
ylabel('Blade DEL [MNm]')
xlabel('wind speed [m/s]')

figure('Name','Other values')
subplot(311)
hold on;grid on;box on
title('Life-time weighted electrical power')
plot(WindSpeedBins,ProcessResults_FeedbackOnly.LTW_mean_GenPwr_PerBin/1e3,'o-')
plot(WindSpeedBins,ProcessResults_LAC_CircularCW.LTW_mean_GenPwr_PerBin/1e3,'.-')
plot(WindSpeedBins,ProcessResults_LAC_4BeamPulsed.LTW_mean_GenPwr_PerBin/1e3,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
ylabel('Electrical power [MW]')
subplot(312)
hold on;grid on;box on
title('Over-speed per bin')
plot(WindSpeedBins,ProcessResults_FeedbackOnly.Overshoot_GenSpeed_PerBin*100,'o-')
plot(WindSpeedBins,ProcessResults_LAC_CircularCW.Overshoot_GenSpeed_PerBin*100,'.-')
plot(WindSpeedBins,ProcessResults_LAC_4BeamPulsed.Overshoot_GenSpeed_PerBin*100,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
ylabel('Over-speed [%]')
subplot(313)
hold on;grid on;box on
title('Life-time weighted pitch travel')
plot(WindSpeedBins,ProcessResults_FeedbackOnly.LTW_Travel_BldPitch1_PerBin,'o-')
plot(WindSpeedBins,ProcessResults_LAC_CircularCW.LTW_Travel_BldPitch1_PerBin,'.-')
plot(WindSpeedBins,ProcessResults_LAC_4BeamPulsed.LTW_Travel_BldPitch1_PerBin,'.-')
legend('FeedbackOnly','LAC CircularCW','LAC 4BeamPulsed')
ylabel('Pitch travel [deg]')
xlabel('wind speed [m/s]')

% Display results
fprintf('-------------------------------------------------\n')
fprintf('Improvements by LAC_CircularCW over FeedbackOnly:\n')
fprintf('Tower load reduction: %4.1f %%\n',(ProcessResults_LAC_CircularCW.LTW_DEL_4_TwrBsMyt/ProcessResults_FeedbackOnly.LTW_DEL_4_TwrBsMyt-1)*100)
fprintf('Shaft load reduction: %4.1f %%\n',(ProcessResults_LAC_CircularCW.LTW_DEL_4_RotTorq_PerBin/ProcessResults_FeedbackOnly.LTW_DEL_4_RotTorq_PerBin-1)*100)
fprintf('Blade load reduction: %4.1f %%\n',(ProcessResults_LAC_CircularCW.LTW_DEL_10_RootMyb1_PerBin/ProcessResults_FeedbackOnly.LTW_DEL_10_RootMyb1_PerBin-1)*100)
fprintf('Energy increase: %4.2f %%\n',(ProcessResults_LAC_CircularCW.LTW_mean_GenPwr/ProcessResults_FeedbackOnly.LTW_mean_GenPwr-1)*100)
fprintf('Over-speed reduction: %4.2f %%\n',(ProcessResults_LAC_CircularCW.Overshoot_GenSpeed-ProcessResults_FeedbackOnly.Overshoot_GenSpeed)*100)
fprintf('Pitch Travel reduction: %4.2f %%\n',(ProcessResults_LAC_CircularCW.LTW_Travel_BldPitch1/ProcessResults_FeedbackOnly.LTW_Travel_BldPitch1-1)*100)
fprintf('-------------------------------------------------\n')
fprintf('Improvements by 4BeamPulsed over FeedbackOnly:\n')
fprintf('Tower load reduction: %4.1f %%\n',(ProcessResults_LAC_4BeamPulsed.LTW_DEL_4_TwrBsMyt/ProcessResults_FeedbackOnly.LTW_DEL_4_TwrBsMyt-1)*100)
fprintf('Shaft load reduction: %4.1f %%\n',(ProcessResults_LAC_4BeamPulsed.LTW_DEL_4_RotTorq_PerBin/ProcessResults_FeedbackOnly.LTW_DEL_4_RotTorq_PerBin-1)*100)
fprintf('Blade load reduction: %4.1f %%\n',(ProcessResults_LAC_4BeamPulsed.LTW_DEL_10_RootMyb1_PerBin/ProcessResults_FeedbackOnly.LTW_DEL_10_RootMyb1_PerBin-1)*100)
fprintf('Energy increase: %4.2f %%\n',(ProcessResults_LAC_4BeamPulsed.LTW_mean_GenPwr/ProcessResults_FeedbackOnly.LTW_mean_GenPwr-1)*100)
fprintf('Over-speed reduction: %4.2f %%\n',(ProcessResults_LAC_4BeamPulsed.Overshoot_GenSpeed-ProcessResults_FeedbackOnly.Overshoot_GenSpeed)*100)
fprintf('Pitch Travel reduction: %4.2f %%\n',(ProcessResults_LAC_4BeamPulsed.LTW_Travel_BldPitch1/ProcessResults_FeedbackOnly.LTW_Travel_BldPitch1-1)*100)
fprintf('-------------------------------------------------\n')