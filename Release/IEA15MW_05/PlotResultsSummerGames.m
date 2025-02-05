% -------------------------------------------------------------------------
%
%   [Description]
%   This script displays and saves the results of the LAC SummerGames 2024 
%   DLC1.2 Marathon submitted by researchers.
%
% -------------------------------------------------------------------------

% setup
clearvars;close all;clc
addpath(genpath('..\WetiMatlabFunctions'))

%% Calculate life-time weighted DEL and mean
SimulationModes     = { 
                    'FeedbackOnly'
                    'LAC_CircularCW_sowento'
                    'LAC_4BeamPulsed_IAV'
                    };
nSimulationModes    = length(SimulationModes);

% Config
WindSpeedBins       = 4:2:24;
WindSpeedChannel    = 'mean_Wind1VelX';
FrequencyResults    = struct(); % dummy, because FrequencyResults are not used here
PostProcessingConfig.CalculateProcessResults = {
    @(ProcessResults,FrequencyResults,Statistics) DataFilterWindSpeedBins(Statistics,WindSpeedBins,WindSpeedChannel) 
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,WindSpeedBins,'DEL_4_TwrBsMyt','WoehlerExponent',4);
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,WindSpeedBins,'DEL_4_RotTorq','WoehlerExponent',4);
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,WindSpeedBins,'DEL_10_RootMyb1','WoehlerExponent',10);
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedMean(ProcessResults,Statistics,WindSpeedBins,'mean_GenPwr')
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedMean(ProcessResults,Statistics,WindSpeedBins,'Travel_BldPitch1')    
    @(ProcessResults,FrequencyResults,Statistics) CalculateMaxValuesPerBin(ProcessResults,Statistics,'Overshoot_GenSpeed')
    @(ProcessResults,FrequencyResults,Statistics) CalculateMaxValuesPerBin(ProcessResults,Statistics,'max_GenTq')    
    };

for iSimulationMode = 1:nSimulationModes
    load(['Statistics_',SimulationModes{iSimulationMode}],'Statistics');
    ProcessResults(iSimulationMode)   = CalculateProcessResults(FrequencyResults,Statistics,PostProcessingConfig);
end

%% Plots
VS_MaxTq    = 21765444.21450; % Maximum generator torque in Region 3 [Nm] 

figure('Name','Life-time weighted DEL')
subplot(211)
hold on;grid on;box on
% title('Life-time weighted tower DEL')
plot(WindSpeedBins,ProcessResults(1).LTW_DEL_4_TwrBsMyt_PerBin/1e3,'o-')
plot(WindSpeedBins,ProcessResults(2).LTW_DEL_4_TwrBsMyt_PerBin/1e3,'.-')
plot(WindSpeedBins,ProcessResults(3).LTW_DEL_4_TwrBsMyt_PerBin/1e3,'.-')
ylabel('Tower DEL [MNm]')
xlim([4 24])
xticks([4:2:24])
% subplot(312)
% hold on;grid on;box on
% title('Life-time weighted shaft DEL')
% plot(WindSpeedBins,ProcessResults(1).LTW_DEL_4_RotTorq_PerBin/1e3,'o-')
% plot(WindSpeedBins,ProcessResults(2).LTW_DEL_4_RotTorq_PerBin/1e3,'.-')
% plot(WindSpeedBins,ProcessResults(3).LTW_DEL_4_RotTorq_PerBin/1e3,'.-')
% ylabel('Shaft DEL [MNm]')
subplot(212)
hold on;grid on;box on
% title('Life-time weighted blade DEL')
plot(WindSpeedBins,ProcessResults(1).LTW_DEL_10_RootMyb1_PerBin/1e3,'o-')
plot(WindSpeedBins,ProcessResults(2).LTW_DEL_10_RootMyb1_PerBin/1e3,'.-')
plot(WindSpeedBins,ProcessResults(3).LTW_DEL_10_RootMyb1_PerBin/1e3,'.-')
legend('FB','sowento','IAV','location','best','Interpreter','none')
ylabel('Blade DEL [MNm]')
xlim([4 24])
xticks([4:2:24])
xlabel('wind speed [m/s]')

% Save Plot
ResizeAndSaveFigure(12,6,'MarathonResearchers.pdf')

% 
% figure('Name','Other values')
% subplot(411)
% hold on;grid on;box on
% title('Life-time weighted electrical power')
% plot(WindSpeedBins,ProcessResults(1).LTW_mean_GenPwr_PerBin/1e3,'o-')
% plot(WindSpeedBins,ProcessResults(2).LTW_mean_GenPwr_PerBin/1e3,'.-')
% plot(WindSpeedBins,ProcessResults(3).LTW_mean_GenPwr_PerBin/1e3,'.-')
% ylabel('Electrical power [MW]')
% subplot(412)
% hold on;grid on;box on
% title('Over-speed per bin')
% plot(WindSpeedBins,ProcessResults(1).Overshoot_GenSpeed_PerBin*100,'o-')
% plot(WindSpeedBins,ProcessResults(2).Overshoot_GenSpeed_PerBin*100,'.-')
% plot(WindSpeedBins,ProcessResults(3).Overshoot_GenSpeed_PerBin*100,'.-')
% ylabel('Over-speed [%]')
% subplot(413)
% hold on;grid on;box on
% title('Life-time weighted pitch travel')
% plot(WindSpeedBins,ProcessResults(1).LTW_Travel_BldPitch1_PerBin,'o-')
% plot(WindSpeedBins,ProcessResults(2).LTW_Travel_BldPitch1_PerBin,'.-')
% plot(WindSpeedBins,ProcessResults(3).LTW_Travel_BldPitch1_PerBin,'.-')
% ylabel('Pitch travel [deg]')
% subplot(414)
% hold on;grid on;box on
% title('Maximum generator torque')
% plot(WindSpeedBins,ProcessResults(1).max_GenTq_PerBin/1e3,'o-')
% plot(WindSpeedBins,ProcessResults(2).max_GenTq_PerBin/1e3,'.-')
% plot(WindSpeedBins,ProcessResults(3).max_GenTq_PerBin/1e3,'.-')
% plot(WindSpeedBins([1,end]),[1 1]*VS_MaxTq/1e6,'k-')
% ylabel('Genrator torque [MW]')
% legend(cat(1,SimulationModes,'maximum'),'location','best','Interpreter','none')
% xlabel('wind speed [m/s]')

%% Display results
for iSimulationMode = 2:nSimulationModes
    Cost = CalculateCost(ProcessResults(iSimulationMode),ProcessResults(1));
    fprintf('-------------------------------------------------\n')
    fprintf('Improvements by %s over %s:\n',SimulationModes{iSimulationMode},SimulationModes{1})
    fprintf('Energy increase: %4.2f %%\n',(ProcessResults(iSimulationMode).LTW_mean_GenPwr/ProcessResults(1).LTW_mean_GenPwr-1)*100)    
    fprintf('Tower load reduction: %4.1f %%\n',(ProcessResults(iSimulationMode).LTW_DEL_4_TwrBsMyt/ProcessResults(1).LTW_DEL_4_TwrBsMyt-1)*100)
    fprintf('Shaft load reduction: %4.1f %%\n',(ProcessResults(iSimulationMode).LTW_DEL_4_RotTorq/ProcessResults(1).LTW_DEL_4_RotTorq-1)*100)
    fprintf('Blade load reduction: %4.1f %%\n',(ProcessResults(iSimulationMode).LTW_DEL_10_RootMyb1/ProcessResults(1).LTW_DEL_10_RootMyb1-1)*100)
    fprintf('Over-speed reduction: %4.2f %%\n',(ProcessResults(iSimulationMode).Overshoot_GenSpeed-ProcessResults(1).Overshoot_GenSpeed)*100)
    fprintf('Pitch travel reduction: %4.2f %%\n',(ProcessResults(iSimulationMode).LTW_Travel_BldPitch1/ProcessResults(1).LTW_Travel_BldPitch1-1)*100)
    fprintf('-----------------------\n')    
    fprintf('Cost for Summer Games 2024 ("DLC 1.2 Marathon"):  %f \n',Cost);
    fprintf('-------------------------------------------------\n')
end

