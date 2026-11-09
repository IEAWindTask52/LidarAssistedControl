% ============================================================
% Run DLC 1.2 - Region 3 Brute Force Optimization
%
% theta_K:
%   3 deg  = 0.05 rad
%   6 deg  = 0.10 rad
%
% URef:
%   12, 16, 20, 24 m/s
%
% Seeds:
%   1, 2, 3
%
% Controller:
%   Minimum generator speed = 7 rpm
%   LPF = 3 Hz
% Pitch Actuator:
%   With corner freq. 6 rad/s with PA_Mode=02 in input file 
% ============================================================

clearvars;
close all;
clc;

%% Setup

addpath(genpath('..\WetiMatlabFunctions'));

%% Simulation mode

SimulationMode = 'FeedbackOnly';

%% Get configuration

[PostProcessingConfig,PreProcessingVariation,InputFiles,Modifications] = GetParametersForBruteForceOptimizationRegion3(SimulationMode);

%% Simulation folder

ExeFile = 'openfast_x64_v4.exe';

SimulationFolder = ['SimulationResults_BruteForceRegion3_',SimulationMode];

TemplateFolder = '../IEA10-198-RWT';

%% Create simulation folder

if ~exist(SimulationFolder,'dir')
    mkdir(SimulationFolder)
end

%% Copy turbine template

copyfile(TemplateFolder,SimulationFolder);

%% Copy OpenFAST executable

copyfile(['..\OpenFAST\',ExeFile],fullfile(SimulationFolder,ExeFile));

%% Generate simulations

[SimulationNames,DataFiles] = PreProcessingSimulations(SimulationFolder,PreProcessingVariation,InputFiles,Modifications);

%% Run OpenFAST

[status,result] = ProcessingSimulations(SimulationFolder,SimulationNames,ExeFile);

%% Time results

TimeResults = CollectTimeResults(DataFiles,PostProcessingConfig);

PlotTimeResults(TimeResults,PostProcessingConfig);

%% Statistics

Statistics = CalculateStatistics(TimeResults,PostProcessingConfig);

%% Frequency results

FrequencyResults = CalculateFrequencyResults( TimeResults,PostProcessingConfig);

PlotFrequencyResults(FrequencyResults,PostProcessingConfig);

%% Save results

save(['Statistics_BruteForceRegion3_',SimulationMode],'Statistics');

disp(' ');
disp('==============================================');
disp(' Region 3 simulations completed');
disp('==============================================');