% Script to run DLC 1.2 for "LAC_CircularCW".

% setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% get configuration for DLC 1.2
SimulationMode              = 'LAC_CircularCW';
[PostProcessingConfig,PreProcessingVariation,InputFiles,Modifications] = GetParametersForDLC1p2(SimulationMode);

% generate simulation folder and copy all files from OpenFAST and TemplateFolder to SimulationFolder
ExeFile                     = 'openfast_x64.exe';
SimulationFolder            = ['SimulationResults_',SimulationMode];
TemplateFolder              = '../IEA-15-240-RWT-Monopile';
if ~exist(SimulationFolder,'dir')
    mkdir(SimulationFolder)
end
copyfile(TemplateFolder,SimulationFolder)
copyfile(['..\OpenFAST\',ExeFile],fullfile(SimulationFolder,ExeFile))

% get SimulationNames and DataFiles
[SimulationNames,DataFiles] = PreProcessingSimulations(SimulationFolder,PreProcessingVariation,InputFiles,Modifications);

% run simulations
[status,result]             = ProcessingSimulations(SimulationFolder,SimulationNames,ExeFile);

% TimeResults
TimeResults                 = CollectTimeResults(DataFiles,PostProcessingConfig);
PlotTimeResults(TimeResults,PostProcessingConfig);

% Statistics
Statistics                  = CalculateStatistics(TimeResults,PostProcessingConfig);

% Evaluate only, if you want to overwrite the results: 
% save(['Statistics_',SimulationMode],'Statistics')