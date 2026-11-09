% setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% get configuration
[PostProcessingConfig,PreProcessingVariation,InputFiles,Modifications] = GetParametersForBruteForceOptimizationRegion1p5;

% generate simulation folder and copy all files from OpenFAST and TemplateFolder to SimulationFolder
ExeFile                     = 'openfast_x64_v4.exe';
SimulationFolder            = 'SimulationResults_BruteForceOptimizationRegion1p5';
TemplateFolder              = '../IEA10-198-RWT';
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

% Statistics
Statistics                  = CalculateStatistics(TimeResults,PostProcessingConfig);

% FrequencyResults
FrequencyResults            = CalculateFrequencyResults(TimeResults,PostProcessingConfig);

% ProcessResults
ProcessResults              = CalculateProcessResults(FrequencyResults,Statistics,PostProcessingConfig);
PlotProcessResults(ProcessResults,PostProcessingConfig)