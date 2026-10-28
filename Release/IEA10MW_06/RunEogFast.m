% EOG simulation for the 6M using OpenFAST and Senvion feedback v2 to test 
% baseline feedforward controller.
% A = 3 m/s amplitude is chosen to avoid reaching maximum pitch rate.
% Select SimulationCase to optimize FF or compare FB vs FBFF.

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% files (should not be be changed)
ExeFile                     = 'openfast_x64_v4.exe';
SimulationFolder            = 'SimulationResults';
TemplateFolder              = '../IEA10-198-RWT';
StatisticsFile      	    = 'Statistics_SteadyStates.mat';

% ProcessName
SimulationCase = 'FBvsFBFF'; % 'FBvsFBFF' or 'FFopt'
switch SimulationCase
    case 'FBvsFBFF' % to compare FB vs FBFF
        PreProcessingVariation  = {  'URef',[25],'%04.1f'
                                     'FF',[0 1],'%d'
                                    };                                    
        MyLegend                = {'FB','FBFF'};       
end

% template files
InputFiles{1,1} = 'IEA-10.0-198-RWT.fst';               % main file
InputFiles{2,1} = 'IEA-10.0-198-RWT_ElastoDyn.dat';     % to adjust initial conditions
InputFiles{3,1} = 'IEA-10.0-198-RWT_InflowFile.dat';    % to adjust wind speed
InputFiles{4,1} = 'IEA-10.0-198-RWT_ServoDyn.dat';      % to link to new .in file       
InputFiles{5,1} = 'WRAPPER.in';                         % to link to new rosco.in file 
InputFiles{6,1} = 'IEA-10.0-198-RWT_DISCON_v2d6mod.IN'; % to enable/disable FF

% new files to be modified
InputFiles{1,2}         = '<SimulationName>.fst';
InputFiles{2,2}         = '<SimulationName>_ElastoDyn.dat';
InputFiles{3,2}         = '<SimulationName>_InflowWind.dat';
InputFiles{4,2}         = '<SimulationName>_ServoDyn.dat';
InputFiles{5,2}         = '<SimulationName>_WRAPPER.IN';
InputFiles{6,2}         = '<SimulationName>_DISCON.IN';

% modifications in new simulation files
Modifications           = {
                        % OpenFAST: change time and link to new files
                        '1','I','TMax',                 num2str(50)
                        '1','I','EDFile',               InputFiles{2,2}
                        '1','I','InflowFile',           InputFiles{3,2}  
                        '1','I','ServoFile',            InputFiles{4,2}   
                        % ElastoDyn: adjust initial conditions
                        '2','I','BlPitch\((1|2|3)\)',   @(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_BldPitch1',VariationValues(1)),'%5.2f')
                        '2','I','RotSpeed',             @(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_RotSpeed' ,VariationValues(1)),'%5.2f')                        
                        % InflowWind: change wind type and link to turbulent wind file
                        '3','I','WindType  '            '2'
                        '3','I','Filename_Uni',	        @(VariationValues)strcat('../Wind/EOG_URef_',num2str(VariationValues(1),'%02d'),'.wnd')                       
                        % ServoDyn: Link to new in file
                        '4','I','DLL_FileName',         'WRAPPER.DLL';
                        '4','I','DLL_InFile',           InputFiles{5,2}; 
                        % WRAPPER.IN: link to new rosco.in file
                        '5','R','"IEA-10.0-198-RWT_DISCON_v2d6mod.IN"'  InputFiles{6,2};                         
                        % DISCON.IN: change minimum generator speed
                        '6','I','! FlagLAC'         @(VariationValues) num2str(VariationValues(2),'%02d') 
                        % --- NEW: Enable Pitch Actuator (Using 'R' for safe string replacement) ---
                        '6','R','0                   ! PA_Mode',         '2                   ! PA_Mode'
                        '6','R','3.140000000000        ! PA_CornerFreq', '6.0                 ! PA_CornerFreq'
                        % --- NEW: Reduce Maximum Pitch Rate to 5 deg/s (0.08726 rad/s) ---
                        '6','R','0.174440000000      ! PC_MaxRat',   '0.087260000000      ! PC_MaxRat'
                        '6','R','-0.17444000000      ! PC_MinRat',   '-0.087260000000      ! PC_MinRat'
                        };

% PlotTimeResults
ID = 1;
PostProcessingConfig.Plots.BasicTimePlot{ID}.Enable     = 1;
PostProcessingConfig.Plots.BasicTimePlot{ID}.Channels   = {'Wind1VelX';'BldPitch1';'RotSpeed';'TwrBsMyt'};
PostProcessingConfig.Plots.BasicTimePlot{ID}.gca.xlim   = [0 50];
PostProcessingConfig.Plots.BasicTimePlot{ID}.legend     = MyLegend;

% CalculateStatistics
PostProcessingConfig.CalculateStatistics = {
    'max'           @(Data,Time)max(Data(Time>10)) {'RotSpeed'}
    }; 

% PlotStatistics
ID = 1;

%% Preprocessing and Processing
% generate simulation folder and copy all files from TemplateFolder to SimulationFolder
if ~exist(SimulationFolder,'dir')
    mkdir(SimulationFolder)
end
copyfile(TemplateFolder,SimulationFolder)
copyfile(['..\OpenFAST\',ExeFile],fullfile(SimulationFolder,ExeFile))

% get SimulationNames and DataFiles
[SimulationNames,DataFiles] = PreProcessingSimulations(SimulationFolder,PreProcessingVariation,InputFiles,Modifications,'OpenFAST');

% run simulations
[status,result] = ProcessingSimulations(SimulationFolder,SimulationNames,ExeFile,'OpenFAST');

%% Postprocessing
% TimeResults
TimeResults = CollectTimeResults(DataFiles,PostProcessingConfig);
PlotTimeResults(TimeResults,PostProcessingConfig);
    
% Statistics
Statistics = CalculateStatistics(TimeResults,PostProcessingConfig);
PlotStatistics(Statistics,PostProcessingConfig);  
