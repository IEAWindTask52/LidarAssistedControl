function [PostProcessingConfig,PreProcessingVariation,InputFiles,Modifications] = GetParametersForBruteForceOptimizationRegion1p5

% Process parameters
StartTime               = 60;       % [s]           time to start evaluation (all signals should be settled) 
WoehlerExponentSteel    = 4;        % [-]           typical value for steel

% files
StatisticsFile      	= 'Statistics_SteadyStates.mat';

% Variation
PreProcessingVariation  = { 'URef',[4:2:8],'%02d';
                            'Seed',[1:6]   ,'%02d';
                            'MinOMSpd',[3.5:0.5:8.0],'%3.1f'};

% template files
InputFiles{1,1} = 'IEA-10.0-198-RWT.fst';
InputFiles{2,1} = 'IEA-10.0-198-RWT_ElastoDyn.dat';
InputFiles{3,1} = 'IEA-10.0-198-RWT_InflowFile.dat';
InputFiles{4,1} = 'IEA-10.0-198-RWT_ServoDyn.dat';       
InputFiles{5,1} = 'IEA-10.0-198-RWT_DISCON.IN';

% new files to be modified
InputFiles{1,2}         = '<SimulationName>.fst';
InputFiles{2,2}         = '<SimulationName>_ElastoDyn.dat';
InputFiles{3,2}         = '<SimulationName>_InflowWind.dat';
InputFiles{4,2}         = '<SimulationName>_ServoDyn.dat';
InputFiles{5,2}         = '<SimulationName>_DISCON.IN';

% modifications in new files (for all Modes)
Modifications           = {
                        % OpenFAST: change time and link to new files
                        '1','I','TMax',                 num2str(600+StartTime)
                        '1','I','EDFile',               InputFiles{2,2}
                        '1','I','InflowFile',           InputFiles{3,2}  
                        '1','I','ServoFile',            InputFiles{4,2}   
                        % ElastoDyn: adjust initial conditions
                        '2','I','BlPitch\((1|2|3)\)',   @(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_BldPitch1',VariationValues(1)),'%5.2f')
                        '2','I','RotSpeed',             @(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_RotSpeed' ,VariationValues(1)),'%5.2f')                        
                        % InflowWind: change wind type and link to turbulent wind file
                        '3','I','WindType  '            '4'
                        '3','I','FilenameRoot',	        @(VariationValues)strcat('../TurbulentWind/URef_',num2str(VariationValues(1),'%02d'),'_Seed_',num2str(VariationValues(2),'%02d'))                       
                        % ServoDyn: Link to new in file
                        '4','I','DLL_InFile',           InputFiles{5,2};                        
                        % DISCON.IN: change minimum generator speed
                        '5','I','! VS_MinOMSpd'         @(VariationValues) num2str(rpm2radPs(VariationValues(3)),'%7.5f') 
                        };

% CalculateStatistics
PostProcessingConfig.CalculateStatistics = { 
    'mean'          @(Data,Time)mean(Data(Time>=StartTime)) {'GenPwr'}
    'DEL_4'         @(Data,Time)CalculateDEL(Data(Time>=StartTime),Time(Time>=StartTime),WoehlerExponentSteel)      {'TwrBsMyt'}
    }; 

% CalculateFrequencyResults
Step                    = 50;
nData                   = 600*100/Step; % assuming 100 Hz and 10 min
nBlocks                 = 4;
window                  = nData/nBlocks;
AutoSpectrumChannels    = {'TwrBsMyt';};
PostProcessingConfig.CalculateFrequencyResults = {
    'AutoSpectrum'      @(Data,Time)EstimateAutoSpectrum(Data,Time,Step,StartTime=StartTime,window=window)  AutoSpectrumChannels;
    }; 

% CalculateProcessResults
PostProcessingConfig.CalculateProcessResults = {
    @(ProcessResults,FrequencyResults,Statistics) DataFilterPreProcessing(ProcessResults,PreProcessingVariation)
    @(ProcessResults,FrequencyResults,Statistics) CalculatePreProcessingDEL(ProcessResults,Statistics,'DEL_4_TwrBsMyt');
    @(ProcessResults,FrequencyResults,Statistics) CalculatePreProcessingMean(ProcessResults,Statistics,'mean_GenPwr');    
    @(ProcessResults,FrequencyResults,Statistics) EstimateAveragedAutoSpectra(ProcessResults,FrequencyResults,'AutoSpectrum_TwrBsMyt')
    };

% PlotProcessResults
nURef               = length(PreProcessingVariation{1,2});
MyXlim              = [1e-3 1e0];
MinOMSpd            = PreProcessingVariation{3,2};
MyLegend            = "MinOMSpd = " + MinOMSpd' +" rpm";

ID = 1;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.Enable          = nURef==1;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.Channels        = {'mean_AutoSpectrum_TwrBsMyt'};
PostProcessingConfig.Plots.BasicProcessPlot{ID}.x               = 'f';
PostProcessingConfig.Plots.BasicProcessPlot{ID}.y               = 'S';        
PostProcessingConfig.Plots.BasicProcessPlot{ID}.ylabel          = '[(Nm)^2/Hz]';  
PostProcessingConfig.Plots.BasicProcessPlot{ID}.xlabel          = 'frequency [Hz]'; 
PostProcessingConfig.Plots.BasicProcessPlot{ID}.title           = "mean tower base bending moment spectra";  
PostProcessingConfig.Plots.BasicProcessPlot{ID}.gca.xlim        = MyXlim;  
PostProcessingConfig.Plots.BasicProcessPlot{ID}.gca.xScale      = 'log';
PostProcessingConfig.Plots.BasicProcessPlot{ID}.gca.yScale      = 'log';
PostProcessingConfig.Plots.BasicProcessPlot{ID}.legend          = MyLegend;

ID = ID+1;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.Enable          = nURef==1;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.Channels        = {'mean_GenPwr_Weighted';'DEL_4_TwrBsMyt_Weighted'};
PostProcessingConfig.Plots.BasicProcessPlot{ID}.x               = 'MinOMSpd';
PostProcessingConfig.Plots.BasicProcessPlot{ID}.yCell           = {'Mean';'DEL'};
PostProcessingConfig.Plots.BasicProcessPlot{ID}.ylabelCell      = {'[MW]';'[MNm]'};
PostProcessingConfig.Plots.BasicProcessPlot{ID}.xlabel          = 'MinOMSpd [rpm]'; 
PostProcessingConfig.Plots.BasicProcessPlot{ID}.titleCell       = {'lifetime weighted mean power';'lifetime weighted tower loads'};  
PostProcessingConfig.Plots.BasicProcessPlot{ID}.gca.LineStyleOrder      = '.-';
PostProcessingConfig.Plots.BasicProcessPlot{ID}.yScale                  = 1e-3;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.AdditionalPlotInputs    = {'MarkerSize',10};

ID = ID+1;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.Enable          = nURef>1;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.Channels        = {'mean_GenPwr_Lifetime';'DEL_4_TwrBsMyt_Lifetime'};
PostProcessingConfig.Plots.BasicProcessPlot{ID}.x               = 'MinOMSpd';
PostProcessingConfig.Plots.BasicProcessPlot{ID}.yCell           = {'Mean';'DEL'};
PostProcessingConfig.Plots.BasicProcessPlot{ID}.ylabelCell      = {'[MW]';'[MNm]'};
PostProcessingConfig.Plots.BasicProcessPlot{ID}.xlabel          = 'MinOMSpd [rpm]'; 
PostProcessingConfig.Plots.BasicProcessPlot{ID}.titleCell       = {'lifetime weighted mean power';'lifetime weighted tower loads'};  
PostProcessingConfig.Plots.BasicProcessPlot{ID}.gca.LineStyleOrder      = '.-';
PostProcessingConfig.Plots.BasicProcessPlot{ID}.yScale                  = 1e-3;
PostProcessingConfig.Plots.BasicProcessPlot{ID}.AdditionalPlotInputs    = {'MarkerSize',10};

end