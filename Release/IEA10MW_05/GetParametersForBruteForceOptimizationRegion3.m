
function [PostProcessingConfig,PreProcessingVariation,InputFiles,Modifications] = GetParametersForBruteForceOptimizationRegion3(SimulationMode)

% ============================================================
% DLC 1.2 - Region 3 Brute Force Optimization
% Optimization variable: theta_K = 3 and 6 degrees
% Wind speeds: 12, 16, 20, 24 m/s
% Seeds: 1, 2, 3
% Improvements:
% 1) Minimum generator speed = 7 rpm
% 2) Low-pass corner frequency = 3 Hz
% 3) Gain scheduling using theta_K
% ============================================================

arguments
    SimulationMode char {mustBeMember(SimulationMode,{'FeedbackOnly'})}
end

%% General settings

StartTime = 60;
WoehlerExponentSteel = 4;
WoehlerExponentComposite = 10;
PC_RefSpd = 0.90767;
StatisticsFile = 'Statistics_SteadyStates.mat';

%% Optimization variables

ThetaK_deg = [3 6];
URef = [12 16 20 24];
Seeds = [1 2 3];

PreProcessingVariation = {'URef',URef,'%02d';'Seed',Seeds,'%02d';'ThetaK',ThetaK_deg,'%02d'};

%% Template files and simulation-specific files

InputFiles{1,1} = 'IEA-10.0-198-RWT.fst';
InputFiles{2,1} = 'IEA-10.0-198-RWT_ElastoDyn.dat';
InputFiles{3,1} = 'IEA-10.0-198-RWT_InflowFile.dat';
InputFiles{4,1} = 'IEA-10.0-198-RWT_ServoDyn.dat';
InputFiles{5,1} = 'IEA-10.0-198-RWT_DISCON.IN';

InputFiles{1,2} = '<SimulationName>.fst';
InputFiles{2,2} = '<SimulationName>_ElastoDyn.dat';
InputFiles{3,2} = '<SimulationName>_InflowWind.dat';
InputFiles{4,2} = '<SimulationName>_ServoDyn.dat';
InputFiles{5,2} = '<SimulationName>_DISCON.IN';

%% Calculate integral time constant

OmegaRated = PC_RefSpd;
Ti = 2*pi/OmegaRated;

% Rated proportional gain from original ROSCO controller
KpRated = -1.869680;

%% Pitch gain-scheduling angles

ThetaAngles = [0.074715 0.104855 0.127756 0.147841 0.166006 0.182149 0.197673 0.211917 0.225671 0.238919 0.251384 0.263856 0.275558 0.287072 0.298597 0.309358 0.320134 0.330914 0.341068 0.351235 0.361422 0.371155 0.380809 0.390472 0.399908 0.409125 0.418341 0.427550 0.436381 0.445192];

% Gain-scheduling formula
MakeKP = @(ThetaKDeg) KpRated./(1 + ThetaAngles./deg2rad(ThetaKDeg));
MakeKI = @(ThetaKDeg) MakeKP(ThetaKDeg)./Ti;

%% Modifications

Modifications = {
    % OpenFAST: simulation time and file connections
    '1','I','TMax',num2str(600+StartTime)
    '1','I','EDFile',InputFiles{2,2}
    '1','I','InflowFile',InputFiles{3,2}
    '1','I','ServoFile',InputFiles{4,2}

    % ElastoDyn: initial conditions
    '2','I','BlPitch\((1|2|3)\)',@(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_BldPitch1',VariationValues(1)),'%5.2f')
    '2','I','RotSpeed',@(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_RotSpeed',VariationValues(1)),'%5.2f')

    % InflowWind
    '3','I','WindType  ','4'
    '3','I','FilenameRoot',@(VariationValues)strcat('../TurbulentWind/URef_',num2str(VariationValues(1),'%02d'),'_Seed_',num2str(VariationValues(2),'%02d'))

    % ServoDyn: connect to simulation-specific ROSCO input file
    '4','I','DLL_InFile',InputFiles{5,2}

    % ROSCO DISCON.IN: minimum generator speed
    '5','I','VS_MinOMSpd',num2str(rpm2radPs(7),'%.12f')

    % ROSCO DISCON.IN: low-pass corner frequency = 3 Hz
    '5','I','F_LPFCornerFreq',num2str(2*pi*3,'%.6f')

    % ROSCO DISCON.IN: theta_K-dependent KP
    '5','I','PC_GS_KP',@(VariationValues)strjoin(arrayfun(@(x)num2str(x,'%.6f'),MakeKP(VariationValues(3)),'UniformOutput',false),' ')

    % ROSCO DISCON.IN: theta_K-dependent KI
    '5','I','PC_GS_KI',@(VariationValues)strjoin(arrayfun(@(x)num2str(x,'%.6f'),MakeKI(VariationValues(3)),'UniformOutput',false),' ')
    };

%% Time plots: separate figures for each theta_K and wind speed

nURef = length(URef);
nSeed = length(Seeds);
nThetaK = length(ThetaK_deg);

for iThetaK = 1:nThetaK
    for iURef = 1:nURef
        ID = (iThetaK-1)*nURef+iURef;
        PostProcessingConfig.Plots.BasicTimePlot{ID}.Enable = 1;
        PostProcessingConfig.Plots.BasicTimePlot{ID}.Channels = {'Wind1VelX';'BldPitch1';'RotSpeed';'TwrBsMyt'};
        PostProcessingConfig.Plots.BasicTimePlot{ID}.gca.xlim = [0 600]+StartTime;
        Indices = (iURef-1)*nSeed*nThetaK+iThetaK+(0:nSeed-1)*nThetaK;
        PostProcessingConfig.Plots.BasicTimePlot{ID}.IndicesConsideredDataFiles = Indices;
    end
end

%% Statistics

PostProcessingConfig.CalculateStatistics = {
    'mean',@(Data,Time)mean(Data(Time>=StartTime)),{'Wind1VelX';'GenPwr'}
    'DEL_4',@(Data,Time)CalculateDEL(Data(Time>=StartTime),Time(Time>=StartTime),WoehlerExponentSteel),{'TwrBsMyt';'RotTorq'}
    'DEL_10',@(Data,Time)CalculateDEL(Data(Time>=StartTime),Time(Time>=StartTime),WoehlerExponentComposite),{'RootMyb1'}
    'Overshoot',@(Data,Time)max(max(rpm2radPs(Data(Time>=StartTime))-PC_RefSpd)/PC_RefSpd,0),{'GenSpeed'}
    'Travel',@(Data,Time)CalculatePitchTravel(Data,Time,StartTime),{'BldPitch1'}
    'max',@(Data,Time)max(Data(Time>=StartTime)),{'GenTq'}
    };

%% Frequency analysis

Step = 50;
nData = 600*100/Step;
nBlocks = 4;
window = nData/nBlocks;
AutoSpectrumChannels = {'RotSpeed'};

PostProcessingConfig.CalculateFrequencyResults = {
    'AutoSpectrum',@(Data,Time)EstimateAutoSpectrum(Data,Time,Step,StartTime=StartTime,window=window),AutoSpectrumChannels
    };

%% Frequency plots

for iURef = 1:nURef
    ID = iURef;
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.Enable = 1;
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.Channels = {'AutoSpectrum_RotSpeed'};
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.x = 'f';
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.yCell = {'S'};
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.ylabelCell = {'[(rad/s)^2/Hz]'};
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.xlabel = 'frequency [Hz]';
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.gca.xScale = 'log';
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.gca.yScale = 'log';
    nPerWind = nSeed*nThetaK;
    PostProcessingConfig.Plots.BasicFrequencyPlot{ID}.IndicesConsideredDataFiles = [1:nPerWind]+(iURef-1)*nPerWind;
end

end
