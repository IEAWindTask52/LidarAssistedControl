function [PostProcessingConfig,PreProcessingVariation,InputFiles,Modifications] = GetParametersForDLC1p2(SimulationMode)

% inputs
arguments
    SimulationMode    char {mustBeMember(SimulationMode,{'FeedbackOnly','LAC_CircularCW','LAC_4BeamPulsed'})}  
end

% Process parameters
StartTime               = 60;       % [s]           time to start evaluation (all signals should be settled) 
WoehlerExponentSteel    = 4;        % [-]           typical value for steel
WoehlerExponentComposite= 10;        % [-]          typical value for composite material
PC_RefSpd               = 0.79168;  % [rad/s]       rated generator speed from ROSCO_v2d6.IN

% files
StatisticsFile      	= 'Statistics_SteadyStates.mat';

% Variation
PreProcessingVariation  = { 'URef',[4:2:24],'%02d';
                            'Seed',[1:6]   ,'%02d'};

% template files
InputFiles{1,1}         = 'IEA-15-240-RWT-Monopile.fst';            % main file
InputFiles{2,1}         = 'IEA-15-240-RWT-Monopile_ElastoDyn.dat';  % to adjust initial conditions
InputFiles{3,1}         = 'IEA-15-240-RWT_InflowFile.dat';          % to adjust wind speed
switch SimulationMode
    case 'LAC_CircularCW'
        InputFiles{4,1} = 'LidarFile_CircularCW.dat';               % to adjust wind speed
        InputFiles{5,1} = 'IEA-15-240-RWT-Monopile_ServoDyn.dat';   % to link to new WRAPPER_CircularCW.IN
        InputFiles{6,1} = 'WRAPPER_CircularCW.IN';                  % to link to new FFP_v1_CircularCW.IN and ROSCO_v2d6.IN
        InputFiles{7,1} = 'ROSCO_v2d6.IN';                          % to enable FF
        InputFiles{8,1} = 'FFP_v1_CircularCW.IN';                   % to adjust f_cutoff and T_buffer
    case 'LAC_4BeamPulsed'    
        InputFiles{4,1} = 'LidarFile_4BeamPulsed.dat';              % to adjust wind speed
        InputFiles{5,1} = 'IEA-15-240-RWT-Monopile_ServoDyn.dat';   % to link to new WRAPPER_4BeamPulsed.IN
        InputFiles{6,1} = 'WRAPPER_4BeamPulsed.IN';                 % to link to new FFP_v1_4BeamPulsed.IN and ROSCO_v2d6.IN
        InputFiles{7,1} = 'ROSCO_v2d6.IN';                          % to enable FF
        InputFiles{8,1} = 'FFP_v1_4BeamPulsed.IN';                  % to adjust f_cutoff and T_buffer
end

% new files to be modified
InputFiles{1,2}         = '<SimulationName>.fst';
InputFiles{2,2}         = '<SimulationName>_ElastoDyn.dat';
InputFiles{3,2}         = '<SimulationName>_InflowWind.dat';
switch SimulationMode
    case {'LAC_CircularCW','LAC_4BeamPulsed'}
        InputFiles{4,2} = '<SimulationName>_LidarFile.dat';
        InputFiles{5,2} = '<SimulationName>_ServoDyn.dat';
        InputFiles{6,2} = '<SimulationName>_WRAPPER.IN';
        InputFiles{7,2} = 'ROSCO_v2d6_LAC.IN';
        InputFiles{8,2} = '<SimulationName>_FFP_v1.IN';       
end

% modifications in new files (for all Modes)
Modifications           = {
                        % OpenFAST: change time and link to new files
                        '1','I','TMax',                 num2str(600+StartTime)
                        '1','I','EDFile',               InputFiles{2,2}
                        '1','I','InflowFile',           InputFiles{3,2}                        
                        % ElastoDyn: adjust initial conditions
                        '2','I','BlPitch\((1|2|3)\)',   @(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_BldPitch1',VariationValues(1)),'%5.2f')
                        '2','I','RotSpeed',             @(VariationValues)num2str(GetStatistics(StatisticsFile,'mean_RotSpeed' ,VariationValues(1)),'%5.2f')                        
                        % InflowWind: change wind type and link to turbulent wind file
                        '3','I','WindType  '            '4'
                        '3','I','FilenameRoot',	        @(VariationValues)strcat('../TurbulentWind/URef_',num2str(VariationValues(1),'%02d'),'_Seed_',num2str(VariationValues(2),'%02d'))                       
                        };


% FFP_v1 Parameters
URef_v                  = 8:2:24; % [m/s]
switch SimulationMode
    case 'LAC_CircularCW'
        f_cutoff_v      = [ 0.1453    0.1816    0.2179    0.2542    0.2905    0.3268    0.3632    0.3995    0.4358]; % [rad/s]
        T_buffer_v      = [16.8750   13.0000   10.4167    8.5714    7.1875    6.1111    5.2500    4.5455    3.9583]; % [s]
    case 'LAC_4BeamPulsed'
        f_cutoff_v      = [0.0547    0.0684    0.0821    0.0958    0.1095    0.1232    0.1369    0.1505    0.1642]; % [rad/s]
        T_buffer_v      = [6.2500    4.5000    3.3333    2.5000    1.8750    1.3889    1.0000    0.6818    0.4167]; % [s] 
end

% additional modifications for LAC modes
switch SimulationMode
    case {'LAC_CircularCW','LAC_4BeamPulsed'}
        Modifications   = cat(1,Modifications,{
                        % OpenFAST: link to new files                    
                        '1','I','SWELidarFile',         InputFiles{4,2}
                        '1','I','ServoFile',            InputFiles{5,2}
                         % LidarFile: adjust wind speed
                        '4','I','URef'                  @(VariationValues)num2str(VariationValues(1))
                        % ServoDyn: link to Wrapper
                        '5','I','DLL_FileName',         'WRAPPER.dll'
                        '5','I','DLL_InFile',           InputFiles{6,2}
                        % Wrapper: link to new ROSCO_v2d6.IN
                        '6','R',InputFiles{7,1}         InputFiles{7,2}   
                        '6','R',InputFiles{8,1}         InputFiles{8,2}
                        % Rosco: enable FF
                        '7','I','! FlagLAC'             '1'
                        % FFP_v1: adjust f_cutoff and T_buffer
                        '8','I','! f_cutoff'            @(VariationValues)num2str(interp1(URef_v,f_cutoff_v, min(max(VariationValues(1),URef_v(1)),URef_v(end)) ),'%8.4f') % use end values
                        '8','I','! T_buffer'            @(VariationValues)num2str(interp1(URef_v,T_buffer_v, min(max(VariationValues(1),URef_v(1)),URef_v(end)) ),'%8.4f') % use end values
                        });
end

% PlotTimeResults
nURef       = length(PreProcessingVariation{1,2});
nSeed       = length(PreProcessingVariation{2,2});
for iURef = 1:nURef
    ID = iURef;
    PostProcessingConfig.Plots.BasicTimePlot{ID}.Enable     = 1;
    PostProcessingConfig.Plots.BasicTimePlot{ID}.Channels   = {'Wind1VelX';'BldPitch1';'RotSpeed';} ;
    PostProcessingConfig.Plots.BasicTimePlot{ID}.gca.xlim   = [0 600]+StartTime;
    PostProcessingConfig.Plots.BasicTimePlot{ID}.IndicesConsideredDataFiles = [1:nSeed]+(iURef-1)*nSeed;
end

% CalculateStatistics
PostProcessingConfig.CalculateStatistics = { 
    'mean'          @(Data,Time)mean(Data(Time>=StartTime)) {'Wind1VelX';'GenPwr'}
    'DEL_4'         @(Data,Time)CalculateDEL(Data(Time>=StartTime),Time(Time>=StartTime),WoehlerExponentSteel)      {'TwrBsMyt';'RotTorq'}
    'DEL_10'        @(Data,Time)CalculateDEL(Data(Time>=StartTime),Time(Time>=StartTime),WoehlerExponentComposite)  {'RootMyb1'}
    'Overshoot'     @(Data,Time)max(max(rpm2radPs(Data(Time>=StartTime))-PC_RefSpd)/PC_RefSpd,0) {'GenSpeed'}
    'Travel'        @(Data,Time)CalculatePitchTravel(Data,Time,StartTime) {'BldPitch1'}
    'max'           @(Data,Time)max(Data(Time>=StartTime)) {'GenTq'}    
    }; 

end