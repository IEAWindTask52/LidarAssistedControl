% Generatates wind fields for DLC 1.2 running TurbSim.

%% Setup
clearvars;
close all;
clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Parameters (can be adjusted, but will provide different results)
URef_vector         = 4:2:24;   % [m/s]         range of wind speeds (operation points)
n_Seed              = 6;        % [-]           number of stochastic turbulence field seeds
R                   = 198/2;    % [m]           rotor radius for REWS calculation
MyXlim              = [60 660]; % [s]           time limits for RWES plot representing the analyzed simulation time
GenerateUniformWind = true;     % [true/false]  flag to generate uniform wind for perfect preview
alpha               = 0.14;     % [-]           vertical power-law wind-shear exponent, same as in TurbSimInputFileTemplateIEA10MW.inp

% Seed Matrix Definition
Seed_vector         = [1:n_Seed];
n_URef              = length(URef_vector);
Seed_matrix         = repmat(URef_vector',1,n_Seed)*100+repmat(Seed_vector,n_URef,1);

% Files (should not be changed)
TurbSimExeFile      = 'TurbSim_x64.exe';
TurbSimTemplateFile = 'TurbSimInputFileTemplateIEA10MW.inp';

% Generate folder 
if ~exist('TurbulentWind','dir')
    mkdir TurbulentWind
end

%% Preprocessing: generate turbulent wind field

% Copy the adequate TurbSim version to the example folder 
copyfile(['..\TurbSim\',TurbSimExeFile],['TurbulentWind\',TurbSimExeFile])
    
% Generate all wind fields for different URef and RandSeed1
for i_URef    = 1:n_URef
    URef      = URef_vector(i_URef);
    parfor i_Seed = 1:n_Seed          
        Seed                = Seed_matrix(i_URef,i_Seed);
        WindFileName        = ['URef_',num2str(URef,'%02d'),'_Seed_',num2str(i_Seed,'%02d')];
        TurbSimInputFile  	= ['TurbulentWind\',WindFileName,'.ipt'];
        TurbSimResultFile  	= ['TurbulentWind\',WindFileName,'.wnd'];
        if ~exist(TurbSimResultFile,'file')
            copyfile([TurbSimTemplateFile],TurbSimInputFile)
            % Adjust the TurbSim input file
            ManipulateFastInputFile(TurbSimInputFile,'URef ',     num2str(URef,'%4.1d'));   % adjust URef
            ManipulateFastInputFile(TurbSimInputFile,'RandSeed1 ',num2str(Seed));        	% adjust seed
            % Generate wind field
            dos(['TurbulentWind\',TurbSimExeFile,' ',TurbSimInputFile]);
        end
    end
end

% Clean up
delete(['TurbulentWind\',TurbSimExeFile])

%% Calculate REWS
REWS = cell(n_URef,n_Seed);

figure
for i_URef    = 1:n_URef
    subplot(n_URef,1,i_URef);
    hold on;box on;grid on
    URef      = URef_vector(i_URef);
    for i_Seed = 1:n_Seed        
        Seed                        = Seed_matrix(i_URef,i_Seed);
        WindFileName                = ['URef_',num2str(URef,'%02d'),'_Seed_',num2str(i_Seed,'%02d')];
        TurbSimResultFile  	        = ['TurbulentWind\',WindFileName,'.wnd'];

        [REWS_WindField,Time_WindField]  	= CalculateREWSfromWindField(TurbSimResultFile,R,2);  

        REWS{i_URef,i_Seed}.Time    = Time_WindField;
        REWS{i_URef,i_Seed}.Data    = REWS_WindField;

        % plot
        plot(Time_WindField,REWS_WindField)     

    end
    title(URef+" m/s")
    xlim(MyXlim)
    ylabel('REWS [m/s]')
end
xlabel('time [s]')


%% Generate Uniform wind
if GenerateUniformWind

    % Generate folder 
    if ~exist('UniformWind','dir')
        mkdir UniformWind
    end

    % loop over wind speeds and seeds
    for i_URef    = 1:n_URef
        URef      = URef_vector(i_URef);
        for i_Seed = 1:n_Seed        

            % get details
            Seed                        = Seed_matrix(i_URef,i_Seed);
            WindFileName                = ['URef_',num2str(URef,'%02d'),'_Seed_',num2str(i_Seed,'%02d')];
            ResultFile  	            = ['UniformWind\',WindFileName,'.wnd'];
            n_data                      = length(REWS{i_URef,i_Seed}.Data);
            t                           = REWS{i_URef,i_Seed}.Time;
            u                           = REWS{i_URef,i_Seed}.Data;

            % write file
            fid = fopen(ResultFile,'w+');
            fprintf(fid,['%f,%f,%f,%f,%f,%f,%f,%f\r\n'],[t u zeros(n_data,3)  ones(n_data,1)*alpha zeros(n_data,2)]');
            fclose(fid);
            
        end
    end

end
