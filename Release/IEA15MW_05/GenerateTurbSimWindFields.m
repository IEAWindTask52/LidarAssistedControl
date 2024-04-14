% Generatates wind fields for DLC 1.2 running TurbSim.

%% Setup
clearvars;
close all;
clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Parameters (can be adjusted, but will provide different results)
URef_vector         = 4:2:24;   % [m/s]         range of wind speeds (operation points)
n_Seed              = 6;        % [-]           number of stochastic turbulence field seeds

% Seed Matrix Definition
Seed_vector         = [1:n_Seed];
n_URef              = length(URef_vector);
Seed_matrix         = repmat(URef_vector',1,n_Seed)*100+repmat(Seed_vector,n_URef,1);

% Files (should not be changed)
TurbSimExeFile      = 'TurbSim_x64.exe';
TurbSimTemplateFile = 'TurbSimInputFileTemplateIEA15MW.inp';

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