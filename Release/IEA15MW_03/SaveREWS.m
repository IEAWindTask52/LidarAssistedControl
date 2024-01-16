% Simple Script to save REWS in mat-file as work-around.
% Authors:
% David Schlipf

%% Setup
clearvars;
close all;
clc;
addpath('..\MatlabFunctions')

nSeed               = 6;                        % [-]	number of stochastic turbulence field samples
Seed_vec            = [1:nSeed]+18*100;         % [-]  	vector of seeds
R                   = 120;                      % [m]  	rotor radius to calculate REWS
Time                = 0:0.0125:660;             % [s]   time vector of simulations

% loop over seeds
for iSeed = 1:nSeed    

    % Load data
    Seed                                = Seed_vec(iSeed);
	WindFileName                        = ['URef_18_Seed_',num2str(Seed,'%04d')];
    TurbSimResultFile                 	= ['TurbulentWind\',WindFileName,'.wnd'];   
    [REWS_WindField,Time_WindField]  	= CalculateREWSfromWindField(TurbSimResultFile,R,2);
    REWS_WindField_Fs                   = interp1(Time_WindField,REWS_WindField,Time);
     
    % Plot REWS
    figure('Name',['REWS seed ',num2str(Seed)])
    hold on; grid on; box on
    plot(Time,   REWS_WindField_Fs);
    ylabel('REWS [m/s]');
    xlabel('time [s]')

    % load into cell
    t_all{iSeed}    = Time;
    v_0_all{iSeed}  = REWS_WindField_Fs;

end

save('CaluculateRWESfromWindField.mat','v_0_all','t_all');