% IEA15MW_03: IEA 15 MW monopile + realistic wind preview  from a 
% MolasNL200 lidar system, single wind speed. 
% Purpose:
% Here, we use a realistic wind preview to demonstrate that the collective
% pitch feedforward controller together with the correct filtering provides
% the reduction in rotor speed variation as predicted by the linear model
% and the coherence. In this example, we assume frozen turbulence, only one 
% 3D turbulence field (y,z,t) at rotor plane is generated.
% Result:
% Change in rotor speed standard deviation:  -50.3 %
% Authors:
% David Schlipf, Feng Guo

%% Setup
clearvars;
close all;
clc;
addpath('..\MatlabFunctions')
addpath('..\MatlabFunctions\AnalyticalModel')

% Seeds (can be adjusted, but will provide different results)
nSeed               = 6;                        % [-]	number of stochastic turbulence field samples
Seed_vec            = [1:nSeed];                % [-]  	vector of seeds

% Parameters postprocessing (can be adjusted, but will provide different results)
t_start             = 60;                       % [s] 	ignore data before for STD and spectra
Fs                	= 80;                       % [Hz]  sampling frequenzy, same as in *.fst
nDataPerBlock       = 600/2*Fs;                 % [-]  	data per block, here 2 blocks per 600 s (TMax-t_start)
vWindow             = hamming(nDataPerBlock);   % [-] 	window for estimation
nFFT                = [];                       % [-]  	number of FFT, default: 2^nextpow2(nDataPerBlock); 
nOverlap            = [];                       % [-]  	samples of overlap, default: 50% overlap
R                   = 120;                      % [m]  	rotor radius to calculate REWS

% Files (should not be be changed)
TurbSimExeFile      = 'TurbSim_x64.exe';
FASTexeFile         = 'openfast_x64.exe';
FASTmapFile         = 'MAP_x64.dll';
SimulationName      = 'IEA-15-240-RWT-Monopile_MolasNL200';
TurbSimTemplateFile = 'TurbSim2aInputFileTemplateIEA15MW.inp';
if ~exist('TurbulentWind','dir')
    mkdir TurbulentWind
end
if ~exist('SimulationResults','dir')
    mkdir SimulationResults
end

%% Preprocessing: generate turbulent wind field
    
% Copy the adequate TurbSim version to the example folder 
copyfile(['..\TurbSim\',TurbSimExeFile],['TurbulentWind\',TurbSimExeFile])
    
% Generate all wind fields
for iSeed = 1:nSeed        
    Seed                = Seed_vec(iSeed);
    WindFileName        = ['URef_18_Seed_',num2str(Seed,'%02d')];
    TurbSimInputFile  	= ['TurbulentWind\',WindFileName,'.ipt'];
    TurbSimResultFile  	= ['TurbulentWind\',WindFileName,'.wnd'];
    if ~exist(TurbSimResultFile,'file')
        copyfile([TurbSimTemplateFile],TurbSimInputFile)
        ManipulateTXTFile(TurbSimInputFile,'MyRandSeed1',num2str(Seed));% adjust seed
        dos(['TurbulentWind\',TurbSimExeFile,' ',TurbSimInputFile]);
    end
end
    
% Clean up
delete(['TurbulentWind\',TurbSimExeFile])

%% Processing: run simulations

% Copy the adequate OpenFAST version to the example folder
copyfile(['..\OpenFAST\',FASTexeFile],FASTexeFile)
copyfile(['..\OpenFAST\',FASTmapFile],'MAP_x64.dll')

% Simulate with all wind fields
for iSeed = 1:nSeed
    
    % Adjust the InflowWind file
    Seed                = Seed_vec(iSeed);
	WindFileName        = ['URef_18_Seed_',num2str(Seed,'%02d')];
	WindFileRoot        = ['TurbulentWind\',WindFileName];
    ManipulateTXTFile('IEA-15-240-RWT_InflowFile.dat','MyFilenameRoot',WindFileRoot);
    
    % Run FB    
    FASTresultFile      = ['SimulationResults\',WindFileName,'_FlagLAC_0.outb'];
    ROSCOresultFile     = ['SimulationResults\',WindFileName,'_FlagLAC_0.dbg'];
    if ~exist(FASTresultFile,'file')    
        ManipulateTXTFile('ROSCO_v2d6.IN','1 ! FlagLAC','0 ! FlagLAC'); % disable LAC
        dos([FASTexeFile,' ',SimulationName,'.fst']);
        movefile([SimulationName,'.outb'],FASTresultFile)
        movefile([SimulationName,'.RO.dbg'],ROSCOresultFile)   % store rosco output file
    end
   
    % Run FB+FF    
    FASTresultFile      = ['SimulationResults\',WindFileName,'_FlagLAC_1.outb'];
    ROSCOresultFile     = ['SimulationResults\',WindFileName,'_FlagLAC_1.dbg'];
    if ~exist(FASTresultFile,'file')    
        ManipulateTXTFile('ROSCO_v2d6.IN','0 ! FlagLAC','1 ! FlagLAC'); % enable LAC
        dos([FASTexeFile,' ',SimulationName,'.fst']);
        movefile([SimulationName,'.outb'],FASTresultFile)
     	movefile([SimulationName,'.RO.dbg'],ROSCOresultFile)   % store rosco output file
    end    
    
    % Reset the InflowWind file again
    ManipulateTXTFile('IEA-15-240-RWT_InflowFile.dat',WindFileRoot,'MyFilenameRoot');
end

% Clean up
delete(FASTexeFile)
delete('MAP_x64.dll')

%% Postprocessing: evaluate data

for iSeed = 1:nSeed    

    % Load data
    Seed                = Seed_vec(iSeed);
	WindFileName        = ['URef_18_Seed_',num2str(Seed,'%02d')];
    FASTresultFile      = ['SimulationResults\',WindFileName,'_FlagLAC_0.outb'];
    ROSCOresultFile     = ['SimulationResults\',WindFileName,'_FlagLAC_0.dbg'];
    FB                  = ReadFASTbinaryIntoStruct(FASTresultFile);
    R_FB                = ReadROSCOtextIntoStruct(ROSCOresultFile);
    FASTresultFile      = ['SimulationResults\',WindFileName,'_FlagLAC_1.outb'];
    ROSCOresultFile     = ['SimulationResults\',WindFileName,'_FlagLAC_1.dbg'];
    FBFF                = ReadFASTbinaryIntoStruct(FASTresultFile);
	R_FBFF              = ReadROSCOtextIntoStruct(ROSCOresultFile);

    % Plot rotor speed
    figure('Name',['Rotor speed seed ',num2str(Seed)])
    hold on; grid on; box on
    plot(FB.Time,       FB.RotSpeed);
    plot(FBFF.Time,     FBFF.RotSpeed);
    ylabel('RotSpeed [rpm]');
    legend('feedback only','feedback-feedforward')
    xlabel('time [s]')

    % Estimate rotor speed spectra
    [S_RotSpeed_FB_est(iSeed,:),f_est]	= pwelch(detrend(FB.RotSpeed  (FB.Time  >t_start),'constant'),vWindow,nOverlap,nFFT,Fs);
    [S_RotSpeed_FBFF_est(iSeed,:),~]  	= pwelch(detrend(FBFF.RotSpeed(FBFF.Time>t_start),'constant'),vWindow,nOverlap,nFFT,Fs);
    
    % Calculate standard deviation rotor speed
    STD_RotSpeed_FB  (iSeed)          	= std(FB.RotSpeed  (FB.Time  >t_start));
    STD_RotSpeed_FBFF(iSeed)          	= std(FBFF.RotSpeed(FBFF.Time>t_start));
    
    % Estimate auto- and cross-spectra of REWS
    TurbSimResultFile                 	= ['TurbulentWind\URef_18_Seed_',num2str(Seed,'%02d'),'.wnd'];   
    [REWS_WindField,Time_WindField]  	= CalculateREWSfromWindField(TurbSimResultFile,R,2);
    REWS_WindField_Fs                   = interp1(Time_WindField,REWS_WindField,R_FBFF.Time);
    [S_LL_est(iSeed,:) ,~]            	= pwelch(detrend(R_FBFF.REWS(R_FBFF.Time>=t_start),'constant'),vWindow,nOverlap,nFFT,Fs);     
    [S_RR_est(iSeed,:) ,~]            	= pwelch(detrend(REWS_WindField_Fs(R_FBFF.Time>=t_start),'constant'),vWindow,nOverlap,nFFT,Fs);     
    [S_RL_est(iSeed,:) ,~]             	= cpsd(detrend(REWS_WindField_Fs(R_FBFF.Time>=t_start),'constant'),detrend(R_FBFF.REWS(R_FBFF.Time>=t_start),'constant'),vWindow,nOverlap,nFFT,Fs);     
    
    % Plot REWS
    figure('Name',['REWS seed ',num2str(Seed)])
    hold on; grid on; box on
    plot(R_FBFF.Time,   REWS_WindField_Fs);
    plot(R_FBFF.Time,   R_FBFF.REWS);
    ylabel('REWS [m/s]');
    legend('wind field','lidar estimate')
    xlabel('time [s]')

    % Estimate cross correlation
    [c_filter(iSeed,:),lags]    = xcorr(detrend(R_FBFF.REWS_f(R_FBFF.Time>=t_start),'constant'),detrend(R_FBFF.REWS(R_FBFF.Time>=t_start),'constant'),'normalized');

end

% calculate mean coherence
gamma2_RL_mean_est          = abs(mean(S_RL_est,1)).^2./mean(S_LL_est,1)./mean(S_RR_est,1);

% Get analytical correlation model
SpectralModelFileName       = 'LidarRotorSpectra_IEA15MW_MolasNL200_160m.mat'; % model for 18 m/s
AnalyticalModel             = load(SpectralModelFileName, 'S_LL', 'S_RL', 'S_RR', 'f'); 
AnalyticalModel.gamma2_RL   = abs(AnalyticalModel.S_RL).^2./AnalyticalModel.S_RR./AnalyticalModel.S_LL;

%% Plot rotor speed spectra
figure('Name','Rotor speed spectra')
hold on; grid on; box on
plot(f_est ,mean(S_RotSpeed_FB_est,1));
plot(f_est ,mean(S_RotSpeed_FBFF_est,1));
set(gca,'Xscale','log')
set(gca,'Yscale','log')
xlabel('frequency [Hz] ')
ylabel('Spectra RotSpeed [(rpm)^2Hz^{-1}]')
legend('FB-only Estimated','FBFF Estimated')

% display results
fprintf('Change in rotor speed standard deviation:  %4.1f %%\n',...
    (mean(STD_RotSpeed_FBFF)/mean(STD_RotSpeed_FB)-1)*100)   

%% Plot REWS spectra
figure('Name','REWS spectra')
hold on; grid on; box on
plot(AnalyticalModel.f,AnalyticalModel.S_LL);
plot(AnalyticalModel.f,AnalyticalModel.S_RR);
plot(f_est ,mean(S_LL_est,1));
plot(f_est ,mean(S_RR_est,1));
set(gca,'Xscale','log')
set(gca,'Yscale','log')
xlabel('frequency [Hz] ')
ylabel('Spectra REWS [(m/s)^2Hz^{-1}]')
legend('Lidar Analytical','Rotor Analytical','Lidar Estimated','Rotor Estimated')

%% Plot filter delay
[c_max,idx_max] = max(mean(c_filter,1));
T_filter        = lags(idx_max)/Fs; % [s]       time delay by the filter
figure('Name','Filter delay')
hold on;grid on; box on
plot(lags/Fs,mean(c_filter,1))
plot(T_filter,c_max,'o')
xlim([-1 1]*20)

%% Plot REWS coherence
figure('Name','REWS coherence')
hold on; grid on; box on
p1 = plot(AnalyticalModel.f,AnalyticalModel.gamma2_RL);
p2 = plot(f_est,gamma2_RL_mean_est);
set(gca,'Xscale','log')
xlabel('frequency [Hz] ')
ylabel('Coherence REWS [-]')
legend([p1 p2],'Analytical','Estimated')

%% get parameters for FFP_v1_MolasNL200.in
G_RL                        = abs(AnalyticalModel.S_RL)./AnalyticalModel.S_LL;                  % [-]       transfer function
f_cutoff                    = interp1(abs(G_RL),AnalyticalModel.f,db2mag(-3),'linear')*2*pi;    % [rad/s]   desired cutoff angular frequency
URef                        = 18;                               % [m/s]     mean wind speed
x_L                         = 160;                              % [m]       distance of lidar measurement 
T_Taylor                    = x_L/URef;                         % [s]       travel time from lidar measurment to rotor
T_scan                      = 1;                                % [s]       time of full lidar scan
tau                         = 2;                                % [s]       time to overcome pitch actuator, from Example 1: tau = T_Taylor - T_buffer, since there T_filter = T_scan = 0
T_buffer                    = T_Taylor-1/2*T_scan-T_filter-tau; % [s]       time needed to buffer signal such that FF signal is applied with tau, see Schlipf2015, Equation (5.40)