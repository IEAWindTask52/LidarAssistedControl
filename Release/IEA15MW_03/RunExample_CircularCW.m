% IEA15MW_03: IEA 15 MW monopile + realistic wind preview  from a 
% circular-scanning continuous-wave lidar system measuring at 200 m.
% Purpose:
% Here, we use a realistic wind preview to demonstrate that the collective
% pitch feedforward controller together with the correct filtering provides
% a reduction in rotor speed variation and the coherence is as expected. 
% In this example, we assume frozen turbulence, only one 3D turbulent
% wind field (y,z,t) at rotor plane is generated.
% Result:
% Change in rotor speed standard deviation:  -64.1 %
% Cost for Summer Games 2024 ("18 m/s hurdles"):  0.352534 m/s

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))

% Seeds (can be adjusted, but will provide different results)
nSeed               = 6;                        % [-]	number of stochastic turbulence field samples
Seed_vec            = [1:nSeed]+18*100;         % [-]  	vector of seeds

% Parameters postprocessing (can be adjusted, but will provide different results)
t_start             = 60;                       % [s] 	ignore data before for STD and spectra
TMax                = 660;                      % [s]   total run time, same as in *.fst
DT                  = 0.0125;                   % [s]   time step, same as in *.fst
R                   = 120;                      % [m]  	rotor radius to calculate REWS
nBlock              = 2;                        % [-]   number of blocks for spectra 
Fs                	= 1/DT;                     % [Hz]  sampling frequency
AnalysisTime        = TMax-t_start;             % [s]   time to calculate spectra etc.
nDataPerBlock       = AnalysisTime/nBlock*Fs;   % [-]  	data per block, here 2 blocks
vWindow             = hamming(nDataPerBlock);   % [-] 	window for estimation
nFFT                = 2^nextpow2(nDataPerBlock);% [-]  	number of FFT, default: 2^nextpow2(nDataPerBlock); 
nOverlap            = nDataPerBlock/2;          % [-]  	samples of overlap, default: 50% overlap

% Parameter for Cost (Summer Games 2024)
tau                 = 2;                        % [s]   time to overcome pitch actuator, from Example 1: tau = T_Taylor - T_buffer, since there T_filter = T_scan = 0

% Files (should not be be changed)
TurbSimExeFile      = 'TurbSim_x64.exe';
FASTexeFile         = 'openfast_x64.exe';
SimulationName      = 'IEA-15-240-RWT-Monopile_CircularCW';
TurbSimTemplateFile = 'TurbSim2aInputFileTemplateIEA15MW.inp';
SimulationFolderFB  = 'SimulationResults_FeedbackOnly';
SimulationFolderLAC = 'SimulationResults_CircularCW';

if ~exist('TurbulentWind','dir')
    mkdir TurbulentWind
end
if ~exist(SimulationFolderFB,'dir')
    mkdir(SimulationFolderFB)
end
if ~exist(SimulationFolderLAC,'dir')
    mkdir(SimulationFolderLAC)
end

%% Preprocessing: generate turbulent wind field
    
% Copy the adequate TurbSim version to the example folder 
copyfile(['..\TurbSim\',TurbSimExeFile],['TurbulentWind\',TurbSimExeFile])
    
% Generate all wind fields
for iSeed = 1:nSeed        
    Seed                = Seed_vec(iSeed);
    WindFileName        = ['URef_18_Seed_',num2str(Seed,'%04d')];
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

% Simulate with all wind fields
for iSeed = 1:nSeed
    
    % Adjust the InflowWind file
    Seed                = Seed_vec(iSeed);
	WindFileName        = ['URef_18_Seed_',num2str(Seed,'%04d')];
	WindFileRoot        = ['TurbulentWind\',WindFileName];
    ManipulateTXTFile('IEA-15-240-RWT_InflowFile.dat','MyFilenameRoot',WindFileRoot);
    
    % Run FB    
    FASTresultFile      = fullfile(SimulationFolderFB,[WindFileName,'_FlagLAC_0.outb']);
    ROSCOresultFile     = fullfile(SimulationFolderFB,[WindFileName,'_FlagLAC_0.dbg']);
    if ~exist(FASTresultFile,'file')    
        ManipulateTXTFile('ROSCO_v2d6.IN','1 ! FlagLAC','0 ! FlagLAC'); % disable LAC
        dos([FASTexeFile,' ',SimulationName,'.fst']);
        movefile([SimulationName,'.outb'],FASTresultFile)
        movefile([SimulationName,'.RO.dbg'],ROSCOresultFile)   % store rosco output file
    end
   
    % Run FB+FF    
    FASTresultFile      = fullfile(SimulationFolderLAC,[WindFileName,'_FlagLAC_1.outb']);
    ROSCOresultFile     = fullfile(SimulationFolderLAC,[WindFileName,'_FlagLAC_1.dbg']);
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

%% Postprocessing: evaluate data

% Allocation
S_RotSpeed_FB_est       = NaN(nSeed,nFFT/2+1);
S_RotSpeed_FBFF_est     = NaN(nSeed,nFFT/2+1);
S_LL_est                = NaN(nSeed,nFFT/2+1);
S_RR_est                = NaN(nSeed,nFFT/2+1);
S_RL_est                = NaN(nSeed,nFFT/2+1);
STD_RotSpeed_FB         = NaN(1,nSeed);
STD_RotSpeed_FBFF       = NaN(1,nSeed);
c_filter                = NaN(nSeed,AnalysisTime*Fs*2+1);
c_RL                    = NaN(nSeed,AnalysisTime*Fs*2+1);
MAE                     = NaN(1,nSeed); % mean absolute error [m/s]

% Loop over all seeds
for iSeed = 1:nSeed    

    % Load data
    Seed                = Seed_vec(iSeed);
	WindFileName        = ['URef_18_Seed_',num2str(Seed,'%04d')];
    FASTresultFile      = fullfile(SimulationFolderFB,[WindFileName,'_FlagLAC_0.outb']);
    ROSCOresultFile     = fullfile(SimulationFolderFB,[WindFileName,'_FlagLAC_0.dbg']);
    FB                  = ReadFASTbinaryIntoStruct(FASTresultFile);
    R_FB                = ReadROSCOtextIntoStruct(ROSCOresultFile);
    FASTresultFile      = fullfile(SimulationFolderLAC,[WindFileName,'_FlagLAC_1.outb']);
    ROSCOresultFile     = fullfile(SimulationFolderLAC,[WindFileName,'_FlagLAC_1.dbg']);
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
    TurbSimResultFile                 	= ['TurbulentWind\',WindFileName,'.wnd'];   
    [REWS_WindField,Time_WindField]  	= CalculateREWSfromWindField(TurbSimResultFile,R,2);
    REWS_WindField_Fs                   = interp1(Time_WindField,REWS_WindField,R_FBFF.Time);
    [S_LL_est(iSeed,:) ,~]            	= pwelch(detrend(R_FBFF.REWS(R_FBFF.Time>=t_start),'constant'),vWindow,nOverlap,nFFT,Fs);     
    [S_RR_est(iSeed,:) ,~]            	= pwelch(detrend(REWS_WindField_Fs(R_FBFF.Time>=t_start),'constant'),vWindow,nOverlap,nFFT,Fs);     
    [S_RL_est(iSeed,:) ,~]             	= cpsd(detrend(REWS_WindField_Fs(R_FBFF.Time>=t_start),'constant'),detrend(R_FBFF.REWS(R_FBFF.Time>=t_start),'constant'),vWindow,nOverlap,nFFT,Fs);     
    
    % Estimate cross correlation between filtered and unfiltered REWS from lidar
    [c_filter(iSeed,:),lags]    = xcorr(detrend(R_FBFF.REWS_f(R_FBFF.Time>=t_start),'constant'),detrend(R_FBFF.REWS(R_FBFF.Time>=t_start),'constant'),'normalized');

    % Estimate cross correlation between rotor and lidar
    [c_RL(iSeed,:),lags]        = xcorr(detrend(REWS_WindField_Fs(R_FBFF.Time>=t_start),'constant'),detrend(R_FBFF.REWS_b(R_FBFF.Time>=t_start)),'normalized');  

    % Calculate mean absolute error
    REWS_WindField_Fs_shifted   = interp1(Time_WindField-tau,REWS_WindField,R_FBFF.Time); % shift the REWS from wind field by tau (intented prediction time) into the future (lower times)
    Error                       = REWS_WindField_Fs_shifted-R_FBFF.REWS_b; % error is  REWS from wind field shifted minus REWS from lidar filtered and buffered.
    MAE(iSeed)                  = mean(abs(Error(R_FBFF.Time>=t_start)));  % only consider error after t_start

    % Plot REWS for absolute error
    figure('Name',['REWS seed ',num2str(Seed)])
    subplot(311)
    hold on; grid on; box on
    plot(R_FBFF.Time,   REWS_WindField_Fs);
    plot(R_FBFF.Time,   R_FBFF.REWS);
    ylabel('REWS [m/s]');
    legend('wind field','lidar estimate')
    subplot(312)
    hold on; grid on; box on
    plot(R_FBFF.Time,   REWS_WindField_Fs_shifted);
    plot(R_FBFF.Time,   R_FBFF.REWS_b);
    ylabel('REWS [m/s]');
    legend('wind field shifted','lidar estimate filtered and buffered')
    subplot(313)
    hold on; grid on; box on
    plot(R_FBFF.Time,   Error);
    ylabel('error [m/s]');
    xlabel('time [s]')        

end
    
% Calculate mean coherence
gamma2_RL_mean_est          = abs(mean(S_RL_est,1)).^2./mean(S_LL_est,1)./mean(S_RR_est,1);

% Get analytical correlation model
SpectralModelFileName       = '..\AnalyticalModel\LidarRotorSpectra_IEA15MW_CircularCW.mat'; % model for 18 m/s
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
ylabel('Spectra RotSpeed [(rpm)^2/Hz]')
legend('FB-only Estimated','FBFF Estimated')

% Display results
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
ylabel('Spectra REWS [(m/s)^2/Hz]')
legend('Lidar Analytical','Rotor Analytical','Lidar Estimated','Rotor Estimated')

%% Plot filter delay
[c_max,idx_max] = max(mean(c_filter,1));
T_filter        = lags(idx_max)/Fs; % [s]       time delay by the filter
figure('Name','Filter delay')
hold on;grid on; box on
plot(lags/Fs,mean(c_filter,1))
plot(T_filter,c_max,'o')
xlim([0 10])
ylim([0.8 1])
xlabel('time [s]')
ylabel('cross correlation [-]')

%% Plot cross-correlation between rotor and lidar
[c_max,idx_max] = max(mean(c_RL,1));
T_RL            = lags(idx_max)/Fs; % [s]       time delay by the filter
figure('Name','Cross-correlation between rotor and lidar')
title('Cross-correlation between rotor and lidar')
hold on;grid on; box on
plot(lags/Fs,mean(c_RL,1))
plot(T_RL,c_max,'o')
xlim([-5 5])
ylim([0.8 1])
xlabel('time [s]')
ylabel('cross correlation [-]')

%% Plot REWS coherence
figure('Name','REWS coherence')
hold on; grid on; box on
plot(AnalyticalModel.f,AnalyticalModel.gamma2_RL);
plot(f_est,gamma2_RL_mean_est);
set(gca,'Xscale','log')
xlabel('frequency [Hz] ')
ylabel('Coherence REWS [-]')
legend('Analytical','Estimated')

%% Get parameters for FFP_v1_CircularCW.in
G_RL                        = AnalyticalModel.S_RL./AnalyticalModel.S_LL;                       % [-]       transfer function
f_cutoff                    = interp1(abs(G_RL),AnalyticalModel.f,db2mag(-3),'linear')*2*pi;    % [rad/s]   desired cutoff angular frequency
URef                        = 18;                               % [m/s]     mean wind speed
x_L                         = 200;                              % [m]       distance of lidar measurement 
T_Taylor                    = x_L/URef;                         % [s]       travel time from lidar measurment to rotor
T_scan                      = 1;                                % [s]       time of full lidar scan
T_buffer                    = T_Taylor-1/2*T_scan-T_filter-tau; % [s]       time needed to buffer signal such that FF signal is applied with tau, see Schlipf2015, Equation (5.40)

%% Calculation of Cost for Summer Games 2024
Cost                        = mean(MAE);
fprintf('Cost for Summer Games 2024 ("18 m/s hurdles"):  %f m/s \n',Cost);