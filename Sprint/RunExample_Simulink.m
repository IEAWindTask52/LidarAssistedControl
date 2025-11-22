% Sprint: DLC 1.4 for IEA 15 MW monopile with and without LAC. 
% Purpose:
% We want to learn how to simulate a DLC 1.4 with an "Extreme coherent gust 
% with direction change (ECD)" with lidar-assisted control (LAC) and how
% LAC can reduce the ultimate tower loads. 
% Here, only the rotor motion and tower motion (GenDOF, TwFADOF1, TwSSDOF1) 
% are enabled for simplicity.
% Result (slightly different to pure matlab version RunExample.m):       
% Cost for Summer Games 2025 ("30 s sprint"):  0.722838 (4BeamPulsed)
% Cost for Summer Games 2025 ("30 s sprint"):  1.217274 (CircularCW)

%% Setup
clearvars;close all;clc;
addpath(genpath('..\WetiMatlabFunctions'))
addpath(genpath('..\NrelMatlabFunctions'))

% select simulated lidar
LidarType       = '4BeamPulsed'; % [4BeamPulsed/CircularCW]

% simulation time
TMax                = 50; % [s]

switch LidarType
    case '4BeamPulsed'
        % configuration from LDP_v1_4BeamPulsed.IN and FFP_v1_4BeamPulsed.IN
        LDP.NumberOfBeams       = 4;            % [-]       Number of beams measuring at different directions               
        LDP.AngleToCenterline   = 19.176;       % [deg]     Angle around centerline
        LDP.IndexGate           = 6;            % [-]       IndexGate
        LDP.FlagLPF             = 0;            % [0/1]     Enable low-pass filter (flag)
        LDP.omega_cutoff        = 0.1232;       % [rad/s]   Corner frequency (-3dB) of the low-pass filter
        LDP.T_buffer            = 5.5;          % [s]       Buffer time for filtered REWS signal
    case 'CircularCW'
        % configuration from LDP_v1_CircularCW.IN and FFP_v1_CircularCW.IN
        LDP.NumberOfBeams       = 50;           % [-]       Number of beams measuring at different directions               
        LDP.AngleToCenterline   = 15;           % [deg]     Angle around centerline
        LDP.IndexGate           = 1;            % [-]       IndexGate
        LDP.FlagLPF             = 0;            % [0/1]     Enable low-pass filter (flag)
        LDP.omega_cutoff        = 0.3268;       % [rad/s]   Corner frequency (-3dB) of the low-pass filter
        LDP.T_buffer            = 7.5;          % [s]       Buffer time for filtered REWS signal        
end

% define FAST input file
SimulationName      = ['IEA-15-240-RWT-Monopile_Simulink_',LidarType];

% get Rosco Parameters
FAST_InputFileName  = [SimulationName,'.fst'];
fast.FAST_InputFile = FAST_InputFileName;
fast.FAST_directory = cd;
P                   = ReadWrite_FAST(fast);
simu.dt             = P.FP.Val{contains(P.FP.Label,'DT')};
[R,F]               = load_ROSCO_params(P,simu);

% add FF Parameter from FFP_v1.IN
R.StaticWind        = [0   10.0000   11.0000   12.0000   13.0000   14.0000   15.0000   16.0000   17.0000   18.0000   19.0000   20.0000   21.0000   22.0000   23.0000   24.0000   25.0000   26.0000   27.0000   28.0000   29.0000   30.0000]; % Wind speed  values in static pitch curve [m/s]
R.StaticPitch       = [0         0    0.0552    0.1085    0.1451    0.1749    0.2011    0.2250    0.2473    0.2682    0.2882    0.3072    0.3255    0.3432    0.3603    0.3769    0.3930    0.4087    0.4240    0.4389    0.4535    0.4679]; % Pitch angle values in static pitch curve [rad]

%% Run FB
clear FAST_SFunc 
clear OpenFAST_ROSCO_LDP_FFP
R.FlagLAC           = 0; % Disable LAC
SimOutFB            = sim('OpenFAST_ROSCO_LDP_FFP.slx',[0,TMax]);
movefile([SimulationName,'.SFunc.outb'],[SimulationName,'_FB.outb'])      % store results

%% Run FBFF
clear FAST_SFunc 
clear OpenFAST_ROSCO_LDP_FFP
R.FlagLAC           = 1; % Enable LAC
SimOutFBFF          = sim('OpenFAST_ROSCO_LDP_FFP.slx',[0,TMax]);
movefile([SimulationName,'.SFunc.outb'],[SimulationName,'_FBFF.outb'])    % store results

%% Comparison
% read in data
FB              = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF            = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF.outb']);

% Plot 
figure('Name','Simulation results')

subplot(4,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(SimOutFBFF.logsout.get('REWS_b').Values);
ylabel('[m/s]');
legend('Wind1VelX','REWS_b','Interpreter','none','Location','best')

subplot(4,1,2);
hold on; grid on; box on
plot(FB.Time,       FB.BldPitch1);
plot(FBFF.Time,     FBFF.BldPitch1);
ylabel({'BldPitch1'; '[deg]'});
legend('feedback only','feedback-feedforward','Location','best')

subplot(4,1,3);
hold on; grid on; box on
plot(FB.Time,       FB.RotSpeed);
plot(FBFF.Time,     FBFF.RotSpeed);
ylabel({'RotSpeed';'[rpm]'});

subplot(4,1,4);
hold on; grid on; box on
plot(FB.Time,       FB.TwrBsMyt/1e3);
plot(FBFF.Time,     FBFF.TwrBsMyt/1e3);
ylabel({'TwrBsMyt';'[MNm]'});

xlabel('time [s]')
linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
xlim([20 50])

% display results
RotSpeed_0  = 7.56;     % [rpm]
TwrBsMyt_0  = 158.3e3;  % [kNm]
t_Start     = 0;        % [s]

Cost = (max(abs(FBFF.RotSpeed(FBFF.Time>=t_Start)-RotSpeed_0))) / RotSpeed_0 ...
     + (max(abs(FBFF.TwrBsMyt(FBFF.Time>=t_Start)-TwrBsMyt_0))) / TwrBsMyt_0;

fprintf('Cost for Summer Games 2024 ("30 s sprint"):  %f \n',Cost);