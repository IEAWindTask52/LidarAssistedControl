% IEA15MW_01: IEA 15 MW monopile + perfect wind preview from a single point 
% lidar system.
% Origin and changes in files: see ChangeLog.txt.
% Purpose:
% Here, we use a perfect wind preview to demonstrate that the collective
% pitch feedforward controller (designed with SLOW) is able to reduce
% significantly the rotor speed variation when OpenFAST is disturbed by an
% Extreme Operating Gust. Here, only the rotor motion and tower motion 
% (GenDOF and TwFADOF1) are enabled.  
% Result:       
% Cost for Summer Games 2024 ("30 s sprint"):  0.849094
% *** Simulink version ***
% Slightly different from RunExample: 0.860783  

%% Setup
clearvars;close all;clc;
clear FAST_SFunc 
addpath(genpath('..\WetiMatlabFunctions'))
addpath(genpath('..\NrelMatlabFunctions'))

SimulationName      = 'IEA-15-240-RWT-Monopile_Simulink';
FAST_InputFileName  = [SimulationName,'.fst'];
TMax                = 30; % [s]

% Copy the adequate OpenFAST version to the example folder
FASTsFuncFile       = 'FAST_SFunc.mexw64';
FASTdllFile         = 'OpenFAST-Simulink_x64.dll';
copyfile(['..\OpenFAST\',FASTsFuncFile],FASTsFuncFile)
copyfile(['..\OpenFAST\',FASTdllFile],  FASTdllFile)

% get Rosco Parameters
fast.FAST_InputFile = FAST_InputFileName;
fast.FAST_directory = cd;
P                   = ReadWrite_FAST(fast);
simu.dt             = P.FP.Val{contains(P.FP.Label,'DT')};
[R,F]               = load_ROSCO_params(P,simu);

% add FF Parameter from FFP_v1.IN
R.T_buffer          = 3.0; % Buffer time for filtered REWS signal [s]
R.StaticWind        = [0   10.0000   11.0000   12.0000   13.0000   14.0000   15.0000   16.0000   17.0000   18.0000   19.0000   20.0000   21.0000   22.0000   23.0000   24.0000   25.0000   26.0000   27.0000   28.0000   29.0000   30.0000]; % Wind speed  values in static pitch curve [m/s]
R.StaticPitch       = [0         0    0.0552    0.1085    0.1451    0.1749    0.2011    0.2250    0.2473    0.2682    0.2882    0.3072    0.3255    0.3432    0.3603    0.3769    0.3930    0.4087    0.4240    0.4389    0.4535    0.4679]; % Pitch angle values in static pitch curve [rad]

%% Run FB
R.FlagLAC           = 0; % Disable LAC
sim('OpenFAST_ROSCO_FFP.slx',[0,TMax]);
movefile([SimulationName,'.SFunc.outb'],[SimulationName,'_FB.outb'])      % store results

%% Run FBFF
R.FlagLAC           = 1; % Enable LAC
sim('OpenFAST_ROSCO_FFP.slx',[0,TMax]);
movefile([SimulationName,'.SFunc.outb'],[SimulationName,'_FBFF.outb'])    % store results

%% Clean up
delete(FASTsFuncFile)
delete(FASTdllFile)

%% Comparison
% read in data
FB              = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
FBFF            = ReadFASTbinaryIntoStruct([SimulationName,'_FBFF.outb']);

% Plot 
figure('Name','Simulation results')

subplot(4,1,1);
hold on; grid on; box on
plot(FB.Time,       FB.Wind1VelX);
plot(FBFF.Time,     FBFF.VLOS01LI);
legend('Hub height wind speed','Vlos')
ylabel('[m/s]');
legend('Wind1VelX','VLOS01LI')

subplot(4,1,2);
hold on; grid on; box on
plot(FB.Time,       FB.BldPitch1);
plot(FBFF.Time,     FBFF.BldPitch1);
ylabel({'BldPitch1'; '[deg]'});
legend('feedback only','feedback-feedforward')

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
xlim([0 30])

% display results
RotSpeed_0  = 7.56;     % [rpm]
TwrBsMyt_0  = 158.3e3;  % [kNm]
t_Start     = 0;        % [s]

Cost = (max(abs(FBFF.RotSpeed(FBFF.Time>=t_Start)-RotSpeed_0))) / RotSpeed_0 ...
     + (max(abs(FBFF.TwrBsMyt(FBFF.Time>=t_Start)-TwrBsMyt_0))) / TwrBsMyt_0;

fprintf('Cost for Summer Games 2024 ("30 s sprint"):  %f \n',Cost);