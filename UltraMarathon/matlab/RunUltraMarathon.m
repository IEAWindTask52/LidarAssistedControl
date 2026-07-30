% Runs the "Ultra Marathon" for the LAC Summer Games 2026:
% Purpose:
% We want to learn how Lidar Assisted Control (LAC) can be applied to real 
% world data. The data has been collected on April 5, 2026 on a turbine
% with 126 m rotor diameter and a 4-beam-pulsed lidar system.
% Details see instructions: <TODO: DOI url>.
% Initial result: 
% Smallest Detectable Eddy Size is estimated as 1.737 D. 
% Cost for Summer Games 2026 is 94.32 %.

%% add paths and load data

clearvars;
close all;
clc;
addpath 'functions' '../data'
load('DataSummerGames2026.mat','time','lineOfSightWindSpeed','isValid','beamID','v_0')

%% simulation parameter (should not be changed)

Parameter   = NREL5MWDefaultParameter_SLOW;             % turbine parameters
x_0         = [rpm2radPs(12.1),0.2,0,deg2rad(13),0];    % initial values for states x = [rotor speed, tower top displacement, tower top speed, pitch angle, pitch rate]
y_0         = [rpm2radPs(12.1)*97,deg2rad(13),0,5e6];   % initial values for measurements y = [generator speed, pitch angle, tower top acceleration, electrical power]
dt          = time(2)-time(1);                          % [s]           time step size
n_t         = length(time);                             % [-]           number of time steps
m           = 4;                                        % [-]           Woehler Exponent for steel
N_REF       = 2e6/(20*8760)*12;                         % [-]           fraction of 2e6 in 20 years for 12 h

%% other parameters (please add here your parameters): as example, the simple LDP from the Summer Games 2025 and a very simple feedforward controller is used

Parameter               = NREL5MWDefaultParameter_FBNREL(Parameter);  % controller parameters
LDP.NumberOfBeams       = 4;                            % [-]           Number of beams measuring at different directions               
LDP.AngleToCenterline   = 19.176;                       % [deg]         Angle around centerline
LDP.FlagLPF             = 1;                            % [0/1]         Enable low-pass filter (flag)
LDP.omega_cutoff        = 0.13;                         % [rad/s]       Corner frequency (-3dB) of the low-pass filter
LDP.T_buffer            = 0.2;                          % [s]           Buffer time for filtered REWS signal
IndexGate               = 1;                            % [-]           IndexGate
GradientStaticPitch     = deg2rad(1);                   % [rad/(m/s)]   Gradient in static pitch curve for feedforward controller
clear  LDP_v3

%% simulation feedback only (should not be changed)

% allocation and initialization
x_FB        = NaN(n_t,5);
y_FB        = NaN(n_t,4);
u_FB        = NaN(n_t,2);
x_FB(1,:)   = x_0; % init states
y_FB(1,:)   = y_0; % init measurements
clear FBController % clear persistent variables

% loop over time
for i_t=1:n_t-1
    
    % calculate feedback controller
    y_ThisStep              = y_FB(i_t,:);
    u_ThisStep              = FBController(y_ThisStep,0,dt,Parameter);

    % simulate wind turbine
    x_ThisStep              = x_FB(i_t,:);
    d_ThisStep              = v_0(i_t);
    [x_NextStep,y_NextStep] = SLOW(x_ThisStep,u_ThisStep,d_ThisStep,dt,Parameter);

    % store simulation results
    u_FB(i_t,:)             = u_ThisStep;
    x_FB(i_t+1,:)           = x_NextStep;
    y_FB(i_t+1,:)           = y_NextStep;

end

% calculate overspeed, energy, loads, power quality and pitch travel
MaxSpeed_FB     = max(y_FB(:,1));
Energy_FB       = sum(y_FB(:,4))*dt;
PowerStd_FB     = std(y_FB(:,4));
M_yT_FB         = Parameter.Turbine.HubHeight*(Parameter.Turbine.c_eT*x_FB(:,3)+Parameter.Turbine.k_eT*x_FB(:,2));
c               = rainflow(M_yT_FB);
TowerDEL_FB     = (sum(c(:,2).^m.*c(:,1))/N_REF).^(1/m);
PitchTravel_FB  = sum(abs(diff(x_FB(:,4))));

%% simulation lidar-assisted: please only adjust code between >>> <<<! 

% allocation and initialization
x_LA        = NaN(n_t,5);
y_LA        = NaN(n_t,4);
u_LA        = NaN(n_t,2);
v_0L        = NaN(n_t,1);
x_LA(1,:)   = x_0; % init states
y_LA(1,:)   = y_0; % init measurements
clear FBController % clear persistent variables

% loop over time
for i_t=1:n_t-1

    % >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    % First discipline (Preview Quality): Provide v_0L(i_t)  only based on current (i_t) or past lidar signals: isValid, beamID, lineOfSightWindSpeed
    % Second discipline (Load Reduction): Provide u_ThisStep only based on current (i_t) or past lidar signals and turbine signals from y_ThisStep

    % simple lidar data processing
    v_0L(i_t)               = LDP_v3(isValid(i_t,IndexGate),beamID(i_t),lineOfSightWindSpeed(i_t,IndexGate),dt,LDP);

    % calculate combined feedback-feedforward controller
    WindAcceleration        = (v_0L(i_t)-v_0L(max(i_t-1,1)))/dt;
    u_FF_ThisStep           = WindAcceleration*GradientStaticPitch; % simple collective pitch feedforward controller
    y_ThisStep              = y_LA(i_t,:);
    u_ThisStep              = FBController(y_ThisStep,u_FF_ThisStep,dt,Parameter);
    % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    % simulate wind turbine
    x_ThisStep              = x_LA(i_t,:);
    d_ThisStep              = v_0(i_t);    
    [x_NextStep,y_NextStep] = SLOW(x_ThisStep,u_ThisStep,d_ThisStep,dt,Parameter);

    % store simulation results
    u_LA(i_t,:)             = u_ThisStep;
    x_LA(i_t+1,:)           = x_NextStep;
    y_LA(i_t+1,:)           = y_NextStep;    

end

% calculate overspeed, energy, loads, power quality and pitch travel
MaxSpeed_LA     = max(y_LA(:,1));
Energy_LA       = sum(y_LA(:,4))*dt;
PowerStd_LA     = std(y_LA(:,4));
M_yT_LA         = Parameter.Turbine.HubHeight*(Parameter.Turbine.c_eT*x_LA(:,3)+Parameter.Turbine.k_eT*x_LA(:,2));
c               = rainflow(M_yT_LA);
TowerDEL_LA     = (sum(c(:,2).^m.*c(:,1))/N_REF).^(1/m);
PitchTravel_LA  = sum(abs(diff(x_LA(:,4))));

%% plot simulation results

figure

subplot(511)
hold on; grid on; box on
plot(time,v_0)
plot(time,v_0L)
ylabel('[m/s]');
legend('rotor','lidar','Location','best')

subplot(512)
hold on; grid on; box on
plot(time,rad2deg(u_FB(:,1)))
plot(time,rad2deg(u_LA(:,1)))
ylabel({'pitch angle'; '[deg]'});
legend('FB','LA','Location','best')

subplot(513)
hold on; grid on; box on
plot(time,u_FB(:,2)*1e3)
plot(time,u_LA(:,2)*1e3)
ylabel({'generator torque';'[kNm]'});

subplot(514)
hold on; grid on; box on
plot(time,radPs2rpm(x_FB(:,1)))
plot(time,radPs2rpm(x_LA(:,1)))
ylabel({'rotor speed';'[rpm]'});

subplot(515)
hold on; grid on; box on
plot(time,M_yT_FB/1e6)
plot(time,M_yT_LA/1e6)
ylabel({'tower base bending moment';'[MNm]'});
xlabel('time [s]')

linkaxes(findobj(gcf, 'Type', 'Axes'),'x');

%% estimate coherence and SDES (should not be changed) 

n_FFT           	    = 2^11; 
[gamma_Sq_est,f_est]    = mscohere(v_0(1:n_t-1)-mean(v_0(1:n_t-1)),v_0L(1:n_t-1)-mean(v_0L(1:n_t-1)),hamming(n_FFT),[],n_FFT,1/dt);
k_est                   = 2*pi*f_est./mean(v_0);

figure
hold on;grid on;box on
plot([1e-3 1],[1 1]*0.5)
plot(k_est,gamma_Sq_est,'-')
set(gca,'xScale','log')
xlabel('wave number [rad/m]')
ylabel('Coherence [-]')
xlim([1e-3 1])
ylim([0 1])

Idx         = (1:find(diff(gamma_Sq_est)>0,1,'first'));     % find monotonic descending values
if min(gamma_Sq_est(Idx)) <= 0.5 && max(gamma_Sq_est(Idx)) > 0.5
    MCB     = interp1(gamma_Sq_est(Idx),k_est(Idx),0.5);    % measurement coherence bandwidth
    SDES    = 2*pi/MCB/126;    
    fprintf('Smallest Detectable Eddy Size is estimated as %#.4g D.\n',SDES)
else
    fprintf('Smallest Detectable Eddy Size cannot be estimated since the monotonic descending values in the coherence do not cross 0.5.\n')
end

%% evaluate simulation results (should not be changed)

Cost            = TowerDEL_LA/TowerDEL_FB;
EnergyOK        = Energy_FB - Energy_LA <= 1 * 60*60*1e3;   % energy loss over 12h up to 1kWh acceptable
MaxSpeedOK      = MaxSpeed_LA <= MaxSpeed_FB;               % same or less overspeed
PowerStdOK      = PowerStd_LA <= PowerStd_FB;               % same or better power quality
PitchTravelOK   = PitchTravel_LA <= PitchTravel_FB;         % same or less pitch actuator duty

if EnergyOK && MaxSpeedOK && PowerStdOK && PitchTravelOK
    fprintf('Constraints OK. Cost for Summer Games 2026 is %#.4g %%.\n',Cost*100)
elseif ~EnergyOK 
    fprintf('Energy too low. Cost for Summer Games 2026 is %#.4g %%.\n',Cost*100)
elseif ~MaxSpeedOK 
    fprintf('Maximum rotor speed too high. Cost for Summer Games 2026 is %#.4g %%.\n',Cost*100)    
elseif ~PowerStdOK
    fprintf('Power standard deviation too high. Cost for Summer Games 2026 is %#.4g %%.\n',Cost*100)
elseif ~PitchTravelOK
    fprintf('Pitch travel too high. Cost for Summer Games 2026 is %#.4g %%.\n',Cost*100)
end