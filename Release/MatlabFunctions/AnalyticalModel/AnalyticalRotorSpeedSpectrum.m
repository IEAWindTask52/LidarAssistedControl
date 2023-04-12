function AnalyticalModel = AnalyticalRotorSpeedSpectrum(v_0_OP,theta_OP,Omega_OP,f_delay,...
    ROSCOInFileName,RotorPerformanceFile,LidarInputFileName,LDPInputFileName,SpectralModelFileName)

% Inputs definition
% v_0_OP:   The mean wind speed at the operating point [m/s]
% theta_OP: The pitch angle at the operating point [rad]
% Omega_OP: The rotor speed at the operating point [rad/s]
% f_delay:  The frequency where rotor speed has a very high spectrum
%           we want to use lidar-assisted control to reduce the rotor speed
%           fluctuation at this frequency

% ROSCOInFileName:       name of the ROSCO DLL input file 
% RotorPerformanceFile:  name of the turbine rotor performance file, includes Cp Ct Cq
% LidarInputFileName??   name of the lidar input file
% LDPInputFileName??     name of the lidar data processing DLL input file
% SpectralModelFileName: name of the ".mat" file that contains the pre-calculated lidar-rotor spectra


% Load 
% 1. S_RR: Baseline Spectrum for rotor effective wind speed (avege u components in the swept area)
% 2. S_LL: Spectrum for lidar estimated rotor effective wind speed
% 3. S_RL: Cross-spectrum between rotor effective wind speed and its lidar estimation
% 4. f:    Frequency vector

load(SpectralModelFileName, 'S_LL', 'S_RL', 'S_RR', 'f'); % dimensions of all variables should be N*1

% Get some parameters from Rosco
Parameter.General.rho           = GetParametersFromText(ROSCOInFileName,'WE_RhoAir');       % [kg/m^3]  air density
[theta,lambda,c_P,c_T]          = GetPowerAndThrustCoefficients(RotorPerformanceFile);
Parameter.Turbine.SS            = struct('theta',theta,'lambda',lambda,'c_P',c_P,'c_T',c_T); % Cp Ct
Parameter.Turbine.i_GB          = 1/GetParametersFromText(ROSCOInFileName,'WE_GearboxRatio');% [-]  gearbox ratio
Parameter.Turbine.R             = GetParametersFromText(ROSCOInFileName,'WE_BladeRadius');  % [m]   Rotor radius
Parameter.Generator.eta_el      = GetParametersFromText(ROSCOInFileName,'VS_GenEff');       % [-]   Generator efficiency
Parameter.Turbine.J             = GetParametersFromText(ROSCOInFileName,'WE_Jtot');         % [kg m^2] Total drivetrain inertia, including blades, hub and casted generator inertia to LSS, 
Parameter.Generator.P_a_rated   = GetParametersFromText(ROSCOInFileName,'VS_RtPwr')/Parameter.Generator.eta_el;   % [W] Rated aerodynamic power

% Pitch control parameters from Rosco
PC_GS_angles            = GetParametersFromText(ROSCOInFileName,'PC_GS_angles');  % Gain-schedule table: pitch angles [rad].
PC_GS_KP                = GetParametersFromText(ROSCOInFileName,'PC_GS_KP'); % Gain-schedule table: pitch controller kp gains [s].
PC_GS_KI                = GetParametersFromText(ROSCOInFileName,'PC_GS_KI'); % Gain-schedule table: pitch controller ki gains [-].
kp                      = -interp1(PC_GS_angles,PC_GS_KP,theta_OP);
KI                      = -interp1(PC_GS_angles,PC_GS_KI,theta_OP);
Ti                      = kp/KI;

% wind turbine
[A,B,C,D]               = LinearizeTurbine1DOF(theta_OP,Omega_OP,v_0_OP,Parameter);
WT_1DOF                 = ss(A,B,C,D);
WT_1DOF.InputName       = {'theta','M_g','v_0'};
WT_1DOF.OutputName      = {'Omega_g','Omega_r'};   

% feedback pitch controller
FB                      = ss(0,1,kp/Ti,kp); 
FB.InputName            = {'Omega_g_f'};    
FB.OutputName           = {'theta_FB'}; 
FB.StateName            = {'IntegratorPI'};

% torque controller
TC                      = ss(-Parameter.Generator.P_a_rated/(Omega_OP/Parameter.Turbine.i_GB)^2); 
TC.InputName            = {'Omega_g_f'};    
TC.OutputName           = {'M_g'};

% Low Pass filter for generator torque control
w_cutoff                = GetParametersFromText(ROSCOInFileName,'F_LPFCornerFreq');
LP                      = ss(-w_cutoff,w_cutoff,1,0); 
LP.InputName            = {'Omega_g'};
LP.OutputName           = {'Omega_g_f'};
LP.StateName            = {'Omega_g_f_dot'};

% pitch actuator
omega                   = GetParametersFromText(ROSCOInFileName,'PA_CornerFreq');
xi                      = GetParametersFromText(ROSCOInFileName,'PA_Damping');
PA                      = ss([0 1 ;-omega^2 -2*omega*xi],[0;omega^2],[1 0],0);
PA.InputName            = {'theta_c'}; 
PA.OutputName           = {'theta'}; 
PA.StateName            = {'theta';'theta_dot'};

% closed loop for FB only control
Sum_FB                  = sumblk('theta_c = theta_FB');
CL_FB                   = connect(WT_1DOF,PA,FB,LP,TC,Sum_FB,{'v_0'},{'Omega_g','Omega_r'});

% Define first Order LPF

G_RL                    = abs(S_RL)./S_LL;
% w_cutoff                = interp1(abs(G_RL),f,db2mag(-3),'linear')*2*pi;
w_cutoff                = interp1(abs(G_RL((f<0.07)&(f>0))),f((f<0.07)&(f>0)),db2mag(-3),'linear')*2*pi; % temporary solution to avoid peaks in high frequencies
LP_FF                   = ss(-w_cutoff,w_cutoff,1,0);                      
LP_FF.InputName         = {'v_0L'};
LP_FF.OutputName        = {'v_0Lf'};  
[~,phase_LP_FF,~]      	= bode(LP_FF,f*2*pi);
TimeDelay               = squeeze(-phase_LP_FF)./360./f;

% Get a time delay for the interested frequency f_delay, it is the frequency where rotor speed fluctuates the most
T_filter                = interp1(f,TimeDelay,f_delay,'linear');  
Azimuth                 = GetParametersFromText(LDPInputFileName,'Lidar_Azimuth');
Elevation               = GetParametersFromText(LDPInputFileName,'Lidar_Elevation');
RangeGate               = GetParametersFromText(LDPInputFileName,'Lidar_RangeGates');% radial direction
x_lead                  = RangeGate(1)*cosd(Azimuth(1))*cosd(Elevation(1));
T_lead                  = x_lead/v_0_OP;                                         
[~,phase_PA,~]          = bode(PA,f*2*pi);
T_pitch_over_f          = squeeze(-phase_PA)./360./f;    
T_pitch                 = interp1(f,T_pitch_over_f,f_delay,'linear');   
Num_LidarBeam           = GetParametersFromText(LDPInputFileName,'NumberOfBeams'); % number of lidar beam
t_measurement_interval  = GetParametersFromText(LidarInputFileName,'t_measurement_interval'); % time required to finish measurement at one beam
T_scan                  = 0.5*Num_LidarBeam*t_measurement_interval;  %half lidar full scan time                  % DS: should be extracted from the lidar file or adjustable
T_buffer                = T_lead-T_scan-T_pitch-T_filter;

% Define normal feedforward collective pitch controller
FF                      = tf(-B(3)/B(1));    %dtheta/dv0
FF.InputName            = {'v_0Lf'};
FF.OutputName           = {'theta_FF'};  

% Closed Loop FB + normal feedforward with a low-pass filter
Sum_FBFF                = sumblk('theta_c = theta_FB+theta_FF');
CL_FBFF                 = connect(WT_1DOF,PA,FB,FF,LP,LP_FF,TC,Sum_FBFF,{'v_0','v_0L'},{'Omega_g','Omega_r'});

% frequecy domain response of FB only 
[MAG,~]                 = bode(CL_FB,2*pi*f); 

% frequency domain response of FBFF control
% calculate analytic spectra of rotor speed with FB+FF control
G_OmegaR_FBFF          = squeeze(freqresp(CL_FBFF(2,1),2*pi*f));                            % tf from Rotor to Omega
G_OmegaL_FBFF          = squeeze(freqresp(CL_FBFF(2,2),2*pi*f)).*exp(-1i*2*pi.*f*T_buffer); % tf from Lidar to Omega
S_Omega_r_FF           =  G_OmegaR_FBFF.*conj(G_OmegaR_FBFF).*S_RR          +...            % equal to abs(G_OmegaR_FBLFF.^2).*S_RR;     
                          G_OmegaR_FBFF.*conj(G_OmegaL_FBFF).*S_RL          +...
                          G_OmegaL_FBFF.*conj(G_OmegaR_FBFF).*conj(S_RL)	+...
                          G_OmegaL_FBFF.*conj(G_OmegaL_FBFF).*S_LL;                         % equal to abs(G_OmegaL_FBLFF.^2).*S_LL;                         

% Package and send out
AnalyticalModel.S_Omega_r_FB            = squeeze(MAG(2,:,:)).^2.*S_RR;
AnalyticalModel.S_Omega_r_FF            = S_Omega_r_FF;
AnalyticalModel.f                       = f;
AnalyticalModel.f_cutoff                = w_cutoff; % Corner frequency (-3dB) of the low-pass filter [rad/s]
AnalyticalModel.T_buffer                = T_buffer; % Buffer time for filtered REWS signal [s] 
AnalyticalModel.G_RL                    = G_RL;
AnalyticalModel.S_RL                    = S_RL;
AnalyticalModel.S_RR                    = S_RR;
AnalyticalModel.S_LL                    = S_LL;
AnalyticalModel.gamma2_RL               = abs(S_RL).^2./S_RR./S_LL;
%AnalyticalModel.gamma2_RL((AnalyticalModel.f>=0.75)&(AnalyticalModel.f<2)) = AnalyticalModel.gamma2_RL((AnalyticalModel.f>=0.75)&(AnalyticalModel.f<2))/max(AnalyticalModel.gamma2_RL((AnalyticalModel.f>=0.75)&(AnalyticalModel.f<2)));

end

