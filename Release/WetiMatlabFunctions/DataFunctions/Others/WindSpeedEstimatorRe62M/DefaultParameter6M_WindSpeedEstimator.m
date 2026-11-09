function Parameter = DefaultParameter6M_WindSpeedEstimator

%% Generator
Parameter.Generator.AuxLossTable.x		= [-1 0 100 101];               % [%]       % AuxLossTable      -> dlc1_2__07__00.inf (extrapolated)
Parameter.Generator.AuxLossTable.y		= [0.98 0.98 2.71 2.71];        % [%]       % AuxLossTable      -> dlc1_2__07__00.inf (extrapolated)
Parameter.Generator.Pref                = 6200;                         % [kW]       % Pref             -> dlc1_2__07__00.inf
Parameter.Generator.ELoss               = [2.0 3.0 3.958];              % [%]       % ELoss             -> dlc1_2__07__00.inf
Parameter.Generator.Mloss               = [0.6 1.9 3.870];              % [%]       % Mloss             -> 6M_standard_150508.nac
Parameter.Generator.Nref                = 1170;                         % [rpm]     % Nref              -> dlc1_2__07__00.inf 

%% General          
Parameter.General.rho               = 1.225;         	% [kg/m^3]  air density, AirDens from MM92-NH100-LM45p3P-EVO_AeroDyn.dat

%% Turbine
load('PowerAndThrustCoefficients6M_FAST','c_P','theta','lambda'); % from GetPowerAndThrustCoefficientsFAST.m
Parameter.Turbine.SS.lambda         = lambda;   
Parameter.Turbine.SS.theta          = theta;
Parameter.Turbine.SS.c_M            = c_P./repmat(lambda',1,length(theta));

Parameter.Turbine.r_GB            	= 97;              % [-]       gearbox ratio, GBRatio from Re6p2M126_ElastoDyn.dat
Parameter.Turbine.R              	= 63.14;           % [m]       rotor radius, TipRad from Re6p2M126_ElastoDyn.dat

% drive-train dynamics
J_G                               	= 955;              % [kgm^2]	generator inertia about high-speed shaft, GenIner from Re6p2M126_ElastoDyn.dat
J_R                                	= 40178388;         % [kgm^2]	rotor inertia about high-speed shaft, from TwrFADmp_0.ED.sum 
Parameter.Turbine.J                	= J_R+J_G*Parameter.Turbine.r_GB^2;

%% Estimator
Parameter.Estimator.u_min           =  2;               % [m/s]     minimum wind speed, should be chosen to stay within the cP limits
Parameter.Estimator.u_max           = 35;               % [m/s]     maximum wind speed, should be chosen to stay within the cP limits
b12                                 = 0.02;             % [(rad/s^2)/(m/s)] impact wind speed change on change of rotor acceleration (partial derivative)
D_d                                 = 0.7;              % [-]       desired damping
omega_d                             = 0.80;             % [rad/s]   desired angular frequency
Parameter.Estimator.kp              = 2*D_d*omega_d/b12;% [(m/s)/(rad/s)]   proportional gain
Parameter.Estimator.Ti              = 2*D_d/omega_d;    % [s]               integration time
Parameter.Estimator.Omega_min       = 0.2*1170/Parameter.Turbine.r_GB*(2*pi)/60; % [rad/s] 20% of rated: 0.2*rpm2radPs(Nref/GBRatio) (Nref [rpm] from Servos_v2_6M.in and GBRatio [-] ElastoDyn)
Parameter.Estimator.Omega_max       = 1.2*1170/Parameter.Turbine.r_GB*(2*pi)/60; % [rad/s] 120% of rated: 1.2*rpm2radPs(Nref/GBRatio) (Nref [rpm] from Servos_v2_6M.in and GBRatio [-] ElastoDyn)

end 