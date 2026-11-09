function Parameter = DefaultParameterMM92_WindSpeedEstimator

%% Generator
Parameter.Generator.AuxLossTable.x  = [0 60 70 100];    % [%]       % AuxLossTable      -> dlc_NWP__17__0.inf
Parameter.Generator.AuxLossTable.y  = [0.5 0.8 1.5 1.75]; % [%]     % AuxLossTable      -> dlc_NWP__17__0.inf
Parameter.Generator.Pref            = 2060;             % [kW]      % Pref              -> dlc_NWP__17__0.inf
Parameter.Generator.ELoss           = [2.0 3.0 4.8];    % [%]       % ELoss             -> dlc_NWP__17__0.inf
Parameter.Generator.Mloss           = [0.6 1.9 3.2];    % [%]       % Mloss             -> MM92_50Hz_rev01.nac
Parameter.Generator.Nref            = 1800;             % [rpm]     % Nref              -> dlc_NWP__17__0.inf      

%% General          
Parameter.General.rho               = 1.225;         	% [kg/m^3]  air density, AirDens from MM92-NH100-LM45p3P-EVO_AeroDyn.dat

%% Turbine
load('PowerAndThrustCoefficientsMM92_FAST','c_P','theta','lambda'); % from GetPowerAndThrustCoefficientsFAST.m
Parameter.Turbine.SS.lambda         = lambda;   
Parameter.Turbine.SS.theta          = theta;
Parameter.Turbine.SS.c_M            = c_P./repmat(lambda',1,length(theta));

Parameter.Turbine.r_GB            	= 120;              % [-]       gearbox ratio, GBRatio from MM92-NH100-LM45p3P-EVO_ElastoDyn.dat
Parameter.Turbine.R              	= 46.357;           % [m]       rotor radius, TipRad from MM92-NH100-LM45p3P-EVO_ElastoDyn.dat

% drive-train dynamics
J_G                               	= 111;              % [kgm^2]	generator inertia about high-speed shaft, GenIner from MM92-NH100-LM45p3P-EVO_ElastoDyn.dat
J_R                                	= 8786255;          % [kgm^2]	rotor inertia about high-speed shaft, from TwrFADmp_0.ED.sum 
Parameter.Turbine.J                	= J_R+J_G*Parameter.Turbine.r_GB^2;

%% Estimator
Parameter.Estimator.u_min           =  2;               % [m/s]     minimum wind speed, should be chosen to stay within the cP limits
Parameter.Estimator.u_max           = 35;               % [m/s]     maximum wind speed, should be chosen to stay within the cP limits
b12                                 = 0.03;             % [(rad/s^2)/(m/s)] impact wind speed change on change of rotor acceleration (partial derivative)
D_d                                 = 0.7;              % [-]       desired damping
omega_d                             = 1.00;             % [rad/s]   desired angular frequency
Parameter.Estimator.kp              = 2*D_d*omega_d/b12;% [(m/s)/(rad/s)]   proportional gain
Parameter.Estimator.Ti              = 2*D_d/omega_d;    % [s]               integration time
Parameter.Estimator.Omega_min       =  3/30*pi;         % [rad/s]    20% of rated 
Parameter.Estimator.Omega_max       = 18/30*pi;         % [rad/s]   120% of rated

end 