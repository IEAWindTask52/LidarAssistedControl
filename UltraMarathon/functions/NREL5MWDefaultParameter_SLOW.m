% -----------------------------
% Function: Provides parameter for NREL5MW SLOW wind turbine model.
% ------------
% Input:
% none
% ------------
% Output:
% - Parameter   struct of Parameters
% ----------------------------------
function [Parameter] = NREL5MWDefaultParameter_SLOW

%% General          
Parameter.General.rho                   = 1.225;         	% [kg/m^3]  air density

%% Turbine
Parameter.Turbine.r_GB             	    = 97;               % [-]       gearbox ratio
Parameter.Turbine.R              	    = 126/2;            % [m]       Rotor radius
Parameter.Turbine.SS             	    = load('PowerAndThrustCoefficientsNREL5MW','c_P','c_T','theta','lambda');

% drive-train dynamics
Parameter.Turbine.J                	    = 4.3784e+07;       % [kgm^2] sum of moments of inertia about low-speed shaft

% fore-aft tower dynamics      
Parameter.Turbine.x_0T           	    = -0.0656145;       % [m]       tower top deflection without wind
Parameter.Turbine.k_eT            	    = 1.80987e+06;      % [kg/s^2]  tower equivalent bending stiffness 
Parameter.Turbine.m_eT          	    = 4.3671e+05;  	    % [kg]      tower equivalent modal mass
Parameter.Turbine.c_eT            	    = 1.7781e+04;       % [kg/s]	tower equivalent structual damping
Parameter.Turbine.HubHeight       	    = 90;               % [m]       hub height

%% Pitch Actuator            
Parameter.PitchActuator.omega           = 2*pi*1;           % [rad/s]   undamped natural frequency
Parameter.PitchActuator.xi              = 0.7;              % [-]       damping factor
Parameter.PitchActuator.theta_dot_max 	= deg2rad(8);       % [rad/s]   pitch rate limit

%% Generator
Parameter.Generator.eta_el      	    = 0.944;            % [-]       Generator efficency

end