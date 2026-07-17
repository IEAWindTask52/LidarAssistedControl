% -----------------------------
% Function:         Provides outputs of a very basic feedback controller.
% ------------
% Input:
% - y_ThisStep      measurements from current time step
% - u_FF_ThisStep   feedforward inputs from current time step
% - dt              time step size
% - Parameter       struct of Parameters
% ------------
% Output:
% - u_ThisStep      controller outputs from this time step
% ----------------------------------
function  [u_ThisStep]   = FBController(y_ThisStep,u_FF_ThisStep,dt,Parameter)

% Local variables
theta_K         = Parameter.CPC.theta_K;
kp              = Parameter.CPC.kp;
Ti              = Parameter.CPC.Ti;
Omega_g_rated   = Parameter.CPC.Omega_g_rated;
theta_min       = Parameter.CPC.theta_min;
theta_max       = Parameter.CPC.theta_max;
M_g_rated       = Parameter.VSC.M_g_rated; 
f_cutoff        = Parameter.Filter.f_cutoff;

% measurements
Omega_g         = y_ThisStep(1);            % generator speed
theta           = y_ThisStep(2);            % blade pitch angle

% feedforward inputs
theta_dot_FF    = u_FF_ThisStep(1);

% persistent variables
persistent Omega_g_f integrator                  
if isempty(Omega_g_f)      
    Omega_g_f   = Omega_g;  
end 
if isempty(integrator)      
    integrator  = theta;
end

%% Low pass filter 
alpha           = exp(-2*pi*dt*f_cutoff);                       % low-pass filter coefficient, equation (7-2)
Omega_g_f       = (1-alpha)*Omega_g+alpha*Omega_g_f;            % filtered generator speed, equation (7-1)

%% pitch controller
e               = Omega_g_f-Omega_g_rated;                      % speed error
g               = 1/(1+theta/theta_K);                          % gain-correction factor, equation (7-20) 
integrator_dot  = g*kp*e/Ti + theta_dot_FF;                     % integrator input
integrator      = integrator + dt * integrator_dot;             % integrator output
integrator      = max(integrator,0);                            % anti-windup
theta_c_unc     = g*kp*e + integrator;                          % unconstrainted commanded pitch
theta_c         = max(min(theta_c_unc,theta_max),theta_min);    % constrainted commanded pitch

%% torque controller
M_g_c           = M_g_rated*Omega_g_rated/Omega_g_f;            % for constant power   

%% final controller outputs
u_ThisStep      = [theta_c,M_g_c];

end