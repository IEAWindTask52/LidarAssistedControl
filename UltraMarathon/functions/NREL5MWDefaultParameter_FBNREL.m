% -----------------------------
% Function: Provides default parameters for feedback controller.
% ------------
% Input:
% - Parameter   struct of Parameters
% ------------
% Output:
% - Parameter   struct of Parameters
% ----------------------------------
function [Parameter] = NREL5MWDefaultParameter_FBNREL(Parameter)

% collective pitch controller
Parameter.CPC.theta_K           = deg2rad(6.302336);
Parameter.CPC.kp                = 0.01882681;                       % [rad/(rad/s)]
Parameter.CPC.Ti                = 0.01882681/0.008068634;           % [s]
Parameter.CPC.Omega_g_rated     = rpm2radPs(12.1*97);               % [rad/s]
Parameter.CPC.theta_max         = deg2rad(90);                      % [rad]
Parameter.CPC.theta_min         = deg2rad(0);                       % [rad]

% variable speed controller
Parameter.VSC.M_g_rated         = 43093.55;                         % [Nm] 

% filter generator speed
Parameter.Filter.f_cutoff       = 0.25;                             % [Hz]

end


