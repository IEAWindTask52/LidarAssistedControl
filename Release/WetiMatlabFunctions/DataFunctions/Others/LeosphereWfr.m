function [HWS_hub,Direction_hub,Shear,Veer,H_plus,H_minus] = LeosphereWfr(RWS_0,RWS_1,RWS_2,RWS_3,tau,iDistance,Parameter)
% difference to manual:
% - Direction_plus/minus: atan
% - H_plus/minus: cos(tau)
% DS on 05-Dec-2023
% (c) sowento GmbH

% local variables
theta   = Parameter.theta;          % [rad]     angle to center line, "zenith", same for all beams
phi_0   = Parameter.phi_0;          % [rad]     angle around center line beam 0, "azimuth 0"
phi_2   = Parameter.phi_2;          % [rad]     angle around center line beam 2, "azimuth 2"
LOS     = Parameter.x_L(iDistance); % [m]       measurement distance in lidar coordinate system
H_hub   = Parameter.H_hub;          % [m]       hub height
H_Lidar = Parameter.H_Lidar;        % [m]       height of lidar over hub

% Longitudinal and Transversal component of the wind speed for upper and lower beams
U_plus          = (RWS_0 + RWS_1) ./ (2 * (cos(theta) * cos(tau) - sin(theta) * sin(phi_0) * sin(tau)));
V_plus          = (RWS_0 - RWS_1) ./ (2 *  sin(theta) * cos(phi_0));
U_minus         = (RWS_2 + RWS_3) ./ (2 * (cos(theta) * cos(tau) - sin(theta) * sin(phi_2) * sin(tau)));
V_minus         = (RWS_2 - RWS_3) ./ (2 *  sin(theta) * cos(phi_2));

% Horizontal wind speed and direction for upper and lower beams
HWS_plus        = sqrt(U_plus.^2  + V_plus.^2);
HWS_minus       = sqrt(U_minus.^2 + V_minus.^2);
Direction_plus  = atan2(V_plus, U_plus);
Direction_minus = atan2(V_minus,U_minus);

% measurement height calculation
H_plus          = cos(tau)*(H_hub + H_Lidar) + LOS * (cos(theta) * sin(tau) + sin(theta) * sin(phi_0) * cos(tau));
H_minus         = cos(tau)*(H_hub + H_Lidar) + LOS * (cos(theta) * sin(tau) + sin(theta) * sin(phi_2) * cos(tau));

% Vertical wind shear and veer
Shear           = log(HWS_plus ./ HWS_minus) ./ log(H_plus ./ H_minus);
Veer            = (Direction_plus - Direction_minus) ./ (H_plus - H_minus); 

% Horizontal wind speed and direction at hub height
HWS_hub         = HWS_plus .* (H_hub ./ H_plus).^Shear;
Direction_hub   = Direction_plus + Veer .* (H_hub - H_plus);

end
