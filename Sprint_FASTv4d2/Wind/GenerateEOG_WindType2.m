clear all;
close all;
clc;

%% Preprocessing
% time
T           = 40;                   % [s]   simulation length
dt          = 1/100;               	% [s]   simulation time step

% wind
V_hub       = 25;                   % [m/s] mean wind speed at hub height 
T_gust      = 10.5;                 % [s]   length of EOG
t_start     = 10;                   % [s]   time when EOG should start
V_ref       = 50;                   % [m/s] reference wind speed average over 10 min
I_ref       = 0.14;                 % [-]   expected value of the turbulence intensity at 15 m/s
D           = 240;                  % [m]   rotor diameter
alpha       = 0.0;                  % [-]   Vertical power-law wind-shear exponent
% calculation of wind signals
t           = 0:dt:T-dt;

% Longitudinal scale parameter at hub height (assuming hub height >60 m)
lambda1     = 42;

% Extreme Operational Gust (EOG) definition
sigma1      = I_ref * (0.75*V_hub + 5.6);
V_e50     	= 1.4*V_ref;
V_e1      	= 0.8*V_e50;
V_gust      = min([1.35*(V_e1-V_hub), 3.3*(sigma1/(1+0.1*D/lambda1))]);

% EOG wind speed vector
u           = zeros(1, length(t));
for iTimeStep = 1:length(t)
    if (t(iTimeStep) >= t_start) && (t(iTimeStep) <= t_start+T_gust)
        t_EOG           = t(iTimeStep) - t_start;
        u(iTimeStep)    = V_hub-0.37*V_gust*sin(3*pi*t_EOG/T_gust)*(1-cos(2*pi*t_EOG/T_gust));
    else
        u(iTimeStep)    = V_hub;
    end
end


figure()
plot(t,u)

%% Preprocessing: generate uniform wind field
fid = fopen(['EOG_URef_',num2str(V_hub,'%02d'),'.wnd'],'w+');
n_data = length(u);
fprintf(fid,['%f,%f,%f,%f,%f,%f,%f,%f,%f\r\n'],[t' u' zeros(n_data,3)  ones(n_data,1)*alpha zeros(n_data,3)]');
fclose(fid);
