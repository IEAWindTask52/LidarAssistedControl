function [t,u] = CalculateEOG_R2015b(T,dt,t_start,V_hub,D,options)

% calculation of wind signals
t           = 0:dt:T-dt;

% Longitudinal scale parameter at hub height (assuming hub height >60 m)
lambda1     = 42;

% Extreme Operational Gust (EOG) definition
sigma1      = options.I_ref * (0.75*V_hub + 5.6);
V_e50     	= 1.4*options.V_ref;
V_e1      	= 0.8*V_e50;
V_gust      = min([1.35*(V_e1-V_hub), 3.3*(sigma1/(1+0.1*D/lambda1))]);

% EOG wind speed vector
u           = zeros(1, length(t));
for iTimeStep = 1:length(t)
    if (t(iTimeStep) >= t_start) && (t(iTimeStep) <= t_start+options.T_gust)
        t_EOG           = t(iTimeStep) - t_start;
        u(iTimeStep)    = V_hub-0.37*V_gust*sin(3*pi*t_EOG/options.T_gust)*(1-cos(2*pi*t_EOG/options.T_gust));
    else
        u(iTimeStep)    = V_hub;
    end
end

% adjust amplitude if requested
if ~isnan(options.A)
    A_org       = max(u)-min(u);
    u           = (u-V_hub)/A_org*options.A+V_hub;
end

end