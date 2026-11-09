function Statistics = ApplyLeosphereWfrMM92(Statistics,IndicesDistances,options)
% Adds the leosphere WFR to Statistics.
% Function for AddDerivedStatistics.

% inputs
arguments
    Statistics                  table
    IndicesDistances   (1,:)    double
    options.IndexOffset (1,1)   double = 0; % RevPi: one-indexed, IndexOffset = 0; IMC: zero-indexed, IndexOffset = 1.        
    options.UnitTilt            char {mustBeMember(options.UnitTilt,{'rad','deg'})} = 'rad'  % RevPi: UnitTilt = 'rad'; IMC: UnitTilt = 'deg'.  
end

% local variables
HA                  = deg2rad(15);
VA                  = deg2rad(12.5);
TC                  = acos(1/sqrt(1^2+(tan(HA))^2+(tan(VA))^2)); % cos(TA)=X/R with X=1;Y=tan(HA);Z=tan(VA)
AC                  = atan2(tan(VA),tan(HA)); % tan(AC) = Z/Y

% Parameters
Parameter.theta     =  TC;          % [rad]     angle to center line, "zenith", same for all beams
Parameter.phi_0     =  AC;          % [rad]     angle around center line beam 0, "azimuth 0"
Parameter.phi_2     = -AC;          % [rad]     angle around center line beam 2, "azimuth 2"
Parameter.x_L       = 50:14:176;    % [m]       measurement distances in lidar coordinate system
Parameter.H_hub     = 100;          % [m]       hub height
Parameter.H_Lidar   = 2;            % [m]       height of lidar over hub

% get data
if strcmp(options.UnitTilt,'deg') % tilt is positive if lidar looks up (negative lidar / positive turbine pitch angle). tau should be in [rad]        
    tau         = deg2rad(Statistics.mean_Tilt); 
else
    tau         = Statistics.mean_Tilt;
end
for iDistance = IndicesDistances        
    RWS_0   = Statistics.("mean_RWS"+iDistance+"_1");
    RWS_1   = Statistics.("mean_RWS"+iDistance+"_2");
    RWS_2   = Statistics.("mean_RWS"+iDistance+"_3");
    RWS_3   = Statistics.("mean_RWS"+iDistance+"_4");
    % apply
    [HWS_hub,Direction_hub,Shear,Veer] = LeosphereWfr(RWS_0,RWS_1,RWS_2,RWS_3,tau,iDistance+options.IndexOffset,Parameter);
    % add to Statistics
    Statistics.("HWS_hub"+iDistance)        = HWS_hub;
    Statistics.("Direction_hub"+iDistance)  = Direction_hub;
    Statistics.("Shear"+iDistance)          = Shear;
    Statistics.("Veer"+iDistance)           = Veer;
end

end