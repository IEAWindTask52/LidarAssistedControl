function DEL = CalculateDEL(Data,Time,WoehlerExponent,N_REF)
% Calculates Damage Equivalent Loads.
% Function for CalculateStatistics.

% inputs
arguments
    Data                (:,1) double
    Time                (:,1) double
    WoehlerExponent     {mustBeInteger}
    N_REF               double = 2e6/(20*8760*3600/600);% [-]   fraction of 2e6 in 20 years for 600s
end

% calculate DEL
dt          = diff(Time([1,2]));
c           = rainflow(Data,1/dt); % TODO: Why slightly different from rainflow(Data,Time-Time(1))?
Count       = c(:,1);
Range       = c(:,2);
DEL         = (sum(Range.^WoehlerExponent.*Count)/N_REF).^(1/WoehlerExponent);

end