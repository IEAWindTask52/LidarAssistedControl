function PitchTravel= CalculatePitchTravel(Data,Time,StartTime)
% Calculates pitch travel from a pitch angle signal.
% Function for CalculateStatistics.

% inputs
arguments
    Data                (:,1) double
    Time                (:,1) double
    StartTime           (1,1) double
end

PitchRate   = gradient(Data,Time);
dt          = Time(2)-Time(1); % assuming equally spaced time
PitchTravel = sum(abs(PitchRate(Time>=StartTime)))*dt;

end