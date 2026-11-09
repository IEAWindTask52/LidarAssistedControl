function MaxGap = LongestGap(Data,Time,options)
% Calculates longest gap in a time series.
% Function for CalculateStatistics.

% inputs
arguments
    Data                (:,1) double
    Time                (:,1) double
    options.ErrorCode   {mustBeNumeric} = 0
end

% get dimensions
nData       = length(Data);

% Vector with 0 if good data and 1 if error
IsError     = Data == options.ErrorCode;

% Get start and end of error series (0 to detect error in first/last point)
StartIdx    = find(diff([0;IsError;0])== 1);
EndIdx      = find(diff([0;IsError;0])==-1);

% limiting EndIdx to nData (necessary if last point is an error)
% could be avoided e.g. by assuming uniform time steps
EndIdx      = min(EndIdx,nData);

% Gap times
Gaps        = Time(EndIdx)-Time(StartIdx);

% get longest gap
MaxGap      = max(Gaps);

end