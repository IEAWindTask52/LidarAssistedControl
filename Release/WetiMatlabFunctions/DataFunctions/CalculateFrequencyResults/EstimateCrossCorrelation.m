function Output = EstimateCrossCorrelation(Data,Time,options)
% Estimates cross correlation for two time series.
% Function for CalculateFrequencyResults.

% inputs
arguments
    Data                (2,1) cell
    Time                (2,1) cell
    options.detrend     char {mustBeMember(options.detrend,{'constant','linear'})}  = 'constant'
    options.scaleopt    char {mustBeMember(options.scaleopt,{'none','biased','unbiased','normalized','coeff'})}  = 'normalized'
    options.StartTime   double  = 0
    options.Mode        char {mustBeMember(options.Mode,{'normal','circular'})}  = 'normal'
    options.Tlim        (1,2) double = [-inf inf]
    options.Step        (1,1) int32 = 1
end

% calculate time step
dt1                 = uniquetol(diff(Time{1}),1e-3);
dt2                 = uniquetol(diff(Time{2}),1e-3);
dt                  = unique([dt1;dt2]);
if length(dt)>1
    error(['Time vectors for cross correlation estimation need to be equally spaced! Here following time steps are found:',num2str(dt)])
end

% considered time only
IsConsidered1       = Time{1}>=options.StartTime;
IsConsidered2       = Time{2}>=options.StartTime;
ConsideredData1     = Data{1}(IsConsidered1);
ConsideredData2     = Data{2}(IsConsidered2);

% detrend
DetrendedData1      = detrend(ConsideredData1,options.detrend);
DetrendedData2      = detrend(ConsideredData2,options.detrend); 

% estimate cross correlation
switch options.Mode
    case 'normal'
        [c,lags]    = xcorr(DetrendedData1,DetrendedData2,options.scaleopt);
        t           = lags*dt;
    case 'circular'
        [c,lags]    = cxcorr(DetrendedData1,DetrendedData2,options.scaleopt);
        t           = lags*dt;
end


% calculate time shift
[c_max,MaxIdx]      = max(c);
T                   = max(t)+dt; % period
T_shift             = t(MaxIdx);
if T_shift < -T/2
    T_shift         = mod(T_shift,T);
end

% output struct
StartIdx            = find(t>=options.Tlim(1),1,'first');
EndIdx              = find(t<=options.Tlim(2),1,'last');
Output.t            = t(StartIdx:options.Step:EndIdx)';
Output.c            = c(StartIdx:options.Step:EndIdx);
Output.T_shift      = T_shift;
Output.c_max        = c_max;

end