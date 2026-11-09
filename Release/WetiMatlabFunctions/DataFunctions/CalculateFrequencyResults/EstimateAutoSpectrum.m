function Output = EstimateAutoSpectrum(Data,Time,Step,options)
% Estimates auto spectrum for a given time series.
% Function for CalculateFrequencyResults.

% inputs
arguments
    Data                (:,1) double
    Time                (:,1) double
    Step                {mustBeInteger} = 1
    options.window      {mustBeNumeric} = floor(length(Data)/Step) % -1 to adjust to new length
    options.nfft        {mustBeInteger} = []
    options.noverlab    {mustBeInteger} = []
    options.detrend     char {mustBeMember(options.detrend,{'constant','linear'})}  = 'constant'
    options.StartTime   double  = 0
end

% calculate sampling time
dt              = uniquetol(diff(Time),1e-3);
if length(dt)>1
    error(['Time vector for spectra estimation needs to be equally spaced! Here following time steps are found:',num2str(dt)])
end
fs              = 1/dt/Step;

% considered time only
ConsideredData  = Data(Time>=options.StartTime);

% downsample, if requested
DownSampledData = ConsideredData(1:Step:end);

% detrend
DetrendedData   = detrend(DownSampledData,options.detrend); 

% adjust window in case of -1
if options.window == -1
    options.window = length(DetrendedData);
end

% estimate spectra
[S,f]           = pwelch(DetrendedData,options.window,options.noverlab,options.nfft,fs);    

% output struct
Output.S        = S;
Output.f        = f;

end