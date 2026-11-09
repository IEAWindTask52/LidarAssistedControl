function Output = EstimateCrossSpectrum(Data,Time,Step,options)
% Estimates cross spectrum for two time series.
% Function for CalculateFrequencyResults.

% inputs
arguments
    Data                (2,1) cell
    Time                (2,1) cell
    Step                {mustBeInteger} = 1    
    options.window      {mustBeInteger} = floor(length(Data{1})/Step) % -1 to adjust to new length 
    options.nfft        {mustBeInteger} = []
    options.noverlab    {mustBeInteger} = []
    options.detrend     char {mustBeMember(options.detrend,{'constant','linear'})}  = 'constant'
    options.StartTime   double  = 0    
end

% calculate time step
dt1                 = uniquetol(diff(Time{1}),1e-3);
dt2                 = uniquetol(diff(Time{2}),1e-3);
dt                  = unique([dt1;dt2]);
if length(dt)>1
    error(['Time vectors for cross spectrum estimation need to be equally spaced! Here following time steps are found:',num2str(dt)])
end
fs                  = 1/dt/Step;

% considered time only
ConsideredData1     = Data{1}(Time{1}>=options.StartTime);
ConsideredData2     = Data{2}(Time{2}>=options.StartTime);

% downsample, if requested
DownSampledData1    = ConsideredData1(1:Step:end);
DownSampledData2    = ConsideredData2(1:Step:end);

% detrend
DetrendedData1      = detrend(DownSampledData1,options.detrend);
DetrendedData2      = detrend(DownSampledData2,options.detrend);    

% adjust window in case of -1
if options.window == -1
    options.window = length(DetrendedData1);
end

% estimate cross spectrum
[S,f]               = cpsd(DetrendedData1,DetrendedData2,options.window,options.noverlab,options.nfft,fs);    

% output struct
Output.S        = S;
Output.f        = f;
end