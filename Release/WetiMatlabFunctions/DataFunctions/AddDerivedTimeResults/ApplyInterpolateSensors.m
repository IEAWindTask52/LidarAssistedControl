function TimeResults = ApplyInterpolateSensors(TimeResults,ThisChannel,IdxChannel,NewChannel,options)
% Interpolates signals including differnt sensors with a given Error Code.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults             cell
    ThisChannel             char
    IdxChannel              char
    NewChannel              char = ''
    options.ErrorCodeIn     {mustBeNumeric} = 0    
    options.ErrorCodeOut    {mustBeNumeric} = -999
    options.tolerance       {mustBeNumeric} = 1e-3  % tolerance to find values, since outb files are scaled                 
    options.method          {mustBeMember(options.method,{'previous','next','nearest','linear','spline','pchip','makima','meanOfNeighbors'})}  = 'linear'
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'
    options.AllowAllError   {mustBeNumericOrLogical} = false
end

% get dimensions
nDataFiles       = size(TimeResults,1);

% init display text
MyText              = "";

% loop over all files
for iDataFile = 1:nDataFiles
    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Interpolating Sensors in " + ThisChannel + " via " + IdxChannel + ": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end  
    % get data
    Data        = TimeResults{iDataFile}.(ThisChannel).Data;
    SensorIdx   = TimeResults{iDataFile}.(IdxChannel).Data;
    % set data with error to NaN
    if ~isnan(options.ErrorCodeIn)
        IsBadData   = abs(Data-options.ErrorCodeIn)<=options.tolerance;
        if mean(IsBadData)==1 && options.AllowAllError % all data is bad
            Data(IsBadData) = options.ErrorCodeIn;
        else
            Data(IsBadData) = NaN;
        end
    end
    % interpolate data
    Data = InterpolateSensors(Data,SensorIdx,method = options.method);

    % add error code if still NaN (all data is bad and or something went wrong with InterpolateSensors)
    StillNaN        = isnan(Data);    
    Data(StillNaN)  = options.ErrorCodeOut;
    % add new channel or overwrite old one
    if ~isempty(NewChannel)
        Time  = TimeResults{iDataFile}.(ThisChannel).Time;
        NewTimeSeries           = timeseries(Data,Time,'Name',NewChannel);
        NewTimeSeries.DataInfo.Units        = TimeResults{iDataFile}.(ThisChannel).DataInfo.Units; % inherit units 
        NewTimeSeries.TimeInfo.StartDate    = TimeResults{iDataFile}.(ThisChannel).TimeInfo.StartDate; 
        TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);
    else
        TimeResults{iDataFile}.(ThisChannel).Data = Data;
    end   
end

end
%% Helper function for mean of neighbors 
function DataOut = InterpolateSensors(Data, SensorID,options)
% This function is created by Chat GPT
arguments
    Data                    (:,1) double
    SensorID                (:,1) double
    options.method          {mustBeMember(options.method,{'previous','next','nearest','linear','spline','pchip','makima','meanOfNeighbors'})}  = 'linear'
end

if numel(Data) ~= numel(SensorID)
    error('Input arrays must have same length')
end

% ---------------------------------------------------------
% 1) collapse high-rate signal to one value per sensor block
% ---------------------------------------------------------
blockStart = [true; diff(SensorID) ~= 0];
blockID    = cumsum(blockStart);

blockValue = splitapply(@(x)x(1), Data, blockID);

% sensor ID per block
blockSensor = splitapply(@(x)x(1), SensorID, blockID);

% ---------------------------------------------------------
% 2) interpolate missing blocks per sensor
% ---------------------------------------------------------
uSensors = unique(blockSensor,'stable');

for k = 1:numel(uSensors)
    sid = uSensors(k);
    idx = (blockSensor == sid);

    blockValue(idx) = fillmissing(blockValue(idx), ...
                                  options.method, ...
                                  'EndValues','nearest');
end

% ---------------------------------------------------------
% 3) expand back to high-rate rectangular signal
% ---------------------------------------------------------
DataOut = blockValue(blockID);
end