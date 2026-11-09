function TimeResults = ApplyFlagInvalidLOS(TimeResults,Channels,StatusChannel,NewChannels,StatusIdx,options)
% Flag invalid LOS measurements per range gate. Used to set the same
% ErrorCode "0" here refered to as MOLAS method. This function is by
% default setting the invalid measurements to 0.
% Function for ApplyFlagInvalidLOS.

% inputs
arguments   
    TimeResults                 cell
    Channels                    char
    StatusChannel               char
    NewChannels                 char = [] % override by default, use [Channel,'c'] to store as a new channel
    StatusIdx                   double = [10:-1:1] % default for all 10 range gates
    options.Display             char {mustBeMember(options.Display,{'off','on'})} = 'off' 
    options.ErrorCode           double = 0; % Value to flag invalid LOS
end

% get dimensions
nDataFiles          = size(TimeResults,1);
nChannels           = size(Channels,1);
% init display text
MyText              = "";

% loop over files
for iDataFile = 1:nDataFiles % parfor slower than for loop
    % logical status matrix size(:,10)
    Status          = (dec2bin(TimeResults{iDataFile}.(StatusChannel).Data,10)=='1'); 

    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Flag invalid LOS of channel " + Channels(1,:) + " to " + Channels(nChannels,:) +": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end

    for iChannel = 1:nChannels 

    % extract signal
    Data            = TimeResults{iDataFile}.(Channels(iChannel,:)).Data; 
    
    % correction
    idx             = StatusIdx(iChannel); 
    BadData         = ~Status(:,idx);
    Data(BadData)   = options.ErrorCode;

    % add new channel or overwriting data
    if ~isempty(NewChannels)
        NewTimeSeries                   = timeseries(Data,Time,'Name',NewChannels);
        NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(Channels(iChannel,:)).DataInfo.Units;
        NewTimeSeries.TimeInfo          = TimeResults{iDataFile}.TimeInfo; % inherit Time Info
        TimeResults{iDataFile}          = TimeResults{iDataFile}.addts(NewTimeSeries);
    else
        TimeResults{iDataFile}.(Channels(iChannel,:)).Data = Data;
    end
    end
end

