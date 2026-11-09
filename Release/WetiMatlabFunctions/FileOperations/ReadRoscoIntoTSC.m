% Simple function to read in log files into a time series collection
% DS on 20-Feb-2024
function TSC = ReadRoscoIntoTSC(FileName)

% load data
fid             = fopen(FileName);
fgetl(fid);
ChannelName  	= strsplit(fgetl(fid));
ChannelName  	= ChannelName(~cellfun(@isempty,ChannelName));
Units           = strsplit(fgetl(fid));
Units  	        = Units(~cellfun(@isempty,Units));
nChannels       = length(ChannelName);
Format          = repmat('%f',1,nChannels);
Data            = textscan(fid,Format);
fclose(fid);

% get time
timeIdx         = strcmp(ChannelName,'Time');
time            = Data{timeIdx};

% create a tscollection object 
TSC                 = tscollection(time);
[~, TSC.Name]       = fileparts(FileName); % set name 
TSC.TimeInfo.Units  = 's';

% loop over channels
for iChannel = find(~timeIdx)
    % create time series
    TS                  = timeseries(ChannelName{iChannel});
    TS.Time             = time;
    TS.Data             = Data{iChannel};
    TS.DataInfo.Units   = Units{iChannel};
    % add to tscollection object 
    TSC = TSC.addts(TS);
end

end
