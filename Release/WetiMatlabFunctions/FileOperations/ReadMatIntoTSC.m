% Simple function to read in vectors in a mat files into a time series collection
function TSC = ReadMatIntoTSC(FileName,Units,options)

% inputs
arguments
    FileName                char
    Units                   struct = struct()
    options.dt              double  = []  
    options.TimeChannel     char = 'Time'
end

% load data
Data            = load(FileName);
ChannelNames    = fields(Data)';

% get time
timeIdx         = strcmp(ChannelNames,'Time');
if any(timeIdx)
    time        = Data.(options.TimeChannel);
elseif ~isempty(options.dt)
    nData       = length(Data.(ChannelNames{1})); % assuming all data has same length
    time        = [0:nData-1]*options.dt;
else
    error('No time channel found or dt defined.')
end

% create a tscollection object 
TSC                 = tscollection(time);
[~, TSC.Name]       = fileparts(FileName); % set name 
TSC.TimeInfo.Units  = 'seconds';

% loop over channels
for iChannel = find(~timeIdx)
    % create time series
    TS                  = timeseries(ChannelNames{iChannel});
    TS.Time             = time;
    TS.Data             = Data.(ChannelNames{iChannel});
    if isfield(Units,ChannelNames{iChannel})
        TS.DataInfo.Units   = Units.(ChannelNames{iChannel});
    end
    % add to tscollection object 
    TSC = TSC.addts(TS);
end

end
