function TSC = ReadFASTbinaryIntoTSC(FileName,Units)

% inputs
arguments
    FileName                char
    Units                   struct = struct()
end

% read in data  and get time
[Channels, ChannelNames, ChanUnit, ~, ~] = ReadFASTbinary(FileName);
time                = Channels(:, 1);

% create a tscollection object 
TSC                 = tscollection(time);
TSC.Name            = FileName; % set name 
TSC.TimeInfo.Units  = 'seconds';

% loop over channels
nChannel            = length(ChannelNames);
for iChannel = 2:nChannel
    % create time series
    TS                  = timeseries(ChannelNames{iChannel});
    TS.Time             = Channels(:, 1);
    TS.Data             = Channels(:, iChannel);

    % units 
    if isfield(Units,ChannelNames{iChannel})
        TS.DataInfo.Units   = Units.(ChannelNames{iChannel});
    else
        TS.DataInfo.Units   = erase(ChanUnit{iChannel},{'(';')'});% remove ()
    end    
    
    % add to tscollection object 
    TSC = TSC.addts(TS);
end

end