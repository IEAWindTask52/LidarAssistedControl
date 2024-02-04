function TSC = ReadFASTbinaryIntoTSC(FileName)
% based on LoadFAST

% read in data  and get time
[Channels, ChanName, ChanUnit, ~, ~] = ReadFASTbinary(FileName);
time                = Channels(:, 1);

% create a tscollection object 
TSC                 = tscollection(time);
TSC.Name            = FileName; % set name 
TSC.TimeInfo.Units  = 's';

% loop over channels
nChannel            = length(ChanName);
for iChannel = 2:nChannel
    % create time series
    TS                  = timeseries(ChanName{iChannel});
    TS.Time             = Channels(:, 1);
    TS.Data             = Channels(:, iChannel);
    TS.DataInfo.Units   = erase(ChanUnit{iChannel},{'(';')'});% remove ()
    % add to tscollection object 
    TSC = TSC.addts(TS);
end

end