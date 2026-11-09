function TSC = ReadFlex5IntoTSC(FileName)
% based on loadFlex5

% read in data  and get time
RawData             = res2y(FileName);
time                = RawData.time;

% create a tscollection object 
TSC                 = tscollection(time);
TSC.Name            = FileName; % set name 
TSC.TimeInfo.Units  = 'seconds';

% loop over channels
nChannel            = size(RawData.data,2);
for iChannel = 1:nChannel
    % create time series
    TS                  = timeseries(RawData.name{iChannel});
    TS.Time             = time;
    TS.Data             = RawData.data(:, iChannel);
    TS.DataInfo.Units   = RawData.unit{iChannel};
    % add to tscollection object 
    TSC = TSC.addts(TS);
end

end