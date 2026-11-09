% Simple function to read in log files into a time series collection
% DS on 23-Nov-2023
function TSC = ReadLogIntoTSC(FileName,Units)

% removes nan
% - causes problems when done for every file 
% - should be only done, if needed
% ManipulateTXTFile(FileName,'-nan(ind)','0'); 

% load data
Data            = readtable(FileName);
ChannelNames    = Data.Properties.VariableNames;

% get time
timeIdx         = strcmp(ChannelNames,'Time');
time            = Data.Time;

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
