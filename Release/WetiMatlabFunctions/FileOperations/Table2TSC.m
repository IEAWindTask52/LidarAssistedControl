% Simple function to convert a table into a time series collection
function TSC = Table2TSC(Table,NewTime,Units,options)

% inputs
arguments
    Table                   table    
    NewTime                 (:,1) double = []
    Units                   struct = struct()
    options.timeChannel     char = 'time'
    options.Name            char = 'unnamed'
end

% get time
ChannelNames        = Table.Properties.VariableNames;
timeIdx             = strcmp(ChannelNames,options.timeChannel);
time                = Table.(options.timeChannel);

% create a tscollection object
if isempty(NewTime)
    TSC             = tscollection(time);
else
    TSC             = tscollection(NewTime);
end
TSC.Name            = options.Name;
TSC.TimeInfo.Units  = 'seconds';

% loop over channels
for iChannel = find(~timeIdx)
    % create time series
    TS                  = timeseries(ChannelNames{iChannel});
    if isempty(NewTime)
        TS.Time         = time;
        TS.Data         = Table.(ChannelNames{iChannel});        
    else
        TS.Time         = NewTime;
        TS.Data         = interp1(time,Table.(ChannelNames{iChannel}),NewTime,'previous','extrap');
    end    
    % add unit
    if isfield(Units,ChannelNames{iChannel})
        TS.DataInfo.Units   = Units.(ChannelNames{iChannel});
    end
    % add to tscollection object 
    TSC = TSC.addts(TS);
end

end
