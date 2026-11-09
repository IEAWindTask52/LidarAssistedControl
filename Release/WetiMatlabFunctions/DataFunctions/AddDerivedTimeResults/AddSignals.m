function TimeResults = AddSignals(TimeResults,InputID1,InputID2,OutputID)
% Adds two signals.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults         cell
    InputID1            char    
    InputID2            char    
    OutputID            char
end

% get dimensions
nDataFile   = size(TimeResults,1);

% loop over all files
for iDataFile = 1:nDataFile
    % identify data for this BeamID
    Time        = TimeResults{iDataFile}.(InputID1).Time;
    Data1       = TimeResults{iDataFile}.(InputID1).Data;
    Data2       = TimeResults{iDataFile}.(InputID2).Data;
    Data        = Data1+Data2;
    NewChannel  = OutputID;
    % add new channel            
    NewTimeSeries           = timeseries(Data,Time,'Name',NewChannel);
    NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(InputID1).DataInfo.Units; % inherit units 
    TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);
end

end