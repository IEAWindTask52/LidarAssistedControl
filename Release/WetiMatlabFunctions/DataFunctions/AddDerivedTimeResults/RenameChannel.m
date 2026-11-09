function TimeResults = RenameChannel(TimeResults,ThisChannel,NewChannel)
% Names channel for all TSC in TimeResults.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults             cell
    ThisChannel             char
    NewChannel              char
end

% get dimensions
nDataFile       = size(TimeResults,1);

% loop over all files
for iDataFile = 1:nDataFile
     TimeResults{iDataFile} = settimeseriesnames(TimeResults{iDataFile},ThisChannel,NewChannel);
end

end