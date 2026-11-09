function TimeResults = RemoveChannels(TimeResults,ChannelCell,options)
% Names channel for all TSC in TimeResults.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults             cell
    ChannelCell             cell
    options.nCore           {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing    
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'    
end

% get dimensions
nDataFile       = size(TimeResults,1);
nChannels       = length(ChannelCell);

% loop over all files
parfor (iDataFile = 1:nDataFiles,options.nCore)
    % display if requested
    if strcmp(options.Display,'on')                
        MyText      = "Removing channels: " + iDataFile + "/" + nDataFiles + "\n";
        fprintf(MyText);
    end 
    % remove TS
    for iChannel = 1:nChannels
        TimeResults{iDataFile}  = removets(TimeResults{iDataFile},ChannelCell{iChannel});
    end
end

end