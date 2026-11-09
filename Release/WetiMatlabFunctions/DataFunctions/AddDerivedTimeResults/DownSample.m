function TimeResults = DownSample(TimeResults,Step,options)
% Samples down all time series collections. 
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults             cell        
    Step                    {mustBeInteger} 
    options.nCore           {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing    
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'    
end

% get dimensions
nDataFiles          = size(TimeResults,1);

% loop over all files
parfor (iDataFile = 1:nDataFiles,options.nCore)
    % display if requested
    if strcmp(options.Display,'on')                
        MyText      = "Downsampling: " + iDataFile + "/" + nDataFiles + "\n";
        fprintf(MyText);
    end     % get new time vector
    OldTime         = TimeResults{iDataFile}.Time;
    NewTime         = OldTime(1:Step:end);
    % resample
    TimeResults{iDataFile} = resample(TimeResults{iDataFile},NewTime);
end

end