function ProcessResults = EstimateAveragedAutoSpectra(ProcessResults,FrequencyResults,InputID,OutputID,options)
% Estimates averaged autospectra for specific filter.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct  % set to struct() to initialize
    FrequencyResults            struct
    InputID                     char            
    OutputID                    char    = ['mean_',InputID] 
    options.FilterID            char    = 'Filter'
end

% determine Filter
if strcmp(options.FilterID,'all') % use all files
    Filter                      = true(FrequencyResults.nDataFiles,1);
else
    Filter                      = ProcessResults.(options.FilterID).Matrix;
end

% get spectra and frequency
[~,nFilters]                    = size(Filter);
for iFilter=1:nFilters
    isConsidered                = Filter(:,iFilter);

    if sum(isConsidered)>0
        S                           = mean([FrequencyResults.(InputID)(isConsidered).S],2);
        iConsideredDataFile         = find(isConsidered,1); % first considered file
        f                           = FrequencyResults.(InputID)(iConsideredDataFile).f;
    
        % store in ProcessResults
        ProcessResults.(OutputID)(iFilter).f   = f;
        ProcessResults.(OutputID)(iFilter).S   = S;
    else
        ProcessResults.(OutputID)(iFilter).f   = NaN;
        ProcessResults.(OutputID)(iFilter).S   = NaN;             
    end
end


end
