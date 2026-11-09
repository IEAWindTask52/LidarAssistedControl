function ProcessResults = EstimateAveragedCoherence(ProcessResults,FrequencyResults,InputID_11,InputID_22,InputID_12,OutputID,options)
% Estimates averaged squared coherence for specific filter.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct  % set to struct() to initialize
    FrequencyResults            struct
    InputID_11                  char 
    InputID_22                  char
    InputID_12                  char
    OutputID                    char
    options.FilterID            char    = 'Filter' 
    options.URef                double  = [] 
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
        S_11                    = mean([FrequencyResults.(InputID_11)(isConsidered).S],2);    
        S_22                    = mean([FrequencyResults.(InputID_22)(isConsidered).S],2);    
        S_12                    = mean([FrequencyResults.(InputID_12)(isConsidered).S],2);    
        iConsideredDataFile     = find(isConsidered,1); % first considered file
        f                       = FrequencyResults.(InputID_11)(iConsideredDataFile).f;
        
        % get average coherence
        C                       = abs(S_12).^2./(S_11.*S_22);
  
        % store in ProcessResults
        ProcessResults.(OutputID)(iFilter).f        = f;     
        ProcessResults.(OutputID)(iFilter).C        = C;

        % optional calculation of k
        if ~isempty(options.URef)
            URef               = options.URef(iFilter);
            ProcessResults.(OutputID)(iFilter).k    = 2*pi*f/URef;
        end
    else
        ProcessResults.(OutputID)(iFilter).f        = NaN;      
        ProcessResults.(OutputID)(iFilter).C        = NaN;
        if ~isempty(options.URef)
            ProcessResults.(OutputID)(iFilter).k    = NaN;
        end
    end
end

end
