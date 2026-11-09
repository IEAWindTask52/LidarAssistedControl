function ProcessResults = EstimateAveragedCrossCorrelation(ProcessResults,FrequencyResults,InputID,OutputID,options)
% Estimates averaged cross correlation for specific filter.
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
        c                           = mean([FrequencyResults.(InputID)(isConsidered).c],2);
        iConsideredDataFile         = find(isConsidered,1); % first considered file
        t                           = FrequencyResults.(InputID)(iConsideredDataFile).t;
    
        % calculate time shift
        dt                  = t(2)-t(1);
        [c_max,MaxIdx]      = max(c);
        T                   = max(t)+dt; % period
        T_shift             = t(MaxIdx);
        if T_shift < -T/2
            T_shift         = mod(T_shift,T);
        end    
    
        % store in ProcessResults
        ProcessResults.(OutputID)(iFilter).t        = t;
        ProcessResults.(OutputID)(iFilter).c        = c;
        ProcessResults.(OutputID)(iFilter).T_shift  = T_shift;
        ProcessResults.(OutputID)(iFilter).c_max    = c_max;
    else
        % store in ProcessResults
        ProcessResults.(OutputID)(iFilter).t        = NaN;
        ProcessResults.(OutputID)(iFilter).c        = NaN;
        ProcessResults.(OutputID)(iFilter).T_shift  = NaN;
        ProcessResults.(OutputID)(iFilter).c_max    = NaN;        
    end
end


end
