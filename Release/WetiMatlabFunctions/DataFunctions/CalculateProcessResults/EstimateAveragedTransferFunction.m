function ProcessResults = EstimateAveragedTransferFunction(ProcessResults,FrequencyResults,InputID_22,InputID_12,OutputID,options)
% Estimates transfer function for specific filter.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct  % set to struct() to initialize
    FrequencyResults            struct
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
        S_22                    = mean([FrequencyResults.(InputID_22)(isConsidered).S],2);    
        S_12                    = mean([FrequencyResults.(InputID_12)(isConsidered).S],2);    
        iConsideredDataFile     = find(isConsidered,1); % first considered file
        f                       = FrequencyResults.(InputID_22)(iConsideredDataFile).f;
        
        % get average transfer function
        G                       = S_12./S_22;
  
        % store in ProcessResults
        ProcessResults.(OutputID)(iFilter).f        = f;     
        ProcessResults.(OutputID)(iFilter).G        = G;
        ProcessResults.(OutputID)(iFilter).G_mag    = abs(G);

        % optional calculation of k
        if ~isempty(options.URef)
            URef               = options.URef(iFilter);
            ProcessResults.(OutputID)(iFilter).k    = 2*pi*f/URef;
        end        
    else
        ProcessResults.(OutputID)(iFilter).f        = NaN;      
        ProcessResults.(OutputID)(iFilter).G        = NaN; 
        ProcessResults.(OutputID)(iFilter).G_mag    = NaN; 
        if ~isempty(options.URef)
            ProcessResults.(OutputID)(iFilter).k    = NaN;
        end        
    end
end

end
