function ProcessResults = CalculateMaxValuesPerBin(ProcessResults,Statistics,InputID,OutputID,OutputID_PerBin,options)
% Calculates maximum value per filter.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    InputID                     char
    OutputID                    char            = [InputID]
    OutputID_PerBin             char            = [InputID,'_PerBin']   
    options.FilterID            char            = 'Filter'        
end

% get dimensions
Filter                      = ProcessResults.(options.FilterID).Matrix;
WindSpeedBins               = ProcessResults.(options.FilterID).URef;
nFilters                    = size(Filter,2);
nWindSpeedBins              = size(WindSpeedBins,2);

% check dimension of filter
if nFilters~=nWindSpeedBins
    error(['Dimension of Filter ',options.FilterID,' does not fit to URef vector.']);
end

% loop over filter
MAX        = Statistics.(InputID);
MAX_PerBin = NaN(1,nWindSpeedBins);
for iWindSpeedBin = 1:nWindSpeedBins
    MAX_PerBin(iWindSpeedBin) = max(MAX(Filter(:,iWindSpeedBin)));    
end

% store in ProcessResults
ProcessResults.(OutputID)           = max(MAX_PerBin);
ProcessResults.(OutputID_PerBin)    = MAX_PerBin;

end
