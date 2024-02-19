function ProcessResults = CalculateMaxValuesPerBin(ProcessResults,Statistics,InputID,FilterID,OutputID,OutputID_PerBin)
% Calculates maximum value per filter.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    InputID                     char
    FilterID                    char            = 'WindSpeedsBinsFilter' %   default from DataFilterWindSpeedBins, but could be different for real data
    OutputID                    char            = [InputID]
    OutputID_PerBin             char            = [InputID,'_PerBin']          
end

% get dimensions
Filter                      = ProcessResults.(FilterID);
[~,nBins]                   = size(Filter);

% loop over filter
MAX        = Statistics.(InputID);
MAX_PerBin = NaN(1,nBins);
for iBin = 1:nBins
    MAX_PerBin(iBin) = max(MAX(Filter(:,iBin)));    
end

% store in ProcessResults
ProcessResults.(OutputID)           = max(MAX_PerBin);
ProcessResults.(OutputID_PerBin)    = MAX_PerBin;

end
