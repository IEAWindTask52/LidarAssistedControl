function ProcessResults = DataFilterWindSpeedBins(ProcessResults,Statistics,WindSpeedBins,WindSpeedChannel,WindSpeedBinWidth,PreviousFilters,FilterID)
% Filter Data based on wind speed bins.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table  
    WindSpeedBins               (1,:) {mustBeNumeric}
    WindSpeedChannel            char
    WindSpeedBinWidth           (1,1) double = 1;
    PreviousFilters             cell = {}
    FilterID                    char = 'Filter'
end

% get dimensions
nWindSpeedBins  = size(WindSpeedBins,2);
nDataFiles      = size(Statistics,1);
GoodData        = false(nDataFiles,nWindSpeedBins);
PreFilter       = true(nDataFiles,1);

% find good data from previous filters
if ~isempty(PreviousFilters)
    nFilters            = length(PreviousFilters);
    for iFilter = 1:nFilters
        ThisFilter      = PreviousFilters{iFilter};
        PreFilter       = PreFilter & ProcessResults.(ThisFilter).Matrix;
    end
end

% filter
for iWindSpeedBin = 1:nWindSpeedBins
    WindSpeedBin                    = WindSpeedBins(iWindSpeedBin);
    GoodData(:,iWindSpeedBin)       =   PreFilter &...
                                        Statistics.(WindSpeedChannel)>=WindSpeedBin - WindSpeedBinWidth/2   &...
                                        Statistics.(WindSpeedChannel)< WindSpeedBin + WindSpeedBinWidth/2;
end
ProcessResults.(FilterID).Matrix        = GoodData;
ProcessResults.(FilterID).WindSpeedBins = WindSpeedBins;

end