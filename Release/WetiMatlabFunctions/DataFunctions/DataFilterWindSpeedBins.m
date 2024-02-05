function ProcessResults = DataFilterWindSpeedBins(Statistics,WindSpeedBins,WindSpeedChannel,WindSpeedBinWidth,FilterID,ProcessResults)
% Filter Data based on wind speed bins
% Function for CalculateProcessResults.

% inputs
arguments
    Statistics                  table  
    WindSpeedBins               (1,:) {mustBeNumeric}
    WindSpeedChannel            char
    WindSpeedBinWidth           (1,1) double = 1;
    FilterID                    char = 'WindSpeedsBinsFilter'
    ProcessResults              struct  = struct()
end

% get dimensions
nWindSpeedBins  = size(WindSpeedBins,2);
nDataFiles      = size(Statistics,1);
GoodData        = false(nDataFiles,nWindSpeedBins);

% filter
for iWindSpeedBin = 1:nWindSpeedBins
    WindSpeedBin                    = WindSpeedBins(iWindSpeedBin);
    GoodData(:,iWindSpeedBin)       =   Statistics.(WindSpeedChannel)>=WindSpeedBin - WindSpeedBinWidth/2   &...
                                        Statistics.(WindSpeedChannel)< WindSpeedBin + WindSpeedBinWidth/2;
end
ProcessResults.(FilterID)           = GoodData;
ProcessResults.WindSpeedBins        = WindSpeedBins;

end