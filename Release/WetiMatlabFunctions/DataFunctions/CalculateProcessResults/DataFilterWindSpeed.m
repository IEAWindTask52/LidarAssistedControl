function ProcessResults = DataFilterWindSpeed(ProcessResults,FrequencyResults,Statistics,URefMin,URefMax,options)
% Filter Data based on minimum and maximum wind speed.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    FrequencyResults            struct
    Statistics                  table  
    URefMin                     {mustBeNumeric}
    URefMax                     {mustBeNumeric}
    options.FilterName          char = ['WindSpeedMin',num2str(URefMin),'Max',num2str(URefMax)]
    options.PreviousFilters     cell = {'GoodData'}
    options.WindSpeedChannel    char = 'mean_VWindEstC';
end

% get dimensions
nFilters            = length(options.PreviousFilters);
nDataFiles          = FrequencyResults.nDataFiles;
GoodData            = NaN(nDataFiles,nFilters+1);

% find good data from previous filters
for iFilter = 1:nFilters
    ThisFilter              = options.PreviousFilters {iFilter};
    GoodData(:,iFilter)     = ProcessResults.(ThisFilter);
end

% find data for this wind speed
GoodData(:,end)     = Statistics.(options.WindSpeedChannel)>=URefMin &...
                      Statistics.(options.WindSpeedChannel)< URefMax;

% combine filters
GoodDataAll         = prod(GoodData,2)==1;  

% store in ProcessResults
ProcessResults.(options.FilterName)             = GoodDataAll;
end
