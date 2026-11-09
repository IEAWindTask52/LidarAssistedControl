function ProcessResults = DataFilterStatistics(ProcessResults,Statistics,FilterFunctions,PreviousFilters,FilterID)
% Filters data based on various criteria.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table  
    FilterFunctions             cell
    PreviousFilters             cell = {}    
    FilterID                    char = 'Filter'
end

% get dimensions
Functions  	= FilterFunctions(:,1);
nFunctions 	= size(Functions,1);
nDataFiles  = size(Statistics,1);
PreFilter   = true(nDataFiles,1);
GoodData    = false(nDataFiles,nFunctions);

% find good data from previous filters
if ~isempty(PreviousFilters)
    nFilters            = length(PreviousFilters);
    for iFilter = 1:nFilters
        ThisFilter      = PreviousFilters{iFilter};
        PreFilter       = PreFilter & ProcessResults.(ThisFilter).Matrix;
    end
end

% loop over functions
for iFunction = 1:nFunctions
    ThisFunction            = Functions{iFunction};
    GoodData(:,iFunction)   = PreFilter & ThisFunction(Statistics);
end

% store in ProcessResults
ProcessResults.(FilterID).Matrix    = GoodData;
end
