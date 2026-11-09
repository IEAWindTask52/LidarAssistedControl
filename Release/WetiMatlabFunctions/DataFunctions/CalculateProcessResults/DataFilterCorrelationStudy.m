function ProcessResults = DataFilterCorrelationStudy(ProcessResults,FrequencyResults,Statistics,FilterFunctions,FilterID)
% Filter Data based on various criteria.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    FrequencyResults            struct
    Statistics                  table  
    FilterFunctions             cell
    FilterID                    char = 'GoodData'
end

% get dimensions
FilterIDs	= FilterFunctions(:,1);
Functions  	= FilterFunctions(:,2);
nFunctions 	= size(Functions,1);
nDataFiles  = FrequencyResults.nDataFiles;
GoodData    = NaN(nDataFiles,nFunctions);

% loop over functions
for iFunction = 1:nFunctions
    ThisFunction            = Functions{iFunction};
    GoodData(:,iFunction)   = ThisFunction(Statistics);
end

% combine filters
GoodDataAll         = prod(GoodData,2)==1;      

% store in ProcessResults
ProcessResults.(FilterID).Matrix    = GoodDataAll;
end
