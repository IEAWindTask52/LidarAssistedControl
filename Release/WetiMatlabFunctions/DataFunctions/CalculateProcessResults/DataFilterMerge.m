function ProcessResults = DataFilterMerge(ProcessResults,Statistics,NewFilterFunction,PreviousFilters,FilterID)
% Merges Data filtes.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table  
    NewFilterFunction           cell            % additional filter to merge on-top of PreviousFilters
    PreviousFilters             cell = {}       % originall filter 
    FilterID                    char = 'Filter' % Merged filter name
end

% get initial dimensions 
Functions  	            = NewFilterFunction(:,1);
nFunctions 	            = size(Functions,1);
nDataFiles              = size(Statistics,1);
PreviousFilterMatrix    = true(nDataFiles,1);
NewFilterMatrix         = true(nDataFiles,nFunctions);

% find good data from previous filters
if ~isempty(PreviousFilters)
    nFilters            = length(PreviousFilters);
    for iFilter = 1:nFilters
        ThisFilter              = PreviousFilters{iFilter};
        PreviousFilterMatrix    = mergeLogicalFilters(PreviousFilterMatrix, ProcessResults.(ThisFilter).Matrix);
    end
end

% evaluate new filters
for iFunctions = 1:nFunctions
    NewFilterMatrix(:,iFunctions) = Functions{iFunctions}(Statistics);
end

% Merge previous + new filters
FinalFilter = mergeLogicalFilters(PreviousFilterMatrix,NewFilterMatrix);

% store result
ProcessResults.(FilterID).Matrix    = FinalFilter;
end

function Merged = mergeLogicalFilters(A, B)
% A, B are logical matrices with same row count

[nA, cA] = size(A);
[nB, cB] = size(B);

if nA ~= nB
    error('Filter row dimensions do not match.');
end

if cA == 1 && cB > 1
    A = repmat(A, 1, cB);
elseif cB == 1 && cA > 1
    B = repmat(B, 1, cA);
elseif cA ~= cB
    error('Filter column dimensions are incompatible.');
end

Merged = A & B;
end