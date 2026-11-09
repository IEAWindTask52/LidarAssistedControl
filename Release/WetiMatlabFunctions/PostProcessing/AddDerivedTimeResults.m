function TimeResults = AddDerivedTimeResults(TimeResults,PostProcessingConfig)

% return, if not defined or empty
if ~isfield(PostProcessingConfig,'AddDerivedTimeResults')||isempty(PostProcessingConfig.AddDerivedTimeResults)
    return
end

% get dimensions
Functions  	= PostProcessingConfig.AddDerivedTimeResults(:,1);
nFunctions  = size(Functions,1);

% loop over functions
for iFunction = 1:nFunctions
    ThisFunction    = Functions{iFunction};
    TimeResults     = ThisFunction(TimeResults);
end

end