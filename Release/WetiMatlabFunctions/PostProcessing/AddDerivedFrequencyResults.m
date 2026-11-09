function FrequencyResults = AddDerivedFrequencyResults(FrequencyResults,PostProcessingConfig)

% return, if not defined or empty
if ~isfield(PostProcessingConfig,'AddDerivedFrequencyResults')||isempty(PostProcessingConfig.AddDerivedFrequencyResults)
    return
end

% get dimensions
Functions  	= PostProcessingConfig.AddDerivedFrequencyResults(:,1);
nFunction  	= size(Functions,1);

% loop over functions
for iFunction = 1:nFunction
    ThisFunction        = Functions{iFunction};
    FrequencyResults    = ThisFunction(FrequencyResults);
end

end