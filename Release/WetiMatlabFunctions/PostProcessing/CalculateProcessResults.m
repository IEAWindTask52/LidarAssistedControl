function ProcessResults = CalculateProcessResults(FrequencyResults,Statistics,PostProcessingConfig)

% return, if not defined or empty
if ~isfield(PostProcessingConfig,'CalculateProcessResults')||isempty(PostProcessingConfig.CalculateProcessResults)
    ProcessResults = struct();
    return
end

% get dimensions
Functions  	= PostProcessingConfig.CalculateProcessResults(:,1);
nFunctions	= size(Functions,1);

% initalization 
ProcessResults = struct();

% loop over functions
for iFunction = 1:nFunctions
    ThisFunction        = Functions{iFunction};
    ProcessResults      = ThisFunction(ProcessResults,FrequencyResults,Statistics);
end

end