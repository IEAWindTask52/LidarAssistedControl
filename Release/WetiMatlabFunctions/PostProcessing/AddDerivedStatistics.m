function Statistics = AddDerivedStatistics(Statistics,PostProcessingConfig)

% return, if not defined or empty
if ~isfield(PostProcessingConfig,'AddDerivedStatistics')||isempty(PostProcessingConfig.AddDerivedStatistics)
    return
end

% get dimensions
Functions  	= PostProcessingConfig.AddDerivedStatistics(:,1);
nFunction  	= size(Functions,1);

% loop over functions
for iFunction = 1:nFunction
    ThisFunction    = Functions{iFunction};
    Statistics      = ThisFunction(Statistics);
end

end