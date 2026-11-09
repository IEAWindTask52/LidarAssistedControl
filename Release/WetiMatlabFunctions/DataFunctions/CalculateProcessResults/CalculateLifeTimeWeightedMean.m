function ProcessResults = CalculateLifeTimeWeightedMean(ProcessResults,Statistics,InputID,OutputID,OutputID_PerBin,options)
% Calculates life-time weighted mean.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    InputID                     char
    OutputID                    char            = ['LTW_',InputID]
    OutputID_PerBin             char            = ['LTW_',InputID,'_PerBin']
    options.A_Weibull           (1,1) double    = 2/sqrt(pi)*10;    % Class I
    options.k_Weibull           (1,1) double    = 2                 % Rayleigh distribution   
    options.FilterID            char            = 'Filter'    
end
% TODO: 
% * options.Distribution to feed in site-specific wind distribution
% * options.WindClass char {mustBeMember(options.WindClass,{'I','II','III'})} 

% get dimensions
Filter                      = ProcessResults.(options.FilterID).Matrix;
WindSpeedBins               = ProcessResults.(options.FilterID).URef;
nFilters                    = size(Filter,2);
nWindSpeedBins              = size(WindSpeedBins,2);

% check dimension of filter
if nFilters~=nWindSpeedBins
    error(['Dimension of Filter ',options.FilterID,' does not fit to URef vector.']);
end

% local variables for options
A                           = options.A_Weibull;
k                           = options.k_Weibull;

% loop over bins
MEAN        = Statistics.(InputID);
MEAN_PerBin = NaN(1,nWindSpeedBins);
for iWindSpeedBin = 1:nWindSpeedBins
    MEAN_PerBin(iWindSpeedBin) = mean(MEAN(Filter(:,iWindSpeedBin)));    
end

% life time weighting MEAN
Distribution        = k/A*(WindSpeedBins/A).^(k-1).*exp(-(WindSpeedBins/A).^k);
Weights             = Distribution./sum(Distribution);
LTW_MEAN_PerBin     = MEAN_PerBin.*Weights;
LTW_MEAN            = sum(LTW_MEAN_PerBin);

% store in ProcessResults
ProcessResults.(OutputID)           = LTW_MEAN;
ProcessResults.(OutputID_PerBin)    = LTW_MEAN_PerBin;

end
