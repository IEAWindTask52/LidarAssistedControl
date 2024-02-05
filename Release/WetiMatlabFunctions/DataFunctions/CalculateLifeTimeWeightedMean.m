function ProcessResults = CalculateLifeTimeWeightedMean(ProcessResults,Statistics,WindSpeedBins,InputID,FilterID,OutputID,OutputID_PerBin,options)
% Calculates life-time weighted DEL.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    WindSpeedBins               (1,:) double
    InputID                     char
    FilterID                    char            = 'WindSpeedsBinsFilter' %   default from DataFilterWindSpeedBins, but could be different for real data
    OutputID                    char            = ['LTW_',InputID]
    OutputID_PerBin             char            = ['LTW_',InputID,'_PerBin']
    options.A_Weibull           (1,1) double    = 2/sqrt(pi)*10;    % Class I
    options.k_Weibull           (1,1) double    = 2                 % Rayleigh distribution                
end
% TODO: 
% * options.Distribution to feed in site-specific wind distribution
% * options.WindClass char {mustBeMember(options.WindClass,{'I','II','III'})} 

% get dimensions
nWindSpeedBins              = size(WindSpeedBins,2);
Filter                      = ProcessResults.(FilterID);
[~,nFilters]                = size(Filter);

% local variables for options
A                           = options.A_Weibull;
k                           = options.k_Weibull;

% check dimension of filter
if nFilters~=nWindSpeedBins
    error(['Dimension of Filter ',FilterID,' does not fit to WindSpeedBins input.']);
end

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
