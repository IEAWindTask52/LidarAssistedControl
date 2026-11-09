function ProcessResults = CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,InputID,OutputID,OutputID_PerBin,options)
% Calculates life-time weighted DEL.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    InputID                     char    
    OutputID                    char            = ['LTW_',InputID]
    OutputID_PerBin             char            = ['LTW_',InputID,'_PerBin']
    options.WoehlerExponent     (1,:) double    = 4 % steel
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
m                           = options.WoehlerExponent;
A                           = options.A_Weibull;
k                           = options.k_Weibull;

% loop over bins
DEL         = Statistics.(InputID);
DEL_PerBin  = NaN(1,nWindSpeedBins);
for iWindSpeedBin = 1:nWindSpeedBins
    DEL_ThisBin     = DEL(Filter(:,iWindSpeedBin));
    n_ThisBin       = length(DEL_ThisBin);
    Weights         = ones(n_ThisBin,1)/n_ThisBin; % equal weights
    DEL_PerBin(iWindSpeedBin) = sum(Weights.*DEL_ThisBin.^m).^(1/m);
end

% life time weighting DEL
Distribution        = k/A*(WindSpeedBins/A).^(k-1).*exp(-(WindSpeedBins/A).^k);
Weights             = Distribution./sum(Distribution);
LTW_DEL_PerBin      = DEL_PerBin.*Weights.^(1/m);
LTW_DEL             = sum(LTW_DEL_PerBin.^m).^(1/m);

% store in ProcessResults
ProcessResults.(OutputID)           = LTW_DEL;
ProcessResults.(OutputID_PerBin)    = LTW_DEL_PerBin;

end
