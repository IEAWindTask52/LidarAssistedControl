function ProcessResults = CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,WindSpeedBins,InputID,FilterID,OutputID,OutputID_PerBin,options)
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
    options.WoehlerExponent     (1,:) double    = 4 % steel
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
m                           = options.WoehlerExponent;
A                           = options.A_Weibull;
k                           = options.k_Weibull;

% check dimension of filter
if nFilters~=nWindSpeedBins
    error(['Dimension of Filter ',FilterID,' does not fit to WindSpeedBins input.']);
end

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
