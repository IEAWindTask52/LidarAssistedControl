function ProcessResults = CalculatePreProcessingDEL(ProcessResults,Statistics,InputID,OutputID,options)
% Calculates combined DEL over Filter and then applies weighting over URef Dimension.
% Works together with DataFilterPreProcessing.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    InputID                     char    
    OutputID                    char            = InputID
    options.WoehlerExponent     (1,:) double    = 4 % steel     
    options.A_Weibull           (1,1) double    = 2/sqrt(pi)*10;    % Class I
    options.k_Weibull           (1,1) double    = 2                 % Rayleigh distribution                    
    options.FilterID            char            = 'Filter'
    options.URef_ex             (:,1) double    = [];               % extention
    options.DEL_uw_ex           (:,1) double    = [];               % extention, same length as URef_ex
end

% get dimensions
Filter                      = ProcessResults.(options.FilterID).Matrix;
nFilters                    = size(Filter,2);

% local variables from options
m                           = options.WoehlerExponent;
A                           = options.A_Weibull;
k                           = options.k_Weibull;

% allocation
DEL                         = Statistics.(InputID);
CombinedDEL                 = NaN(1,nFilters);

% combined DEL
for iFilter = 1:nFilters
    DEL_ThisFilter          = DEL(Filter(:,iFilter));
    n_ThisFilter            = length(DEL_ThisFilter);
    Weights                 = ones(n_ThisFilter,1)/n_ThisFilter; % equal weights
    CombinedDEL(iFilter)    = sum(Weights.*DEL_ThisFilter.^m).^(1/m);
end

switch ProcessResults.(options.FilterID).nVariation
    case {1,2,3}

        % dimensions
        URef                = ProcessResults.(options.FilterID).Weighted.URef;
        nURefs              = length(URef);
        nx3                 = nFilters/nURefs; % 

        % reshape                    
        ReshapedDEL         = reshape(CombinedDEL,nURefs,[]);

        % extend, if requested
        % TODO: currently only for smaller wind speeds, should be generalized, e.g by sorting 
        if ~isempty(options.URef_ex) && ~isempty(options.DEL_uw_ex)
            URef            = [options.URef_ex; ProcessResults.(options.FilterID).Weighted.URef];
            nURefs          = length(URef);
            ReshapedDEL     = [repmat(options.DEL_uw_ex,1,nx3);ReshapedDEL];
        end

        % Weights     
        Distribution        = k/A*(URef/A).^(k-1).*exp(-(URef/A).^k);
        Weights             = Distribution./sum(Distribution);
      
        % weighted DEL
        WeightedDEL         = ReshapedDEL.*repmat(Weights,1,nx3).^(1/m);
        
        % Lifetime DEL
        LifetimeDEL         = sum(WeightedDEL.^m).^(1/m);

    case 4

        % dimensions
        URef                = ProcessResults.(options.FilterID).Weighted.URef;
        nURefs              = length(URef);        
        nx3                 = ProcessResults.(options.FilterID).Lifetime.nx3;
        nx4                 = ProcessResults.(options.FilterID).Lifetime.nx4;

        % reshape
        ReshapedDEL         = reshape(CombinedDEL,nURefs,[]);

        % extend, if requested
        % TODO: currently only for smaller wind speeds, should be generalized, e.g by sorting 
        if ~isempty(options.URef_ex) && ~isempty(options.DEL_uw_ex)
            URef            = [options.URef_ex; ProcessResults.(options.FilterID).Weighted.URef];
            nURefs          = length(URef);
            ReshapedDEL     = [repmat(options.DEL_uw_ex,1,nx3*nx4);ReshapedDEL];
        end

        % Weights
        Distribution        = k/A*(URef/A).^(k-1).*exp(-(URef/A).^k);
        Weights             = Distribution./sum(Distribution);        
        
        % weighted DEL
        WeightedDEL         = ReshapedDEL.*repmat(Weights,1,nx3*nx4).^(1/m); 
        
        % Lifetime DEL
        LifetimeDEL         = reshape(sum(WeightedDEL.^m,1).^(1/m),nx3,nx4);
        
    otherwise
        error('Only defined up to 4 dimension by now. Extend if needed.')
end

% store in ProcessResults
ProcessResults.([OutputID,'_Weighted'])         = ProcessResults.(options.FilterID).Weighted; % handover (e.g. x3Name)
ProcessResults.([OutputID,'_Weighted']).URef    = URef; % update in case of extention
ProcessResults.([OutputID,'_Weighted']).DEL     = WeightedDEL;
ProcessResults.([OutputID,'_Weighted']).DEL_uw  = ReshapedDEL;
if isfield(ProcessResults.(options.FilterID),'Lifetime')
    ProcessResults.([OutputID,'_Lifetime'])     = ProcessResults.(options.FilterID).Lifetime; % handover
end
ProcessResults.([OutputID,'_Lifetime']).DEL     = LifetimeDEL;

end
