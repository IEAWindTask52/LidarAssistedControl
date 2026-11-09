function ProcessResults = CalculatePreProcessingMean(ProcessResults,Statistics,InputID,OutputID,options)
% Calculates combined Mean over Filter and then applies weighting over URef Dimension.
% Similar to CalculatePreProcessingDEL, without m.
% Works together with DataFilterPreProcessing.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    InputID                     char    
    OutputID                    char            = InputID
    options.A_Weibull           (1,1) double    = 2/sqrt(pi)*10;    % Class I
    options.k_Weibull           (1,1) double    = 2                 % Rayleigh distribution                    
    options.FilterID            char            = 'Filter'
    options.URef_ex             (:,1) double    = [];               % extention
    options.Mean_uw_ex          (:,1) double    = [];               % extention, same length as URef_ex     
end

% get dimensions
Filter                      = ProcessResults.(options.FilterID).Matrix;
nFilters                    = size(Filter,2);

% local variables from options
A                           = options.A_Weibull;
k                           = options.k_Weibull;

% allocation
MEAN                        = Statistics.(InputID);
CombinedMean                = NaN(1,nFilters);

% combined MEAN
for iFilter = 1:nFilters
    Mean_ThisFilter         = MEAN(Filter(:,iFilter));
    CombinedMean(iFilter)   = mean(Mean_ThisFilter);
end

switch ProcessResults.(options.FilterID).nVariation
    case {1,2,3}

        % dimensions
        URef                = ProcessResults.(options.FilterID).Weighted.URef;
        nURefs              = length(URef);
        nx3                 = nFilters/nURefs; % 

        % reshape                    
        ReshapedMean        = reshape(CombinedMean,nURefs,[]);

        % extend, if requested
        % TODO: currently only for smaller wind speeds, should be generalized, e.g by sorting 
        if ~isempty(options.URef_ex) && ~isempty(options.Mean_uw_ex)
            URef            = [options.URef_ex; ProcessResults.(options.FilterID).Weighted.URef];
            nURefs          = length(URef);
            ReshapedMean    = [repmat(options.Mean_uw_ex,1,nx3);ReshapedMean];
        end

        % Weights     
        Distribution        = k/A*(URef/A).^(k-1).*exp(-(URef/A).^k);
        Weights             = Distribution./sum(Distribution);

        % weighted Mean
        WeightedMean        = ReshapedMean.*repmat(Weights,1,nx3);
        
         % Lifetime Mean
        LifetimeMean        = sum(WeightedMean);        

    case 4

        % dimensions
        URef                = ProcessResults.(options.FilterID).Weighted.URef;
        nURefs              = length(URef);        
        nx3                 = ProcessResults.(options.FilterID).Lifetime.nx3;
        nx4                 = ProcessResults.(options.FilterID).Lifetime.nx4;

        % reshape
        ReshapedMean        = reshape(CombinedMean,nURefs,[]);

        % extend, if requested
        % TODO: currently only for smaller wind speeds, should be generalized, e.g by sorting 
        if ~isempty(options.URef_ex) && ~isempty(options.Mean_uw_ex)
            URef            = [options.URef_ex; ProcessResults.(options.FilterID).Weighted.URef];
            nURefs          = length(URef);
            ReshapedMean    = [repmat(options.Mean_uw_ex,1,nx3*nx4);ReshapedMean];
        end

        % Weights     
        Distribution        = k/A*(URef/A).^(k-1).*exp(-(URef/A).^k);
        Weights             = Distribution./sum(Distribution);        
      
        % weighted Mean
        WeightedMean        = ReshapedMean.*repmat(Weights,1,nx3*nx4);
        
        % Lifetime Max
        LifetimeMean        = reshape(sum(WeightedMean,1),nx3,nx4); 
   
    otherwise
        error('Only defined for 3 dimension by now. Extend if needed.')
end

% store in ProcessResults
ProcessResults.([OutputID,'_Weighted'])         = ProcessResults.(options.FilterID).Weighted; % handover (e.g. x3Name)
ProcessResults.([OutputID,'_Weighted']).URef    = URef; % update in case of extention
ProcessResults.([OutputID,'_Weighted']).Mean    = WeightedMean;
ProcessResults.([OutputID,'_Weighted']).Mean_uw = ReshapedMean;
if isfield(ProcessResults.(options.FilterID),'Lifetime')
    ProcessResults.([OutputID,'_Lifetime'])     = ProcessResults.(options.FilterID).Lifetime; % handover
end
ProcessResults.([OutputID,'_Lifetime']).Mean    = LifetimeMean;

end
