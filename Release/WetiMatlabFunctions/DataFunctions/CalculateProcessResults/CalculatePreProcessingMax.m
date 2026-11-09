function ProcessResults = CalculatePreProcessingMax(ProcessResults,Statistics,InputID,OutputID,options)
% Calculates combined Mean over Filter and then max over URef Dimension.
% Similar to CalculatePreProcessingDEL, without m.
% Works together with DataFilterPreProcessing.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    Statistics                  table
    InputID                     char    
    OutputID                    char            = InputID                  
    options.FilterID            char            = 'Filter'    
    options.URef_ex             (:,1) double    = [];               % extention
    options.Mean_uw_ex          (:,1) double    = [];               % extention, same length as URef_ex    
end

% get dimensions
Filter                      = ProcessResults.(options.FilterID).Matrix;
nFilters                    = size(Filter,2);

% allocation
MAX                         = Statistics.(InputID);
CombinedMean                = NaN(1,nFilters);

% combined MEAN
for iFilter = 1:nFilters
    MAX_ThisFilter          = MAX(Filter(:,iFilter));
    CombinedMean(iFilter)   = mean(MAX_ThisFilter);
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
      
        % weighted Mean
        WeightedMean        = ReshapedMean;
        
         % Lifetime Max
        LifetimeMax         = max(WeightedMean);            

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
      
        % weighted Mean
        WeightedMean        = ReshapedMean; 
        
        % Lifetime Max
        LifetimeMax         = reshape(max(WeightedMean,[],1),nx3,nx4);
        
    otherwise
        error('Only defined for 3 dimension by now. Extend if needed.')
end

% store in ProcessResults
ProcessResults.([OutputID,'_Weighted'])         = ProcessResults.(options.FilterID).Weighted; % handover (e.g. x3Name)
ProcessResults.([OutputID,'_Weighted']).URef    = URef; % update in case of extention
ProcessResults.([OutputID,'_Weighted']).Mean    = WeightedMean;
if isfield(ProcessResults.(options.FilterID),'Lifetime')
    ProcessResults.([OutputID,'_Lifetime'])     = ProcessResults.(options.FilterID).Lifetime; % handover
end
ProcessResults.([OutputID,'_Lifetime']).Max     = LifetimeMax;

end
