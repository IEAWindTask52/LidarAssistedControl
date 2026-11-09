function ProcessResults = DataFilterPreProcessing(ProcessResults,PreProcessingVariation,URefIdx,SeedIdx,FilterID)
% Filter Data based on PreProcessingVariation.
% Function for CalculateProcessResults.

% inputs
arguments
    ProcessResults              struct
    PreProcessingVariation      (:,3) cell 
    URefIdx                     (1,1) int32 = 1
    SeedIdx                     (1,1) int32 = 2
    FilterID                    char = 'Filter'
end

% get permutation
[nVariation, nPermutation, Permutation] = CreatePermutationMatrix(PreProcessingVariation);

% get dimensions
URefVariation   = PreProcessingVariation{URefIdx,2};
nURefs          = size(URefVariation,2);
nSeeds          = size(PreProcessingVariation{SeedIdx,2},2);
nFilters        = nPermutation/nSeeds;

% allocation vor all cases
GoodData        = false(nPermutation,nFilters);  
URefCombined    = NaN(1,nFilters);

% get filters
switch nVariation
    case 2      
        for iURef = 1:nURefs
            GoodData(:,iURef)   = Permutation(:,URefIdx)==iURef;
        end
        % output
        ProcessResults.(FilterID).nVariation            = nVariation;
        ProcessResults.(FilterID).Matrix                = GoodData;
        ProcessResults.(FilterID).URef                  = URefVariation;
        ProcessResults.(FilterID).Weighted.URef         = URefVariation';
    case 3      
        x3Combined      = NaN(1,nFilters);
        x3Idx           = find(~ismember(1:3,[URefIdx,SeedIdx]));
        x3Name          = PreProcessingVariation{x3Idx,1};
        x3Variation     = PreProcessingVariation{x3Idx,2};
        nx3             = length(x3Variation);
        CounterC        = 0;
        for ix3 = 1:nx3
            for iURef = 1:nURefs
                CounterC                = CounterC + 1;
                GoodData(:,CounterC)    = Permutation(:,x3Idx)==ix3 & Permutation(:,URefIdx)==iURef;
                x3Combined(CounterC)    = x3Variation(ix3);
                URefCombined(CounterC)  = URefVariation(iURef);
            end
        end
        % output
        ProcessResults.(FilterID).nVariation            = nVariation;
        ProcessResults.(FilterID).Matrix                = GoodData;
        ProcessResults.(FilterID).(x3Name)              = x3Combined; 
        ProcessResults.(FilterID).URef                  = URefCombined;
        ProcessResults.(FilterID).Weighted.URef         = URefVariation';
        ProcessResults.(FilterID).Weighted.(x3Name)     = x3Variation;
        ProcessResults.(FilterID).Lifetime.(x3Name)     = x3Variation;
    case 4       
        x3Combined      = NaN(1,nFilters);
        x4Combined      = NaN(1,nFilters);        
        OtherIdx        = find(~ismember(1:4,[URefIdx,SeedIdx]));
        x3Idx           = OtherIdx(1);
        x3Name          = PreProcessingVariation{x3Idx,1};
        x3Variation     = PreProcessingVariation{x3Idx,2};        
        nx3             = length(x3Variation);        
        x4Idx           = OtherIdx(2);
        x4Name          = PreProcessingVariation{x4Idx,1};
        x4Variation     = PreProcessingVariation{x4Idx,2};  
        nx4             = length(x4Variation);  
        CounterC        = 0;
        CounterW        = 0;
        x3Weighted      = NaN(1,nx3*nx4);
        x4Weighted      = NaN(1,nx3*nx4);       
        for ix4 = 1:nx4
            for ix3 = 1:nx3
                CounterW                    = CounterW + 1;
                x3Weighted(CounterW)        = x3Variation(ix3);  
                x4Weighted(CounterW)        = x4Variation(ix4);
                for iURef = 1:nURefs
                    CounterC                = CounterC + 1;
                    GoodData(:,CounterC)    = Permutation(:,x4Idx)==ix4 & Permutation(:,x3Idx)==ix3 & Permutation(:,URefIdx)==iURef;
                    x4Combined(CounterC)    = x4Variation(ix4);
                    x3Combined(CounterC)    = x3Variation(ix3);                    
                    URefCombined(CounterC)  = URefVariation(iURef);
                end
            end
        end
        % output
        ProcessResults.(FilterID).nVariation            = nVariation;
        ProcessResults.(FilterID).Matrix                = GoodData;
        ProcessResults.(FilterID).(x3Name)              = x3Combined; 
        ProcessResults.(FilterID).(x4Name)              = x4Combined;
        ProcessResults.(FilterID).URef                  = URefCombined;
        ProcessResults.(FilterID).Weighted.URef         = URefVariation';
        ProcessResults.(FilterID).Weighted.(x3Name)     = x3Weighted;
        ProcessResults.(FilterID).Weighted.(x4Name)     = x4Weighted;
        ProcessResults.(FilterID).Lifetime.(x3Name)     = x3Variation';
        ProcessResults.(FilterID).Lifetime.(x4Name)     = x4Variation;
        ProcessResults.(FilterID).Lifetime.nx3          = length(x3Variation);
        ProcessResults.(FilterID).Lifetime.nx4          = length(x4Variation);       
     
    otherwise
        error('Filters only defined up to 4th dimension by now. Extend if needed.')
end


end

