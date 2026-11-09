function DataFiles = GetDataFilesFromPreProcessingVariation(SimulationFolder,PreProcessingVariation,Tool)

% inputs
arguments
    SimulationFolder        char       
    PreProcessingVariation  (:,3) cell
    Tool                    {mustBeMember(Tool,{'OpenFAST','Flex5'})} = 'OpenFAST'
end

[nVariation, nPermutation, Permutation] = CreatePermutationMatrix(PreProcessingVariation);
SimulationNames         = {};
DataFiles               = cell(nPermutation,1);
i_Simulation          	= 0; % init counter

for iPermutation = 1:nPermutation    
    % get VariationValues for this permutation
    VariationValues = NaN(nVariation,1);
    for iVariation  = 1:nVariation        
        VariationValues(iVariation)   = PreProcessingVariation{iVariation, 2}(Permutation(iPermutation, iVariation));
    end
    
    % Get SimulationName, ResultFile and store in DataFiles
    SimulationName          = GetSimulationName(PreProcessingVariation,VariationValues);
    switch Tool 
        case 'OpenFAST'
            ResultFile      = fullfile(SimulationFolder,[SimulationName,'.outb']);       
        case 'Flex5'
            ResultFile      = fullfile(SimulationFolder,[SimulationName,'.res']);
    end
    DataFiles{iPermutation} = ResultFile;   
end

end