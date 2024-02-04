function [SimulationNames,DataFiles] = PreProcessingSimulations(SimulationFolder,PreProcessingVariation,InputFiles,Modifications,Tool)

% inputs
arguments
    SimulationFolder        char       
    PreProcessingVariation  (:,3) cell
    InputFiles              (:,2) cell
    Modifications           (:,4) cell
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
    
    % generate files only if simulation does not exist already  
    if ~exist(ResultFile,'file')        
        % make a copy of Modifications
        ThisModifications = Modifications;
        % replace function handles with string
        nModification = size(Modifications,1);
        for iModification = 1:nModification
            if isa(Modifications{iModification,4},'function_handle')
                ThisFunction = Modifications{iModification,4};
                ThisModifications{iModification,4} = ThisFunction(VariationValues);
            end
        end        
        % replace <SimulationName>
        ThisModifications       = replace(ThisModifications,'<SimulationName>',SimulationName);
        ThisInputFiles          = replace(InputFiles,'<SimulationName>',SimulationName);
        % adjust InputFiles
        AdjustInputFiles(ThisInputFiles,ThisModifications,SimulationFolder)
        % update counter and store SimulationName        
        i_Simulation                    = i_Simulation + 1;
        SimulationNames{i_Simulation,1} = SimulationName;
        % display
        fprintf('Writing files for simulation %s (%d)\n',SimulationNames{i_Simulation},i_Simulation); 
    end
end

end