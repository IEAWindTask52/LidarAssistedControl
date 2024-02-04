function [status,result] = ProcessingSimulations(SimulationFolder,SimulationNames,ExeFile,Tool,options)

% inputs
arguments
    SimulationFolder    char       
    SimulationNames     cell
    ExeFile             char
    Tool                {mustBeMember(Tool,{'OpenFAST','Flex5'})} = 'OpenFAST'
    options.nCore       {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing
end

% get dimensions and allocation
n_Simulation    = size(SimulationNames,1);
result          = cell(1,n_Simulation);
status          = NaN(1,n_Simulation);

% move to SimulationFolder
MainFolder      = pwd;
cd(SimulationFolder)

% run simulations
switch Tool
    case 'Flex5'
        for i_Simulation    = 1:n_Simulation   
            SimulationName  = SimulationNames{i_Simulation};
            fprintf('Running simulation %s (%d/%d)\n',SimulationName,i_Simulation,n_Simulation);        
            [status(i_Simulation),result{i_Simulation}] = ...
                system([ExeFile,...
                ' ',SimulationName,'.inf',...
                ' ',SimulationName,'.log',...
                ' ',SimulationName,'.res']);
        end
    case 'OpenFAST'
        parfor (i_Simulation    = 1:n_Simulation,options.nCore)    
            SimulationName  = SimulationNames{i_Simulation};
            fprintf('Running simulation %s (%d/%d)\n',SimulationName,i_Simulation,n_Simulation);        
            [status(i_Simulation),result{i_Simulation}] = ...
                system([ExeFile,...
                ' ',SimulationName,'.fst']);
        end
end

% back to main folder
cd(MainFolder)

end