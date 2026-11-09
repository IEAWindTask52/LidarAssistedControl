function ProcessingSlowSimulations(SimulationFolder,SimulationNames,Tool,options)

% inputs
arguments
    SimulationFolder    char       
    SimulationNames     cell
    Tool                {mustBeMember(Tool,{'Slow','RunSlowWithServos'})} = 'Slow'
    options.nCore       {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing
end

% get dimensions and allocation
nSimulations    = size(SimulationNames,1);
result          = cell(1,nSimulations);
status          = NaN(1,nSimulations);

% move to SimulationFolder
MainFolder      = pwd;
cd(SimulationFolder)

% run simulations
switch Tool
    case 'Slow'
%         for iSimulation    = 1:nSimulations
        parfor (iSimulation    = 1:nSimulations,options.nCore)
            SimulationName  = SimulationNames{iSimulation};
            fprintf('Running simulation %s (%d/%d)\n',SimulationName,iSimulation,nSimulations);        
            % load InitFile needed
            InitFile        = [SimulationName,'_init.mat'];
            SLOW            = load(InitFile,'Config','DllFile','InFile','ServoDyn','Disturbance','Parameter');
            % run
            SlowResultFile  = [SimulationName,'.mat'];
            RunSlow(SLOW.Config,SlowResultFile,SLOW.DllFile,SLOW.InFile,SLOW.ServoDyn,SLOW.Disturbance,SLOW.Parameter); % TODO: get DllFile,InFile,ServoDyn into Config
        end
end

% back to main folder
cd(MainFolder)

end