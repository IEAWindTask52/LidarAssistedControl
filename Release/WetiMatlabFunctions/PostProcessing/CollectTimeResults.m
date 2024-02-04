function TimeResults = CollectTimeResults(DataFiles,PostProcessingConfig,options)

% inputs
arguments
    DataFiles               cell
    PostProcessingConfig    struct
    options.nCore           {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing
end

% get dimensions
nDataFiles  = size(DataFiles,1);
TimeResults = cell(nDataFiles,1);

% loop over data files
parfor (iDataFile = 1:nDataFiles,options.nCore) 
    ThisDataFile    = DataFiles{iDataFile};
    [~,~,EXT]       = fileparts(ThisDataFile);
    TSC             = []; % to avoid warning of "Uninitialized Temporaries"
    switch EXT
        case '.outb'
            TSC     = ReadFASTbinaryIntoTSC(ThisDataFile);    
        case '.res'
            TSC     = ReadFlex5IntoTSC(ThisDataFile);
        case '.mat'            
            Dummy   = load(ThisDataFile,'TSC');
            TSC     = Dummy.TSC
        otherwise
            error('Currently %s files are not supported!',EXT)
    end
    % keep only some channels, if requested
    if isfield(PostProcessingConfig,'DataChannels')&&~isempty(PostProcessingConfig.DataChannels)
        Channels  	= PostProcessingConfig.DataChannels;
        TsNames     = gettimeseriesnames(TSC);
        RemoveIdx   = find(~ismember(TsNames,Channels));
        for iTsName = RemoveIdx
            TSC     = removets(TSC,TsNames(iTsName));
        end 
    end
    % load into cell
    TimeResults{iDataFile} = TSC;
end

end