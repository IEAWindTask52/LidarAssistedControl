function TimeResults = CollectTimeResults(DataFiles,PostProcessingConfig,options)

% inputs
arguments
    DataFiles               cell
    PostProcessingConfig    struct = struct()
    options.nCore           {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% get dimensions
nDataFiles  = size(DataFiles,1);
TimeResults = cell(nDataFiles,1);

% requested channels: passed to the readers which support a channel
% selection, so that only these channels are read from the file
% (broadcast variable for the parfor loop)
if isfield(PostProcessingConfig,'DataChannels')&&~isempty(PostProcessingConfig.DataChannels)
    DataChannels    = PostProcessingConfig.DataChannels;
else
    DataChannels    = {};
end

% loop over data files
parfor (iDataFile = 1:nDataFiles,options.nCore)
    % display if requested
    if strcmp(options.Display,'on')                
        MyText      = "Reading data: " + iDataFile + "/" + nDataFiles + "\n";
        fprintf(MyText);
    end 
    % get data
    ThisDataFile    = DataFiles{iDataFile};
    [~,~,EXT]       = fileparts(ThisDataFile);
    TSC             = []; % to avoid warning of "Uninitialized Temporaries"
    switch EXT
        case '.prj'
            TSC     = ReadBladedIntoTSC(ThisDataFile);
        case '.dat'
            TSC     = ReadFamosDataIntoTSC(ThisDataFile,DataChannels);
        case '.outb'
            TSC     = ReadFASTbinaryIntoTSC(ThisDataFile);    
        case '.res'
            TSC     = ReadFlex5IntoTSC(ThisDataFile);
        case '.mat'
            switch PostProcessingConfig.CollectTimeResults.MAT.Type
                case 'TSC'
                    Dummy   = load(ThisDataFile,'TSC');
                    TSC     = Dummy.TSC;
                case 'Vector'
                    if isfield(PostProcessingConfig.CollectTimeResults.MAT,'Units')
                        Units   = PostProcessingConfig.CollectTimeResults.MAT.Units;
                    else
                        Units   = struct()
                    end
                    if isfield(PostProcessingConfig.CollectTimeResults.MAT,'dt')
                        dt      = PostProcessingConfig.CollectTimeResults.MAT.dt;
                    else
                        dt      = [];
                    end
                    TSC     = ReadMatIntoTSC(ThisDataFile,Units,dt=dt);                    
            end
        case '.csv'
            switch PostProcessingConfig.CollectTimeResults.CSV.Type
                case 'solis'
                    Table   = readtable(ThisDataFile);
                    Units   = PostProcessingConfig.CollectTimeResults.CSV.Units;
                    NewTime = PostProcessingConfig.CollectTimeResults.CSV.NewTime;
                    TSC     = Table2TSC(Table,NewTime,Units,Name=ThisDataFile)
            end  
        case '.CSV'
            switch PostProcessingConfig.CollectTimeResults.CSV.Type
                case 'ZX300'
                    opts = detectImportOptions(ThisDataFile);
                    opts = setvaropts(opts, 'TimeAndDate', ...
                        'InputFormat', 'dd/MM/yyyy HH:mm:ss');
                    Table               = readtable(ThisDataFile,opts);
                    % convert time
                    Table.TimeAndDate   = datenum(Table.TimeAndDate);
                    % keep numerical columns only
                    isDouble            = varfun(@(x) isa(x,'double'), Table, 'OutputFormat','uniform');
                    Table               = Table(:, isDouble);
                    TSC                 = Table2TSC(Table,timeChannel='TimeAndDate',Name=ThisDataFile)
            end
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