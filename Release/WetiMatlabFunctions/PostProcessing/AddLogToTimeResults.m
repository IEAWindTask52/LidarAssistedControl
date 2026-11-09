function TimeResults = AddLogToTimeResults(TimeResults,LogFiles,PostProcessingConfig,options)

% inputs
arguments
    TimeResults             cell
    LogFiles                cell
    PostProcessingConfig    struct = struct()
    options.nCore           {mustBeInteger} = maxNumCompThreads % set option to 0 for no parallel processing
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'
    options.TimeOffset      double = 0
    options.method          char {mustBeMember(options.method,{'linear','zoh'})} = 'zoh'
end

% internal variables
Tolerance   = 1e-3; % [s]

% get dimensions
nLogFile    = size(LogFiles,1);

% init display text
MyText              = "";

% loop over data files
for iLogFile = 1:nLogFile

    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Reading log files: " + iLogFile + "/" + nLogFile;
        fprintf(MyText);
        if iLogFile==nLogFile % new line
            fprintf('\n');
        end
    end        

    % load log file
    ThisDataFile    = LogFiles{iLogFile};
    [~,~,EXT]       = fileparts(ThisDataFile);
    switch EXT
        case {'.csv','.log'}
            LOG     = ReadLogIntoTSC(ThisDataFile,PostProcessingConfig.Log.Units); 
        case '.outb'
            LOG     = ReadFASTbinaryIntoTSC(ThisDataFile,PostProcessingConfig.Log.Units);
        otherwise
            error('Only *.csv, *.log, and *.outb are supported.')
    end       

    % set log time to TimeResults time
    if length(TimeResults{iLogFile}.time)==length(LOG.time) && sqrt(mean((TimeResults{iLogFile}.time-LOG.time).^2))<Tolerance % rmse<Tolerance
        LOG.time    = TimeResults{iLogFile}.time;
    else
        LOG         = resample(LOG,TimeResults{iLogFile}.time+options.TimeOffset,options.method);
        LOG.time    = TimeResults{iLogFile}.time;
        if strcmp(options.Display,'on') 
            warning(['Time vectors of TimeResults and LogFile ',num2str(iLogFile),' did not match and have been resampled.'])
        end
    end

    % add channels
    ChannelNames    = gettimeseriesnames(LOG);
    nLogChannel     = length(ChannelNames);
    for iLogChannel = 1:nLogChannel
        TimeResults{iLogFile} = TimeResults{iLogFile}.addts(LOG.(ChannelNames(iLogChannel)));
    end

end