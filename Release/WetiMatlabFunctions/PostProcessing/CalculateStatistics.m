function Statistics = CalculateStatistics(TimeResults,PostProcessingConfig,options)

% inputs
arguments
    TimeResults             cell
    PostProcessingConfig    struct 
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% init
Statistics                      = table;

% return, if not defined or empty
if ~isfield(PostProcessingConfig,'CalculateStatistics')||isempty(PostProcessingConfig.CalculateStatistics)
    return
end

% get dimensions
nStatistics     = size(PostProcessingConfig.CalculateStatistics,1);
StatisticsIDs	= PostProcessingConfig.CalculateStatistics(:,1);
Functions  	    = PostProcessingConfig.CalculateStatistics(:,2);
ChannelCells    = PostProcessingConfig.CalculateStatistics(:,3);
nDataFiles      = size(TimeResults,1);

% loop over Statistics
for iStatistic = 1:nStatistics
    % extract
    ThisStatisticsID            = StatisticsIDs{iStatistic};
    ThisFunction                = Functions{iStatistic};
    ThisChannelCell             = ChannelCells{iStatistic};
    nChannels                   = size(ThisChannelCell,1);

    % loop over Channels
    for iChannel = 1:nChannels
        % define Variable name etc.
        ThisChannel             = ThisChannelCell{iChannel};
        Variable                = [ThisStatisticsID,'_',ThisChannel];   
        Value                   = NaN(nDataFiles,1); % Allocation
        % display if requested
        if strcmp(options.Display,'on')                
            MyText      = "Processing " + Variable + "\n";
            fprintf(MyText);
        end 
        % loop over data
        for iDataFile = 1:nDataFiles % parfor is slower
            ThisTimeSeries      = TimeResults{iDataFile}.(ThisChannel);
            Data                = ThisTimeSeries.Data;
            Time                = ThisTimeSeries.Time;
            ThisValue           = ThisFunction(Data,Time);
            if isempty(ThisValue)
                ThisValue       = NaN;
            end
            Value(iDataFile)    = ThisValue;
        end
        Statistics.(Variable)   = Value;    
    end
end

% add RowNames
for iDataFile = 1:nDataFiles
    Statistics.Properties.RowNames{iDataFile} = TimeResults{iDataFile}.Name;
end

end