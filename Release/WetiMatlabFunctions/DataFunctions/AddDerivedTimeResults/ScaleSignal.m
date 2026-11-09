function TimeResults = ScaleSignal(TimeResults,ThisChannel,Scale,NewChannel,options)
% Scales channel and either overwrites or adds new channel.
% Could be also realized with ApplyFunctionToOneChannel, but kept for
% convenicence. 
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults                 cell
    ThisChannel                 char
    Scale                       {mustBeNumeric}
    NewChannel                  char = ''
    options.Unit                char = ''
    options.DeleteFlag          logical = false
    options.Display             char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% get dimensions
nDataFiles      = size(TimeResults,1);

% init display text
MyText              = "";

% loop over all files
for iDataFile = 1:nDataFiles

    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Scaling " + NewChannel + ": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end

    % get data    
    Data        = TimeResults{iDataFile}.(ThisChannel).Data;
    NewData     = Data*Scale;

    % add new channel or overwrite old one
    if ~isempty(NewChannel)
        Time  = TimeResults{iDataFile}.(ThisChannel).Time;
        NewTimeSeries           = timeseries(NewData,Time,'Name',NewChannel);
        NewTimeSeries.TimeInfo  = TimeResults{iDataFile}.(ThisChannel).TimeInfo; % inherit TimeInfo        
        if ~isempty(options.Unit)
            NewTimeSeries.DataInfo.Units    = options.Unit;
        else
            NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(ThisChannel).DataInfo.Units; % inherit units 
        end
        TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);

        % delete channel if requested
        if options.DeleteFlag        
            TimeResults{iDataFile}  = removets(TimeResults{iDataFile},ThisChannel);
        end        
    else
        TimeResults{iDataFile}.(ThisChannel).Data = NewData;
        if ~isempty(options.Unit)
            TimeResults{iDataFile}.(ThisChannel).DataInfo.Units    = options.Unit;
        end        
    end   
end

end