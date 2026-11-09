function TimeResults = ApplyFunctionToOneChannel(TimeResults,ThisChannel,Function,NewChannel,options)
% Scales channel and either overwrites or adds new channel.
% Scaling could be realized with ScaleSignal.
% Function for AddDerivedTimeResults.

% inputs
arguments   
    TimeResults                 cell
    ThisChannel                 char
    Function                    function_handle
    NewChannel                  char = ''
    options.Unit                char = '' 
    options.DeleteFlag          logical = false
    options.Display             char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% get dimensions
nDataFiles          = size(TimeResults,1);

% init display text
MyText              = "";

% loop over all files
for iDataFile = 1:nDataFiles

    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Generating " + NewChannel + ": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end

    % get data     
    Data        = TimeResults{iDataFile}.(ThisChannel).Data;
    Time        = TimeResults{iDataFile}.(ThisChannel).Time;
    NewData     = Function(Time,Data);

    % add new channel or overwrite old one
    if ~isempty(NewChannel)
        Time  = TimeResults{iDataFile}.(ThisChannel).Time;        
        NewTimeSeries           = timeseries(NewData,Time,'Name',NewChannel);
        NewTimeSeries.TimeInfo  = TimeResults{iDataFile}.(ThisChannel).TimeInfo; % inherit TimeInfo from first channel
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