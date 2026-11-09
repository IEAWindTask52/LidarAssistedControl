function TimeResults = ApplyFunctionToTwoChannels(TimeResults,FirstChannel,SecondChannel,Function,NewChannel,options)
% Combines two channels and adds new channel.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults                 cell
    FirstChannel                char
    SecondChannel               char
    Function                    function_handle
    NewChannel                  char
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
    Time        = TimeResults{iDataFile}.Time;
    Data1       = TimeResults{iDataFile}.(FirstChannel).Data;    
    Data2       = TimeResults{iDataFile}.(SecondChannel).Data;   
    NewData     = Function(Time,Data1,Data2);

    % add new channel 
    NewTimeSeries           = timeseries(NewData,Time,'Name',NewChannel);
    NewTimeSeries.TimeInfo  = TimeResults{iDataFile}.TimeInfo; % inherit TimeInfo from first channel
    if ~isempty(options.Unit)
        NewTimeSeries.DataInfo.Units    = options.Unit;
    else
        NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(FirstChannel).DataInfo.Units; % inherit unit from first channel
    end
    TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries); 

    % delete channels if requested
    if options.DeleteFlag        
        TimeResults{iDataFile}  = removets(TimeResults{iDataFile},{FirstChannel,SecondChannel});
    end
  
end

end