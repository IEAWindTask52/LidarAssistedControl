function TimeResults = ApplyCorrectLosTiming(TimeResults,Channels,LOSchannel,NewChannels,options)
% Corrects LOS signal from NL200: at some times the step in the LOS signal
% seems to be earlier or later than the change in the RWS signals. Here,
% only RWS0 is used to detect this issue and only +/- 1 step is corrected.
% More can be implemented if warning ever comes up. 
% Timestamp signal cannot be used, since it sometimes changes with LOS and
% sometimes with RWS0.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults                 cell
    Channels                    char
    LOSchannel                  char = {'LOS'} % LOS by default (MM92) Replace e.g. by 'SoWETI_1_LOS'
    NewChannels                 char = [] % override by default, use [Channel,'c'] to store as a new channel
    options.Display             char {mustBeMember(options.Display,{'off','on'})} = 'off' 
    options.Tolerance           double = 0.005 % RWS only has two digits
end

% get dimensions
nDataFiles          = size(TimeResults,1);
nChannels           = size(Channels,1);

% init display text
MyText              = "";

% loop over files
for iDataFile = 1:nDataFiles % parfor slower than for loop
    % extract signals
    Time            = TimeResults{iDataFile}.(LOSchannel).Time;
    LOS             = TimeResults{iDataFile}.(LOSchannel).Data;

    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Correcting channel " + Channels(1,:) + " to " + Channels(nChannels,:) +": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end

    % loop over channel
    for iChannel = 1:nChannels

        % extract signals
        Data            = TimeResults{iDataFile}.(Channels(iChannel,:)).Data;

        % correction
        Datac           = CorrectLosTiming(Time,LOS,Data,Tolerance=options.Tolerance);

        % add new channel or overwriting data
        if ~isempty(NewChannels)
            NewTimeSeries                   = timeseries(Datac,Time,'Name',NewChannels);
            NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(Channels(iChannel,:)).DataInfo.Units;
            NewTimeSeries.TimeInfo          = TimeResults{iDataFile}.TimeInfo; % inherit Time Info
            TimeResults{iDataFile}          = TimeResults{iDataFile}.addts(NewTimeSeries);
        else
            TimeResults{iDataFile}.(Channels(iChannel,:)).Data = Datac;
        end
    end
end
