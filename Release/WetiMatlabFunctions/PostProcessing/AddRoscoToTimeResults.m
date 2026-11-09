function TimeResults = AddRoscoToTimeResults(TimeResults,RoscoFiles)

% internal variables
Tolerance   = 1e-3; % [s]

% get dimensions
nRoscoFile  = size(RoscoFiles,1);

% loop over data files
for iRoscoFile = 1:nRoscoFile
    % load rosco file
    ThisRoscoFile   = RoscoFiles{iRoscoFile};
    ROSCO           = ReadRoscoIntoTSC(ThisRoscoFile);    

    % set log time to TimeResults time
    if length(TimeResults{iRoscoFile}.time)==length(ROSCO.time) && sqrt(mean((TimeResults{iRoscoFile}.time-ROSCO.time).^2))<Tolerance % rmse<Tolerance
        ROSCO.time = TimeResults{iRoscoFile}.time;
    else
        error(['Time vectors of TimeResults and LogFile ',num2str(iRoscoFile),' do not match'])
    end

    % add channels
    ChannelNames    = gettimeseriesnames(ROSCO);
    nLogChannel     = length(ChannelNames);
    for iLogChannel = 1:nLogChannel
        TimeResults{iRoscoFile} = TimeResults{iRoscoFile}.addts(ROSCO.(ChannelNames(iLogChannel)));
    end

end