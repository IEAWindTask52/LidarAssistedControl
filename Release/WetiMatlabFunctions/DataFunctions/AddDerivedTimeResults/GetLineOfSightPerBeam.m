function TimeResults = GetLineOfSightPerBeam(TimeResults,BeamIdChannel,VlosChannel,BeamID,NewChannel,options)
% Gets the line-of-sight wind speed from a single beam.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults         cell
    BeamIdChannel       char    
    VlosChannel         char    
    BeamID              {mustBeInteger}
    NewChannel          char = [VlosChannel,'_',num2str(BeamID)]
    options.Tolerance   double = 0.1
    options.Display     char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% get dimensions
nDataFiles          = size(TimeResults,1);

% init display text
MyText              = "";

% loop over all files
for iDataFile = 1:nDataFiles % parfor much slower
    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Extracting " + VlosChannel + " from beam " + BeamID + ": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end     
    % identify data for this BeamID
    Idx         = abs(TimeResults{iDataFile}.(BeamIdChannel).Data - BeamID)<options.Tolerance;
    Time        = TimeResults{iDataFile}.(VlosChannel).Time;
    Data        = nan(size(Time));
    Data(Idx)   = TimeResults{iDataFile}.(VlosChannel).Data(Idx);
    % add new channel            
    NewTimeSeries           = timeseries(Data,Time,'Name',NewChannel);
    NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(VlosChannel).DataInfo.Units; % inherit units 
    NewTimeSeries.TimeInfo          = TimeResults{iDataFile}.TimeInfo; % inherit Time Info
    TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);
end

end