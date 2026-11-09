function TimeResults = ApplyBGALdp(TimeResults,Parameter,BeamIdChannel,VlosChannel,NewChannel,InitialValue,options)
% Applies a simple LDP (similar to Molas) with Blade Ghost avoidance.
% Intented to reproduce Molas results.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults                 cell
    Parameter                   struct
    BeamIdChannel               char    
    VlosChannel                 char 
    NewChannel                  char = 'v_0L'
    InitialValue                (1,1) double = 0
    options.ErrorCodeIn         (1,1) {mustBeNumeric}               = 0
    options.ErrorCodeOut        (1,1) {mustBeNumeric}               = 0
    options.Display             char {mustBeMember(options.Display,{'off','on'})} = 'off'    
    options.ResetAfterEachFile  {mustBeNumericOrLogical}            = false
end

% clear buffer
clear BGALdp

% get dimensions
nDataFiles          = size(TimeResults,1);

% extract lidar parameters:
numberOfBeams       = Parameter.Lidar.numberOfBeams;            % [-]
toCenterlineAngle   = Parameter.Lidar.toCenterlineAngle;        % [rad]

% init display text
MyText              = "";

% loop over files
for iDataFile = 1:nDataFiles % parfor not possible because of initial value  
    
    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Applying BGA LDP to " + VlosChannel + ": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end 

    % extract signals
    Time        = TimeResults{iDataFile}.(VlosChannel).Time;
    v_LOS       = TimeResults{iDataFile}.(VlosChannel).Data;
    BeamID      = TimeResults{iDataFile}.(BeamIdChannel).Data;   

    % LDP
    v_0L        = BGALdp(Time,v_LOS,BeamID,InitialValue,numberOfBeams,toCenterlineAngle,...
                    ErrorCodeIn=options.ErrorCodeIn,ErrorCodeOut=options.ErrorCodeOut);
   
    % add new channel 
    NewTimeSeries           = timeseries(v_0L,Time,'Name',NewChannel);
    NewTimeSeries.DataInfo.Units    = 'm/s';
    NewTimeSeries.TimeInfo  = TimeResults{iDataFile}.TimeInfo; % inherit Time Info
    TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);

    if options.ResetAfterEachFile
        clear BGALdp
    end

end

end