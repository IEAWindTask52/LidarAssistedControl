function TSC = ReadFamosDataIntoTSC(FileName,Channels)
% Reads an imc FAMOS file into a tscollection.
% Optional input "Channels" (cell/string array of channel names) limits the
% data which is read from the file and added to the tscollection. The
% common time base is still derived from the meta data of all channels in
% the file, so the result is identical to reading all channels and removing
% the not requested ones afterwards.

if nargin < 2
    Channels = {};
end

% read in data  and get time
Channel             = importfamos(FileName,Channels);
nt                  = min(cat(1,Channel.length)); % lowest number
Idx                 = find(cat(1,Channel.length)==nt,1,'first');
dt                  = str2double(Channel(Idx).dt(1:end-1));
time                = (0:nt-1)'*dt;

% create a tscollection object
TSC                     = tscollection(time);
TSC.Name                = FileName;             % set name

% loop over the channels which have been read
LoadedChannel       = find([Channel.loaded]);
TsCell              = cell(1,numel(LoadedChannel));
for iLoaded = 1:numel(LoadedChannel)
    iChannel            = LoadedChannel(iLoaded);
    % create time series
    TS                  = timeseries(Channel(iChannel).name);
    ThisLength          = Channel(iChannel).length;
    TS.Time             = time;
    if  ThisLength == nt
        TS.Data         = Channel(iChannel).data;
    else % interpolation on lowest resolution
        ThisDt          = str2double(Channel(iChannel).dt(1:end-1));
        ThisTime        = (0:ThisLength-1)'*ThisDt;
        ThisData        = Channel(iChannel).data;
        TS.Data         = interp1(ThisTime,ThisData,time);
    end
    TS.UserData.Comment = Channel(iChannel).comment;
    TS.DataInfo.Units   = Channel(iChannel).yUnit;
    % collect for one single addts call (much faster than adding one by one)
    TsCell{iLoaded}     = TS;
end
if ~isempty(TsCell)
    TSC = TSC.addts(TsCell);
end

% add more time information at the end to avoid to add them for each data
TSC.TimeInfo.StartDate  = Channel(Idx).t0;
TSC.TimeInfo.Units      = 'seconds';

end
