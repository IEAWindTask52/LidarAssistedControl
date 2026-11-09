function TT = ReadFamosDataIntoTimeTable(FileName)

% read in data  and get time
Channel             = importfamos(FileName);
nt                  = min(cat(1,Channel.length)); % lowest number
Idx                 = find(cat(1,Channel.length)==nt,1,'first');
dt                  = str2double(Channel(Idx).dt(1:end-1));
time                = [0:nt-1]'*dt;

% create a time table
TT                  = timetable();

% loop over channels
nChannel            = length(Channel);
for iChannel = 1:nChannel

    % data
    ThisLength          = Channel(iChannel).length;
    if  ThisLength == nt        
        Data            = Channel(iChannel).data;
    else % interpolation on lowest resolution
        ThisDt          = str2double(Channel(iChannel).dt(1:end-1));
        ThisTime        = [0:ThisLength-1]'*ThisDt;
        ThisData        = Channel(iChannel).data;
        Data            = interp1(ThisTime,ThisData,time);
    end
   
    % add to time table object 
    TT = addvars(TT,Data);

    % add units and name
    TT.Properties.VariableNames{iChannel} = Channel(iChannel).name;
    TT.Properties.VariableUnits{iChannel} = Channel(iChannel).yUnit;        
end

% add more time information at the end to avoid to add them for each data
TT.Time                 = seconds(time);
TT.Properties.StartTime = datetime(Channel(Idx).t0);

end