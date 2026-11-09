%RES2Y  Import of binary FLEX5 time series to MATLAB
%    The size of the resulting data set is by default restricted to 7.5e6 elements.
%    If the file contains more data, only the first sensors are read. In this
%    case data can be read by specifying the sensors in the argument
%    Sensor2Read
%
%    Syntax:   [y,Hd] = res2y(filename,Sensor2Read,MaxSize)
% 
%    Inputs:   filename     Filename. If passed without argument, a getfile GUI is started
%              Sensor2Read  a) Cell array of sensor names
%                          b) Vector of Sensor numbers
%                          Sensor2Read = 0: Only the header is read
%              MaxSize    Maximum number of read data (Default: 5e6)
%
%    Outputs:  y          Data structure. See YSTRUCT for format specification
%              Hd         Further file information
%
%    Examples: y = res2y;                     Read all data from file via file select GUI
%              y = res2y('file.res')          Read all data from file.res
%              y = res2y('file.res',[11 111]) Read sensors 11 and 111
%              y = res2y('file.res',{'Teta2';'Teta3'}) Read sensors 'Teta2' and 'Teta3'
%              [y,Hd] = res2y('file.res',0)   Read only header
%

function [y,Hd] = res2y(FileName,Sensor2Read,MaxSize)

READDATA = 1;

if nargin < 1, FileName = []; end
if nargin < 2, Sensor2Read  = []; end
if nargin < 3, MaxSize = 20e6; end

if isempty(FileName)
    [FileName,pathname] = uigetfile({ ...
        '*.int';'*.res';'*.byt';'*.wrd'; ...
        '*.res1';'*.int1';'*.byt1';'*.wrd1'; ...
        '*.res2';'*.int2';'*.byt2';'*.wrd2'}, ...
        'Select a FLEX file');
    FileName = [pathname,FileName];
end

if exist(FileName) ~= 2, error('file not found %s', FileName); end

% open file
fid   = fopen(FileName , 'rb');

% get file size
fseek(fid, 0, 'eof');
FileSize = ftell(fid);
fseek(fid, 0, 'bof');

% BEGIN read file header

Hd.ERROR    = 0;
Hd.EoH      = 0;
Hd.fSLst    = fread(fid,1,'int32');
Hd.TimeVec  = fread(fid,6,'int32');
if Hd.TimeVec(1) < 50
    % only last 2 digits of year stored
    Hd.TimeVec(1) = Hd.TimeVec(1) + 2000;
elseif Hd.TimeVec(1) < 100
    % before 2000
    Hd.TimeVec(1) = Hd.TimeVec(1) + 1900;
end
Hd.TimeStr  = datestr(datenum(Hd.TimeVec'),'yyyy-mm-dd HH:MM:SS');
Hd.EoH      = Hd.EoH + 7*4;

L_TXT = 40;
if Hd.fSLst < 0
    L_TXT = fread(fid,1,'int32');
    Hd.EoH = Hd.EoH + 1*4;
end;
Hd.TXTstr = char(fread(fid,L_TXT,'char'))';
Hd.TXTstr = strtok(Hd.TXTstr,sprintf('\n'));    % Remove additional info from header

Hd.EoH = Hd.EoH + L_TXT;

RL    = fread(fid,1,'int32');
RL    = fread(fid,1,'int32');

NumSensor  = fread(fid , 1 ,'int32');           % Number of sensors in file
Hd.fOrder = 'ByTimeStep';
if NumSensor < 0
    Hd.fOrder= 'BySensor';
    NumSensor = -NumSensor;
end;

Hd.SensorNumber = fread(fid,NumSensor,'int32');

Hd.NumTimeStep    = fread(fid , 1 ,'int32'); %..with "native" Flex files in ByTmeStp order  NT  is always 0 , as the final nb. of time steps in the file is not fixed during run time

Hd.EoH = Hd.EoH + (NumSensor + 4)*4;

% READING SENSOR NAME,UNIT,DESCRIPTION IF EXISTING
if Hd.fSLst < 0
    L_NUD           = fread(fid,1,'int32');
    Hd.EoH          = Hd.EoH + L_NUD + 4*4;
    Hd.name         = ReadHeaderField(fid);
    Hd.unit         = ReadHeaderField(fid);
    Hd.comment      = ReadHeaderField(fid);
    if (nargin < 2) | isempty(Sensor2Read)
        Sensor2Read  = Hd.SensorNumber;
        Index2Read   = [1:length(Hd.SensorNumber)]';
    end
    if ischar(Sensor2Read), Sensor2Read = {Sensor2Read}; end
    if iscell(Sensor2Read)
        if isempty(Sensor2Read), READDATA = 0; end
        [SENSOREXIST,Index2Read] = ismember(Sensor2Read,Hd.name);
        Index2Read = Index2Read(SENSOREXIST);
        SensorNo2Read = Hd.SensorNumber(Index2Read);
    else
        if Sensor2Read == 0, READDATA = 0; end
        Sensor2Read = reshape(Sensor2Read,length(Sensor2Read),1);
        [SENSOREXIST,Index2Read] = ismember(Sensor2Read,Hd.SensorNumber);
        Index2Read = Index2Read(SENSOREXIST);
        SensorNo2Read = Sensor2Read(SENSOREXIST);
    end
    NumSensor2Read = length(Index2Read);
    y.name      = strtrim(Hd.name(Index2Read));
    y.unit      = Hd.unit(Index2Read);
    y.comment   = Hd.comment(Index2Read);
    y.description = ['Flex5 simulation, filename: ',FileName, ' ',Hd.TXTstr];
    y.date      = Hd.TimeStr;
else
    fclose(fid); error('Unsupported (probably old) file format')
end

% FORMAT SWITCH:  ->  END OF COMMON HEADER SECTION

Hd.RLSgl  = fread(fid,1,'float32');
fseek(fid,ftell(fid)-4,'bof');  % DON'T COUNT BACKWARD JUMP TO Hd.EoH !!  ONLY COUNT 1x  !!
Hd.RL     = fread(fid , 1 ,'int32');

if (Hd.RLSgl == 12) | (Hd.RL == 12)
    Hd.FileFormat = 'RES';
else
    switch Hd.RL
        case 1
            Hd.FileFormat = 'BYT';
        case 3
            Hd.FileFormat = 'WRD';
        case 4
            Hd.FileFormat = 'BXT'; % BXT FORMAT CURRENTLY NOT SUPPORTED
        otherwise
            Hd.FileFormat = 'INT';
    end
end

switch Hd.FileFormat
    case 'RES'
        Hd.DataFormat = 'float32';
        Hd.T0  = fread(fid,1,'float32');
        switch Hd.fOrder
            case 'BySensor'
                Hd.dT       = fread(fid , 1 , 'float32');
                Hd.EoH      = Hd.EoH + 3*4; % with RES format in BySensor order  Hd.RL(=12),T0,DT  belong to HEADER !!
                Hd.T1       = Hd.T0 + Hd.dT .* (Hd.NumTimeStep-1);
                DatSetSize  = Hd.NumTimeStep .* NumSensor * 4;
            case 'ByTimeStep'
                fseek(fid , FileSize-(NumSensor+4)*4 , 'bof');
                Hd.T1       = fread(fid , 1 ,'float32');
                Hd.NumTimeStep       = fix( (FileSize-Hd.EoH) ./ ((NumSensor+5)*4) );
                Hd.dT       = (Hd.T1 - Hd.T0) ./ (Hd.NumTimeStep-1);
                DatSetSize  = Hd.NumTimeStep .* ((NumSensor+5)*4);
        end;
    case 'BYT'
        Hd.DataFormat = 'uint8';
        Hd.T0       = fread(fid,1,'float32');
        Hd.dT       = fread(fid,1,'float32');
        Hd.EoH      = Hd.EoH + 3*4; % with BYT format  Hd.RL(=12),T0,DT  belong to HEADER !!
        Hd.Offset   = fread(fid,NumSensor,'float32');
        Hd.Gain     = fread(fid,NumSensor,'float32');
        Hd.EoH      = Hd.EoH + 2*NumSensor*4;
        Hd.NumTimeStep       = fix((FileSize-Hd.EoH) ./ (NumSensor*1));
        Hd.T1       = Hd.T0 + Hd.dT .* (Hd.NumTimeStep-1);
        DatSetSize  = Hd.NumTimeStep .* NumSensor * 1;
    case 'WRD'
        Hd.DataFormat = 'uint16';
        Hd.T0       = fread(fid,1,'float32');
        Hd.dT       = fread(fid,1,'float32');
        Hd.EoH      = Hd.EoH + 3*4; % with WRD format  Hd.RL(=12),T0,DT  belong to HEADER !!
        Hd.Offset   = fread(fid,NumSensor,'float32');
        Hd.Gain     = fread(fid,NumSensor,'float32');
        Hd.EoH      = Hd.EoH + 2*NumSensor*4;
        Hd.NumTimeStep       = fix( (FileSize-Hd.EoH) ./ (NumSensor*2) );
        Hd.T1       = Hd.T0 + Hd.dT .* (Hd.NumTimeStep-1);
        DatSetSize  = Hd.NumTimeStep .* NumSensor * 2;
    case 'INT'
        Hd.DataFormat = 'int16';
        Hd.T0       = fread(fid,1,'float32');
        Hd.dT       = fread(fid,1,'float32');
        Hd.EoH      = Hd.EoH + 3*4; % with INT format  Hd.RL(=12),T0,DT  belong to HEADER !!
        Hd.Gain     = fread(fid,NumSensor,'float32');
        Hd.EoH      = Hd.EoH + 1*NumSensor*4;
        Hd.NumTimeStep       = fix( (FileSize-Hd.EoH) ./ (NumSensor*2) );
        Hd.T1       = Hd.T0 + Hd.dT .* (Hd.NumTimeStep-1);
        DatSetSize  = Hd.NumTimeStep .* NumSensor * 2;
end

Hd.sensorlist = GetSensorList(Hd.SensorNumber,Hd.name,Hd.unit,Hd.comment);

DatSize = FileSize - Hd.EoH;
Delta = DatSize - DatSetSize;

if Delta ~= 0
    Hd.ERROR = -2; % ERROR reading file header: unexpected end of file?
    READDATA = 0;
    warning('Unexpected end of file. No data will be read. Filename: %s',FileName)
end

% END read file header

if ~READDATA, fclose(fid); return; end        % Only header is read

NumData = Hd.NumTimeStep;

DataSize = NumData * NumSensor;
if DataSize > MaxSize, LARGEFILE = 1; else, LARGEFILE = 0; end
if LARGEFILE
    NumSensorMax = floor(MaxSize/NumData);
    if NumSensor2Read > NumSensorMax
        NumSensor2Read  = NumSensorMax;
        Index2Read      = Index2Read(1:NumSensorMax);
        y.name          = y.name(Index2Read);
        y.unit          = y.unit(Index2Read);
        y.comment       = y.comment(Index2Read);
        warning(['Data exceeds specified limit of ',num2str(MaxSize),' elements: Only the first ',num2str(NumSensorMax),' sensors will be read.'])
    end
end

RawData = zeros(NumData,length(Index2Read));

if LARGEFILE, disp('Reading data ...'), end
switch Hd.fOrder
    case 'ByTimeStep'
        BlockLength = floor(MaxSize/(2*NumSensor));
        fseek(fid, Hd.EoH , 'bof'); % seek to beginning of data
        switch Hd.FileFormat
            case 'RES'
                if LARGEFILE
                    for ibg = 1:BlockLength:NumData
                        iend = min(ibg+BlockLength-1,NumData);
                        DataBlock = fread(fid,[NumSensor+5,iend-ibg+1],Hd.DataFormat)';
                        RawData(ibg:iend,:) = DataBlock(:,Index2Read+4);
                    end
                else
                    RawData = fread(fid,[NumSensor+5,NumData],Hd.DataFormat)';
                    RawData = RawData(:,Index2Read+4);
                end
            otherwise
                if LARGEFILE
                    for ibg = 1:BlockLength:NumData
                        iend = min(ibg+BlockLength-1,NumData);
                        DataBlock = fread(fid,[NumSensor,iend-ibg+1],Hd.DataFormat)';
                        RawData(ibg:iend,:) = DataBlock(:,Index2Read);
                    end
                else
                    RawData = fread(fid,[NumSensor,NumData],Hd.DataFormat)';
                    RawData = RawData(:,Index2Read);
                end
        end
    case 'BySensor'
        BlockLength = floor(MaxSize/(2*NumData));
        if LARGEFILE
%            warning('Reading of large file by sensor is not tested')
            for ibg = 1:BlockLength:NumSensor
                iend        = min(ibg+BlockLength-1,NumSensor);
                DataBlock   = fread(fid,[NumData,iend-ibg+1],Hd.DataFormat);
                IndexInBlock = Index2Read((Index2Read >= ibg)&(Index2Read <= iend));
                [dum,IndexInData] = ismember(IndexInBlock,Index2Read);
%                [IndexInBlock,IndexInData]
                if ~isempty(IndexInBlock)
                    RawData(:,IndexInData) = DataBlock(:,IndexInBlock-ibg+1);
                end
            end
        else
            RawData = fread(fid,[NumData,NumSensor],Hd.DataFormat);
            RawData = RawData(:,Index2Read);
        end
end

% close file
fclose(fid);

switch Hd.FileFormat
    case 'RES'
        y.data = RawData;
    case {'WRD','BYT'}
        if LARGEFILE, disp('Scaling data ...'), end
        Hd.Gain     = Hd.Gain(Index2Read);
        Hd.Offset   = Hd.Offset(Index2Read);
        y.data = (ones(NumData,1) * Hd.Gain') .* RawData + ...
            (ones(NumData,1) * Hd.Offset');
    case 'INT'
        if LARGEFILE, disp('Scaling data ...'), end
        Hd.Gain     = Hd.Gain(Index2Read);
        y.data = (ones(NumData,1) * Hd.Gain') .* RawData;
end
y.time = Hd.T0  +  Hd.dT * [0:NumData-1]';

for i= 1:length(y.name)
    if strncmp(y.name{i}, 'StyrV', 5)
        y.name{i}= ['<' strtok(y.comment{i}, ',') '>'];
        if strcmp(y.name{i}, '<VWindMeas>')
            y.name{i}= '<VwindMeas>';
        end
        if strcmp(y.name{i}, '<VWindEst>')
            y.name{i}= '<VwindEst>';
        end
        if strcmp(y.name{i}, '<ULevel>')
            y.name{i}= '<Ulevel>';
        end
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function Field = ReadHeaderField(fid)

Delimiter = char(9);
FieldLength             = fread(fid,1,'int32');
ASCIIField              = fread(fid,FieldLength,'char');
ASCIIField(~ASCIIField) = Delimiter;
FieldStr                = char(ASCIIField');
Field                   = strread(FieldStr,'%s','delimiter',Delimiter);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function sensorlist = GetSensorList(no,name,unit,comment)

sensorlist = cell(length(name)+2,1);

sensorlist{1} = tabline({'No','Flx No','Label','Unit','Comment'},[5 7 10 8 40]);
sensorlist{2} = '--------------------------------------------------------------------';
for j=1:length(name)
    sensorlist{j+2} = tabline({j,no(j),name{j},unit{j},comment{j}},[5 7 10 8 40]);
end

