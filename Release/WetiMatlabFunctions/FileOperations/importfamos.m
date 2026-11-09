function Channel=importfamos(FullRawData,Channels)
%__________________________________________________________________________
% The sequnce importfamos.m was produced to convert imc raw data(*.raw;
% *.dat) to matlab data. Here, struct is used to manage the channel
% information.
%
% For more information of FAMOS file format, please see the
% manufacturer's website: http://www.imc-berlin.de
%
% Corresponding to Data Structure in Matlab, the channels are stored
% as struct struct: Channel + name (channel name)
%                           + comment
%                           + data (channel Value)
%                           + length
%                           + yUnit
%                           + t0
%                           + dt(sampling period)
%                           + xUnit
%                           + loaded (true, if data was read from file)
%
% Optional input "Channels" (cell/string array of channel names) limits the
% binary read to the requested channels. The header of all channels is
% parsed in any case, so that the meta data (especially "length" and "dt",
% which define the common time base) stays complete. Channels which are not
% requested are returned with "loaded" = false and empty "data".
%
% Version history:
% Version 1.0 (2011.1.19); Current version only can deal with analog rawdata
% from imc devices. The digital and group function is ongoning and will be
% released in version 2.
% Version 1.1 (2013.12.31): In order to solve non-manually save data,
% regular pattern is introduced into sloving data structure without CR and
% LF.
% (only support data(Channel) from imc devices)
% Version 1.2 (2026.08.10): Speed up: only the header is read as text
% (instead of the complete file), the numeric fields are parsed with sscanf
% (instead of str2num), the trigger time is converted only once per unique
% time stamp and the binary data is read only for the requested channels.
%
%%-------------------------------------------------------------------------
% Author: Liang
% Danke.Liang@gmail.com
% Started on Dec.14, 2010
%__________________________________________________________________________

% channel selection
if nargin<2 || isempty(Channels)
    LoadAllChannels     = true;
    Channels            = {};
else
    LoadAllChannels     = false;
    Channels            = cellstr(Channels);
end

fid = fopen(FullRawData,'r');
if fid==-1
    disp('failed to read rawdata')
    return
end

CSstring='\|CS,\d,\D*\d+,\D*\d+,';

% read only the header: the key CS marks the begin of the binary data, so
% the file is read in growing chunks until this key is complete
ChunkSize   = 2^16;
InfoSegment = '';
CSend       = [];
while isempty(CSend)
    % '*uint8' instead of '*char': independent of the file encoding, so that
    % the character index of the key CS is also the byte offset of the data
    Chunk   = char(fread(fid,ChunkSize,'*uint8'))'; %#ok<FREAD>

    if isempty(Chunk)
        break
    end
    InfoSegment = [InfoSegment Chunk]; %#ok<AGROW>
    CSend       = regexp(InfoSegment,CSstring,'end','once');
    ChunkSize   = 2*ChunkSize;
end
if isempty(CSend)
    fclose(fid);
    error('importfamos:NoCSKey','Key CS not found in %s',FullRawData)
end
InfoSegment = InfoSegment(1,1:CSend);

Cbstr='\|Cb.*?;';
CGstr='\|CG,.*?;';
CDstr='\|CD,.*?;';
NTstr='\|NT,.*?;';
CPstr='\|CP,.*?;';
CRstr='\|CR,.*?;';
CNstr='\|CN,.*?;';



ArrCG=regexp(InfoSegment,CGstr,'match');
ChannelNum=numel(ArrCG);

ArrCN=regexp(InfoSegment,CNstr,'match');
ArrCD=regexp(InfoSegment,CDstr,'match');
ArrNT=regexp(InfoSegment,NTstr,'match');
ArrCP=regexp(InfoSegment,CPstr,'match');
ArrCb=regexp(InfoSegment,Cbstr,'match');
ArrCR=regexp(InfoSegment,CRstr,'match');

%% CN
ChannelName=cell(ChannelNum,1);
ChannelComment=cell(ChannelNum,1);

%% CD,NT
CDsample=zeros(ChannelNum,1);
CDUnit=cell(ChannelNum,1);

%% CP
KeyBufferRef=zeros(ChannelNum,1);
KeyBytes=zeros(ChannelNum,1);
KeyNumberFormat=cell(ChannelNum,1);
KeySignBits=zeros(ChannelNum,1);

%% Cb
KeyBufferRefIndex=zeros(ChannelNum,1);
KeyBufferRefCb=zeros(ChannelNum,1);
KeyOffsetBufferInSamplesKey=zeros(ChannelNum,1);
KeyBufferFilledBytes=zeros(ChannelNum,1);

%% CR
KeyTransformation=zeros(ChannelNum,1);
KeyCRfactor=zeros(ChannelNum,1);
KeyCRoffset=zeros(ChannelNum,1);
KeyUnit=cell(ChannelNum,1);

%% NT: the trigger time is usually identical for all channels, so the
% (expensive) conversion is done only once per unique time stamp
[UniqueNT,~,IdxUniqueNT]    = unique(ArrNT);
UniqueTriggerTime           = cell(size(UniqueNT));
for iUniqueNT = 1:numel(UniqueNT)
    UniqueTriggerTime{iUniqueNT} = ProcessNT(UniqueNT{iUniqueNT});
end
% reshape: UniqueTriggerTime is a row as soon as there is more than one
% unique time stamp, so the orientation has to be forced to a column
TriggerTime                 = reshape(UniqueTriggerTime(IdxUniqueNT),[],1);

%% Define Return object
Channel=repmat(struct('name','','comment','','data',[],'length',0,'yUnit','','t0','','dt','','xUnit','','loaded',false),1,ChannelNum);

BinaryStart=CSend;
ChannelID=1;
while ChannelID <= ChannelNum
   temptext=ArrCD{ChannelID};
   [CDsample(ChannelID,1), CDUnit{ChannelID,1}]=ProcessCD(temptext);
   [ChannelName{ChannelID,1},ChannelComment{ChannelID,1}]=ProcessCN(ArrCN{ChannelID});
   Channel(ChannelID).name=ChannelName{ChannelID,1};
   Channel(ChannelID).comment=ChannelComment{ChannelID,1};
   Channel(ChannelID).dt=[sprintf('%.15g',CDsample(ChannelID,1)),CDUnit{ChannelID,1}];
   Channel(ChannelID).xUnit=CDUnit{ChannelID,1};
   Channel(ChannelID).t0=TriggerTime{ChannelID,1};
   [KeyBufferRef(ChannelID,1),KeyBytes(ChannelID,1),KeyNumberFormat{ChannelID,1},KeySignBits(ChannelID,1)]=ProcessCP(ArrCP{ChannelID});
   [KeyBufferRefIndex(ChannelID,1),KeyBufferRefCb(ChannelID,1),KeyOffsetBufferInSamplesKey(ChannelID,1),KeyBufferFilledBytes(ChannelID,1)]=ProcessCblittle(ArrCb{ChannelID});
   [KeyTransformation(ChannelID,1),KeyCRfactor(ChannelID,1),KeyCRoffset(ChannelID,1),KeyUnit{ChannelID,1}]=ProcessCR(ArrCR{ChannelID});
   % <DS> quick fix for signals without transformation
   if KeyCRfactor(ChannelID,1)==0
       KeyCRfactor(ChannelID,1)=1;
   end
   % ---
   Channel(ChannelID).yUnit=KeyUnit{ChannelID,1};
   ChannelLength=KeyBufferFilledBytes(ChannelID,1)*8/KeySignBits(ChannelID,1);
   Channel(ChannelID).length=ChannelLength;
   % read the binary data only for the requested channels
   if LoadAllChannels || any(strcmp(Channel(ChannelID).name,Channels))
       BinaryRead= BinaryStart+KeyOffsetBufferInSamplesKey(ChannelID,1);
       Channel(ChannelID).data=ReadChannel(fid,BinaryRead,ChannelLength,KeyNumberFormat{ChannelID,1},KeyCRfactor(ChannelID,1),KeyCRoffset(ChannelID,1));
       Channel(ChannelID).loaded=true;
   end
   ChannelID=ChannelID+1;
end
fclose(fid);

end
%%

function Value = GetNumericField(TxtString,CommaLocation,iField)
% numeric value between the comma iField and the comma iField+1
Value = sscanf(TxtString(CommaLocation(iField)+1:CommaLocation(iField+1)-1),'%f');
if isempty(Value)
    Value = NaN;
end
end

function [KeyDx, KeyUnit]= ProcessCD(TxtString)

CommaLocation=find(TxtString==',');
KeyDx=GetNumericField(TxtString,CommaLocation,3);
KeyUnit=TxtString(CommaLocation(6)+1:CommaLocation(7)-1);
end

function TimeStart = ProcessNT(TxtString)

    CommaLocation=find(TxtString==',');
    KeyDay      = GetNumericField(TxtString,CommaLocation,3);
    KeyMonth    = GetNumericField(TxtString,CommaLocation,4);
    KeyYear     = GetNumericField(TxtString,CommaLocation,5);
    KeyHours    = GetNumericField(TxtString,CommaLocation,6);
    KeyMinutes  = GetNumericField(TxtString,CommaLocation,7);
    KeySeconds  = sscanf(TxtString(CommaLocation(8)+1:end),'%f');

    TimeStart=datestr(datenum([KeyYear, KeyMonth,KeyDay,KeyHours,KeyMinutes,KeySeconds]),'yyyy-mm-dd HH:MM:SS');
end

function [KeyBufferRef,KeyBytes,KeyNumerFormat,KeySignBits]= ProcessCP(TxtString)
CommaLocation=find(TxtString==',');
KeyBufferRef=GetNumericField(TxtString,CommaLocation,3);
KeyBytes=GetNumericField(TxtString,CommaLocation,4);
NumberFormat=GetNumericField(TxtString,CommaLocation,5);
switch (NumberFormat)
    case 1
        KeyNumerFormat='*uint';
    case 2
        KeyNumerFormat='*int';
    case 3
        KeyNumerFormat='*ushort';
    case 4
        KeyNumerFormat='*short';
    case 5
        KeyNumerFormat='*ulong';
    case 6
        KeyNumerFormat='*long';
    case 7
        KeyNumerFormat='*float';
    case 8
        KeyNumerFormat='*float32';
    case 9
        KeyNumerFormat='*'; % imc Device Transitional Recording
    case 10
        KeyNumerFormat='*TimeStampASII' ;% TimeStamp is famos type
    case 11
        KeyNumerFormat='*bit16'; %2-byte-word digital
    case 13
        KeyNumerFormat='*bit48';
    otherwise
        error('importfamos:UnknownNumberFormat','Number format %d is not supported!',NumberFormat)
end
KeySignBits=GetNumericField(TxtString,CommaLocation,6);
end

function [KeyBufferRefIndex,KeyBufferRefCb,KeyOffsetBufferInSamplesKey,KeyBufferFilledBytes] = ProcessCblittle(TxtString)
CommaLocation=find(TxtString==',');
KeyBufferRefIndex=GetNumericField(TxtString,CommaLocation,3);
KeyBufferRefCb=GetNumericField(TxtString,CommaLocation,5);
KeyOffsetBufferInSamplesKey=GetNumericField(TxtString,CommaLocation,7);
KeyBufferFilledBytes=GetNumericField(TxtString,CommaLocation,10);
end


function [KeyTransformation,KeyCRfactor,KeyCRoffset,KeyUnit]= ProcessCR(TxtString)
CommaLocation=find(TxtString==',');
KeyTransformation=GetNumericField(TxtString,CommaLocation,3);
KeyCRfactor=GetNumericField(TxtString,CommaLocation,4);
KeyCRoffset=GetNumericField(TxtString,CommaLocation,5);
KeyUnitLength=GetNumericField(TxtString,CommaLocation,7);
KeyUnit=TxtString(CommaLocation(8)+1:CommaLocation(8)+KeyUnitLength);

end

function [ChannelName,ChannelComment]= ProcessCN(TxtString)

CommaLocation=find(TxtString==',');
ChannelName=TxtString(CommaLocation(7)+1:CommaLocation(8)-1);

ChannelCommLength=TxtString(CommaLocation(8)+1:CommaLocation(9)-1);
if strcmp(ChannelCommLength,'0')
ChannelComment='';
else
    temp=str2double(ChannelCommLength);
    ChannelComment=TxtString(CommaLocation(9)+1:CommaLocation(9)+temp);
end

end

function tempChannel=ReadChannel(FileID, ReadStart,ChannelLength, Datatype,factor,offset)
fseek(FileID,ReadStart,'bof');

tempChannel=double(fread(FileID,ChannelLength,Datatype))*factor+offset;
end
