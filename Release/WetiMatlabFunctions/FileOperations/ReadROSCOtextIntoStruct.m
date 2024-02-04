% Simple function to read in ROSCO text into a structure
% DS on 01-Oct-2022
function Data = ReadROSCOtextIntoStruct(FileName)
RawData         = importdata(FileName);
ChannelName  	= strsplit(RawData.textdata{end-1,1});
ChannelName  	= ChannelName(~cellfun(@isempty,ChannelName));
for iChannel = 1:length(ChannelName)
    Data.(ChannelName{iChannel}) = RawData.data(:,iChannel);    
end
end