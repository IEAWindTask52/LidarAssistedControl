% Simple function to read in log files into a structure
% DS on 18-Jul-2023
function Data = ReadLogIntoStruct(FileName)
RawData         = importdata(FileName);
ChannelName  	= RawData.textdata;
for iChannel = 1:length(ChannelName)
    Data.(ChannelName{iChannel}) = RawData.data(:,iChannel);    
end
end