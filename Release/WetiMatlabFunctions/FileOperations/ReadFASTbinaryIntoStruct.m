% Simple function to read in FAST binary into a structure
% DS on 27-Sep-2022
function Data = ReadFASTbinaryIntoStruct(FileName)
[RawData, ChannelName, ~, ~, ~] 	= ReadFASTbinary(FileName);
for iChannel = 1:length(ChannelName)
    Data.(ChannelName{iChannel}) = RawData(:,iChannel);    
end
end