% Simple function to read in ROSCO text into a structure
% DS on 01-Oct-2022
function Data = ReadROSCOtextIntoStruct(FileName)

    % load data
    fid             = fopen(FileName);
    FirstLine       = fgetl(fid);
    ChannelName  	= strsplit(fgetl(fid));
    ChannelName  	= ChannelName(~cellfun(@isempty,ChannelName));
    Units           = strsplit(fgetl(fid));
    Units  	        = Units(~cellfun(@isempty,Units));
    nChannels       = length(ChannelName);
    Format          = repmat('%f',1,nChannels);
    RawData         = textscan(fid,Format);
    fclose(fid);
    
    % loop over channels
    for iChannel = 1:length(ChannelName)
        Data.(ChannelName{iChannel}) = RawData{:,iChannel};    
    end
    
    % Display
    fprintf('Reading from the file %s:\n "%s"\n',FileName,FirstLine);
end