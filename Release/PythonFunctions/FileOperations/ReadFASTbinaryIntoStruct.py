from FileOperations.ReadFASTbinary import ReadFASTbinary


def ReadFASTbinaryIntoStruct(file_name):
    RawData, ChannelName, _, _, _ = ReadFASTbinary(file_name)
    Data = {}

    for iChannel in range(len(ChannelName)):
        channel_name = ChannelName[iChannel]
        Data[ChannelName[iChannel]] = RawData[:, iChannel]

    return Data
# source: Matlab-Function (ReadFASTbinaryIntoStruct.m)
