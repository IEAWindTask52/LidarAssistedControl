from FileOperations.ReadFASTbinary import ReadFASTbinary


def ReadFASTbinaryIntoStruct(file_name):
    data, info = ReadFASTbinary(file_name)

    channel_names = info['attribute_names']
    structured_data = {}

    for i in range(len(channel_names)):
        channel_name = channel_names[i]
        structured_data[channel_name] = data[:, i]

    return structured_data
# source: Matlab-Function (ReadFASTbinaryIntoStruct.m)