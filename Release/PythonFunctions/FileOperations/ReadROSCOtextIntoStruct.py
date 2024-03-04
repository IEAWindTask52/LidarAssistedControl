import numpy as np

def ReadROSCOtextIntoDataframe(FileName):
    raw_data = np.loadtxt(FileName, skiprows=3)

    with open(FileName, 'r') as file:
        lines = file.readlines()
        channel_names = lines[1].split()

    data = {}
    for i, channel_name in enumerate(channel_names):
        data[channel_name] = raw_data[:, i]
                                                                        

                                     
    return data
    