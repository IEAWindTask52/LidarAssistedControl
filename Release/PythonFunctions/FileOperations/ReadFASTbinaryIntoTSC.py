import pandas as pd
from FileOperations.ReadFASTbinary_05 import ReadFASTbinary
def ReadFASTbinaryIntoTSC(file_name):
    # based on LoadFAST

    # read in data and get time
    channels, chan_name, chan_unit, _, _ = ReadFASTbinary(file_name)
    time = channels[:, 0]

    # create a DataFrame object
    tsc = pd.DataFrame(channels, columns=chan_name, index=time)

    # loop over channels
    n_channel = len(chan_name)
    for i_channel in range(1, n_channel):
        # create time series
        ts = pd.Series(channels[:, i_channel], index=time)
        ts.name = chan_name[i_channel]
        # add to DataFrame object
        tsc[ts.name] = ts
        for col, unit in zip(tsc.columns, chan_unit):
            tsc[col + '_Unit'] = unit

    return tsc
