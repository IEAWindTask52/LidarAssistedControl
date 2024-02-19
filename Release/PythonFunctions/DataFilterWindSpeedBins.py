import numpy as np


def DataFilterWindSpeedBins(Statistics, WindSpeedBins, WindSpeedChannel, WindSpeedBinWidth=1,
                                FilterID='WindSpeedsBinsFilter', ProcessResults={}):
    """
    Filter Data based on wind speed bins
    Function for CalculateProcessResults.
    """
    # get dimensions
    nWindSpeedBins = len(WindSpeedBins)
    nDataFiles = len(Statistics)
    GoodData = np.zeros((nDataFiles, nWindSpeedBins), dtype=bool)

    # filter
    for iWindSpeedBin in range(nWindSpeedBins):
        WindSpeedBin = WindSpeedBins[iWindSpeedBin]
        GoodData[:, iWindSpeedBin] = (Statistics[WindSpeedChannel] >= WindSpeedBin - WindSpeedBinWidth / 2) & \
                                     (Statistics[WindSpeedChannel] < WindSpeedBin + WindSpeedBinWidth / 2)

    ProcessResults[FilterID] = GoodData
    ProcessResults['WindSpeedBins'] = WindSpeedBins

    return ProcessResults
