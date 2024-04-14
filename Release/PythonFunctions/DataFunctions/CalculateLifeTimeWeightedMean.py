import numpy as np

def CalculateLifeTimeWeightedMean(ProcessResults, Statistics, WindSpeedBins, InputID, FilterID='WindSpeedsBinsFilter', OutputID=None, OutputID_PerBin=None, options=None):
    # Calculates life-time weighted DEL.
    # Function for CalculateProcessResults.

    # inputs
    if options is None:
        options = {}
    if OutputID is None:
        OutputID = 'LTW_' + InputID
    if OutputID_PerBin is None:
        OutputID_PerBin = 'LTW_' + InputID + '_PerBin'

    # default variables for options
    options.setdefault('A_Weibull', 2 / np.sqrt(np.pi) * 10)  # Class I
    options.setdefault('k_Weibull', 2)  # Rayleigh distribution

    # get dimensions
    Filter = ProcessResults[FilterID]
    nFilters = Filter.shape[1]

    # local variables for options
    A = options['A_Weibull']
    k = options['k_Weibull']

    # check dimension of filter
    if nFilters != len(WindSpeedBins):
        raise ValueError(f"Dimension des Filters '{FilterID}' passt nicht zu WindSpeedBins Eingabe.")

    # loop over bins
    MEAN = Statistics[InputID]
    MEAN_PerBin = np.empty(len(WindSpeedBins))
    for i, wind_speed_bin in enumerate(WindSpeedBins):
        MEAN_PerBin[i] = np.mean(MEAN[Filter[:, i]])

    # life time weighting MEAN
    Distribution = k / A * (WindSpeedBins / A) ** (k - 1) * np.exp(-(WindSpeedBins / A) ** k)
    Weights = Distribution / np.sum(Distribution)
    LTW_MEAN_PerBin = MEAN_PerBin * Weights
    LTW_MEAN = np.sum(LTW_MEAN_PerBin)

    # store in ProcessResults
    ProcessResults[OutputID] = LTW_MEAN
    ProcessResults[OutputID_PerBin] = LTW_MEAN_PerBin

    return ProcessResults