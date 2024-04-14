import numpy as np

def CalculateLifeTimeWeightedDEL(ProcessResults, Statistics, WindSpeedBins, InputID, FilterID='WindSpeedsBinsFilter', OutputID=None, OutputID_PerBin=None, options=None):
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
    options.setdefault('WoehlerExponent', 4)
    options.setdefault('A_Weibull', 2 / np.sqrt(np.pi) * 10)  # Class I
    options.setdefault('k_Weibull', 2)  # Rayleigh distribution

    # get dimensions
    Filter = ProcessResults[FilterID]
    nFilters = Filter.shape[1]

    # local variables from options
    m = options['WoehlerExponent']
    A = options['A_Weibull']
    k = options['k_Weibull']

    # check dimension of filter
    if nFilters != len(WindSpeedBins):
        raise ValueError(f'Dimension of Filter {FilterID} does not fit WindSpeedBins input.')
    # loop over bins
    DEL = Statistics[InputID]
    DEL_PerBin = np.full(nFilters, np.nan)
    for i_wind_speed_bin in range(nFilters):
        DEL_ThisBin = DEL[Filter[:, i_wind_speed_bin]]
        n_ThisBin = len(DEL_ThisBin)
        Weights = np.ones(n_ThisBin) / n_ThisBin  # equal weights
        DEL_PerBin[i_wind_speed_bin] = np.sum(Weights * DEL_ThisBin ** m) ** (1 / m)

    # life-time weighting DEL
    Distribution = k / A * (WindSpeedBins / A) ** (k - 1) * np.exp(-(WindSpeedBins / A) ** k)
    Weights = Distribution / np.sum(Distribution)
    LTW_DEL_PerBin = DEL_PerBin * Weights ** (1 / m)
    LTW_DEL = np.sum(LTW_DEL_PerBin ** m) ** (1 / m)

    ProcessResults[OutputID] = LTW_DEL

    ProcessResults[OutputID_PerBin] = LTW_DEL_PerBin

    return ProcessResults
