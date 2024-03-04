import numpy as np

def CalculateLifeTimeWeightedDEL(process_results, statistics, wind_speed_bins, input_id, filter_id='WindSpeedsBinsFilter', output_id=None, output_id_per_bin=None, options=None):
    # Function for CalculateProcessResults.

    # inputs
    if options is None:
        options = {}
    if output_id is None:
        output_id = 'LTW_' + input_id
    if output_id_per_bin is None:
        output_id_per_bin = 'LTW_' + input_id + '_PerBin'

    # local variables for options
    m = options.get('WoehlerExponent', 4)
    A = options.get('A_Weibull', 2 / np.sqrt(np.pi) * 10)
    k = options.get('k_Weibull', 2)

    # get dimensions
    n_wind_speed_bins = len(wind_speed_bins)
    filter_data = process_results[filter_id]
    _, n_filters = filter_data.shape

    # check dimension of filter
    if n_filters != n_wind_speed_bins:
        raise ValueError(f'Dimension of Filter {filter_id} does not fit WindSpeedBins input.')

    # loop over bins
    DEL = statistics[input_id]
    DEL_per_bin = np.full(n_wind_speed_bins, np.nan)
    for i_wind_speed_bin in range(n_wind_speed_bins):
        filter_this_bin = filter_data[:, i_wind_speed_bin]
        DEL_this_bin = DEL[filter_this_bin]
        n_this_bin = len(DEL_this_bin)
        weights = np.ones(n_this_bin) / n_this_bin  # equal weights
        DEL_per_bin[i_wind_speed_bin] = np.sum(weights * DEL_this_bin**m)**(1 / m)

    # life-time weighting DEL
    distribution = k / A * (wind_speed_bins / A)**(k - 1) * np.exp(-(wind_speed_bins / A)**k)
    weights = distribution / np.sum(distribution)
    LTW_DEL_per_bin = DEL_per_bin * weights**(1 / m)
    LTW_DEL = np.sum(LTW_DEL_per_bin**m)**(1 / m)

    # store in ProcessResults
    process_results[output_id] = LTW_DEL
    process_results[output_id_per_bin] = LTW_DEL_per_bin

    return process_results
