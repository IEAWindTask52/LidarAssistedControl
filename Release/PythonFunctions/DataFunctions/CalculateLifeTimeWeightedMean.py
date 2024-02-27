import numpy as np

def CalculateLifeTimeWeightedMean(process_results, statistics, wind_speed_bins, input_id, filter_id='WindSpeedsBinsFilter', output_id=None, output_id_per_bin=None, options=None):
    # Function for CalculateProcessResults.

    # inputs
    if options is None:
        options = {}
    if output_id is None:
        output_id = 'LTW_' + input_id
    if output_id_per_bin is None:
        output_id_per_bin = 'LTW_' + input_id + '_PerBin'

    # local variables for options
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
    mean_data = statistics[input_id]
    mean_per_bin = np.full(n_wind_speed_bins, np.nan)
    for i_wind_speed_bin in range(n_wind_speed_bins):
        filter_this_bin = filter_data[:, i_wind_speed_bin]
        mean_per_bin[i_wind_speed_bin] = np.mean(mean_data[filter_this_bin])

    # life time weighting MEAN
    distribution = k / A * (wind_speed_bins / A)**(k - 1) * np.exp(-(wind_speed_bins / A)**k)
    weights = distribution / np.sum(distribution)
    ltw_mean_per_bin = mean_per_bin * weights
    ltw_mean = np.sum(ltw_mean_per_bin)

    # store in ProcessResults
    process_results[output_id] = ltw_mean
    process_results[output_id_per_bin] = ltw_mean_per_bin

    return process_results
