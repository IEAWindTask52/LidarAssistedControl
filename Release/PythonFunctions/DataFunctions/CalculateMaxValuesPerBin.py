import numpy as np

def CalculateMaxValuesPerBin(process_results, statistics, input_id, filter_id='WindSpeedsBinsFilter', output_id=None, output_id_per_bin=None):
    if output_id is None:
        output_id = input_id
    if output_id_per_bin is None:
        output_id_per_bin = input_id + '_PerBin'

    filter_matrix = process_results[filter_id]
    _, n_bins = filter_matrix.shape

    max_values = statistics[input_id].values
    max_per_bin = np.empty(n_bins)

    for i_bin in range(n_bins):
        max_per_bin[i_bin] = np.max(max_values[filter_matrix[:, i_bin]])

    process_results[output_id] = np.max(max_per_bin)
    process_results[output_id_per_bin] = max_per_bin

    return process_results