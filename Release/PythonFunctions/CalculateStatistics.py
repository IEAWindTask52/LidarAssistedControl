import pandas as pd
import numpy as np

def CalculateStatistics(time_results, post_processing_config):
    # init
    statistics = pd.DataFrame()

    # return, if not defined or empty
    if 'CalculateStatistics' not in post_processing_config or not post_processing_config['CalculateStatistics']:
        return statistics

    # get dimensions
    n_statistics = len(post_processing_config['CalculateStatistics'])
    statistics_ids = [item[0] for item in post_processing_config['CalculateStatistics']]
    functions = [item[1] for item in post_processing_config['CalculateStatistics']]
    channel_cells = [item[2] for item in post_processing_config['CalculateStatistics']]
    n_data_files = len(time_results)

    # loop over Statistics
    for i_statistic in range(n_statistics):
        # extract
        this_statistics_id = statistics_ids[i_statistic]
        this_function = functions[i_statistic]
        this_channel_cell = channel_cells[i_statistic]
        n_channels = len(this_channel_cell)

        # loop over Channels
        for i_channel in range(n_channels):
            this_channel = this_channel_cell[i_channel]
            variable = f'{this_statistics_id}_{this_channel}'
            value = np.full(n_data_files, np.nan)  # Allocation
            for i_data_file in range(n_data_files):  # parfor is slower
                data = time_results[i_data_file][this_channel].values
                time = time_results[i_data_file][this_channel].index.values
                value[i_data_file] = this_function(data, time)
            statistics[variable] = value

    # add RowNames
    statistics.index = [time_results[i_data_file].name for i_data_file in range(n_data_files)]

    return statistics
