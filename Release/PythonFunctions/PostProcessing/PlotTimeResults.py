import matplotlib.pyplot as plt

def PlotTimeResults(time_results, post_processing_config):
    if 'BasicTimePlot' in post_processing_config['Plots']:
        n_figures = len(post_processing_config['Plots']['BasicTimePlot'])
        for i_figure in range(n_figures):
            plot_config = post_processing_config['Plots']['BasicTimePlot'][i_figure]
            if plot_config['Enable']:
                plt.figure(f'BasicTimePlot {i_figure+1}')
                n_channels = len(plot_config['Channels'])
                # determine IndicesConsideredDataFiles and nConsideredDataFiles
                if 'IndicesConsideredDataFiles' in plot_config:  # plot only considered files
                    indices_considered_data_files = plot_config['IndicesConsideredDataFiles']
                    n_considered_data_files = len(indices_considered_data_files)
                else:
                    n_considered_data_files = len(time_results)
                    indices_considered_data_files = list(range(n_considered_data_files))
                # make one subplot for each channel
                for i_channel in range(n_channels):
                    this_channel = plot_config['Channels'][i_channel]
                    this_channel_unit = f"{this_channel}_Unit"
                    plt.subplot(n_channels, 1, i_channel+1)
                    plt.grid(True)
                    # plot data
                    for i_considered_data_file in range(n_considered_data_files):
                        i_data_file = indices_considered_data_files[i_considered_data_file]-1
                        plt.plot(time_results[i_data_file][this_channel])
                    # ylabel: use unit from last considered file
                    plt.ylabel(f"[{time_results[i_data_file][this_channel_unit][0].replace('(', '').replace(')', '')}]")
                    # plot title: fixed to channels
                    plt.title(this_channel)
                    # plot legend
                    if 'legend' in plot_config:
                        plt.legend(plot_config['legend'])
                    # set gca properties
                    if 'gca' in plot_config:
                        for property_field, property_value in plot_config['gca'].items():
                            plt.gca().set(**{property_field: property_value})
                # xlabel: fixed to time
                plt.xlabel('time [s]')
                plt.tight_layout()
    plt.show()

    if 'ComparisonTimePlot' in post_processing_config['Plots']:
        n_figures = len(post_processing_config['Plots']['ComparisonTimePlot'])
        for i_figure in range(n_figures):
            plot_config = post_processing_config['Plots']['ComparisonTimePlot'][i_figure]
            if plot_config['Enable']:
                plt.figure(f'ComparisonTimePlot {i_figure + 1}')
                n_channels = len(plot_config['Channels'])
                # determine IndicesConsideredDataFiles and nConsideredDataFiles
                if 'IndicesConsideredDataFiles' in plot_config:  # plot only considered files
                    indices_considered_data_files = plot_config['IndicesConsideredDataFiles']
                    n_considered_data_files = len(indices_considered_data_files)
                else:
                    n_considered_data_files = len(time_results)
                    indices_considered_data_files = list(range(n_considered_data_files))
                # make one subplot for each considered data file
                for i_considered_data_file in range(n_considered_data_files):
                    plt.subplot(n_considered_data_files, 1, i_considered_data_file + 1)
                    plt.grid(True)
                    i_data_file = indices_considered_data_files[i_considered_data_file]
                    for i_channel in range(n_channels):
                        this_channel = plot_config['Channels'][i_channel]
                        plt.plot(time_results[i_data_file][this_channel])
                    # ylabel: need to be defined due to different channels
                    if 'ylabel' in plot_config:
                        plt.ylabel(plot_config['ylabel'])
                    # plot title
                    if 'title' in plot_config:
                        plt.title(plot_config['title'][i_data_file])
                    # plot legend: fixed to Channels
                    plt.legend(plot_config['Channels'])
                    # set gca properties
                    if 'gca' in plot_config:
                        for property_field, property_value in plot_config['gca'].items():
                            plt.gca().set(property_field, property_value)
                # xlabel: fixed to time
                plt.xlabel('time [s]')
                plt.show()