from joblib import Parallel, delayed
import os
import scipy.io as sio
from ReadFASTbinaryIntoTSC import ReadFASTbinaryIntoTSC
def CollectTimeResults(data_files, post_processing_config, n_core=None):
    if n_core is None:
        n_core = os.cpu_count()

    n_data_files = len(data_files)
    time_results = [None] * n_data_files

    # loop over data files
    def process_file(i_data_file):
        this_data_file = data_files[i_data_file]
        _, _, ext = os.path.splitext(this_data_file)
        tsc = None  # to avoid warning of "Uninitialized Temporaries"
        if ext == '.outb':
            tsc = ReadFASTbinaryIntoTSC(this_data_file)
        elif ext == '.res':
            tsc = ReadFlex5IntoTSC(this_data_file) #this function is not written yet
        elif ext == '.mat':
            dummy = sio.loadmat(this_data_file)
            tsc = dummy['TSC']
        else:
            raise ValueError(f'Currently {ext} files are not supported!')

        # keep only some channels, if requested
        if 'DataChannels' in post_processing_config and post_processing_config['DataChannels']:
            channels = post_processing_config['DataChannels']
            ts_names = list(TSC.columns)
            remove_idx = [i for i, name in enumerate(ts_names) if name not in channels]
            for i_ts_name in remove_idx:
                tsc = remove_ts(tsc, ts_names[i_ts_name])

        # load into cell
        time_results[i_data_file] = tsc

    Parallel(n_jobs=n_core)(delayed(process_file)(i_data_file) for i_data_file in range(n_data_files))

    return time_results
