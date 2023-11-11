

import pandas as pd
import numpy as np

def ReadROSCOtextIntoDataframe(file_name):
    """Reads ROSCO text data into a Python DataFrame.

      Args:
        FileName: The path to the ROSCO .IN file.

      Returns:
        A Python structure containing the ROSCO data.
      """
    raw_data = pd.read_csv(file_name, delim_whitespace=True, skiprows=3)

    # Remove cells containing strings
    raw_data = raw_data.apply(lambda series: series.map(lambda x: np.nan if isinstance(x, str) else x))

    return raw_data

