import numpy as np

def CalculateREWSfromLidarData_LDP_v1(FBFF, DT, TMax, LDP):
    # Function to postprocess lidar data and calculate rotor-effective wind speed (REWS)
    # similar to LDP_v1/FFP_v1 without the need for compiling a DLL.

    # Initialize time
    R_FBFF = {}
    R_FBFF["Time"] = np.arange(0, TMax + DT, DT)
    n_t = len(R_FBFF["Time"])

    # Allocate arrays
    R_FBFF["REWS"] = np.full(n_t, np.nan)
    R_FBFF["REWS_f"] = np.full(n_t, np.nan)
    R_FBFF["REWS_b"] = np.full(n_t, np.nan)

    # Internal variable
    ThisChannel = f"VLOS{LDP['IndexGate']:02d}LI"

    # Loop over time
    for i_t in range(n_t):
        Idx = max(i_t - 1, 0)  # Due to co-simulation of OpenFAST and the controller DLLs

        # If there is a new measurement, perform wind field reconstruction
        if FBFF["NEWDATALI"][Idx]:
            v_los = FBFF[ThisChannel][Idx]
            REWS = WindFieldReconstruction(v_los, LDP["NumberOfBeams"], LDP["AngleToCenterline"])

        # Low-pass filter the REWS
        if LDP["FlagLPF"]:
            REWS_f = LPFilter(REWS, DT, LDP["f_cutoff"])
        else:
            REWS_f = REWS

        # Get buffered and filtered REWS from buffer
        REWS_b = Buffer(REWS_f, DT, LDP["T_buffer"])

        # Store in structure
        R_FBFF["REWS"][i_t] = REWS
        R_FBFF["REWS_f"][i_t] = REWS_f
        R_FBFF["REWS_b"][i_t] = REWS_b

    return R_FBFF


def WindFieldReconstruction(v_los, NumberOfBeams, AngleToCenterline):

    # Persistent variable (use list for simplicity)
  if not hasattr(WindFieldReconstruction, 'u_est_Buffer'):
    WindFieldReconstruction.u_est_Buffer = [np.nan] * NumberOfBeams

  # Estimate u component assuming perfect alignment
  u_est = v_los / np.cos(np.deg2rad(AngleToCenterline))

  # Update buffer for estimated u component
  WindFieldReconstruction.u_est_Buffer = WindFieldReconstruction.u_est_Buffer[1:] + [u_est]

  # Calculate REWS from mean over all estimated u components (handling NaNs)
  REWS = np.nanmean(WindFieldReconstruction.u_est_Buffer)

  return REWS


def LPFilter(InputSignal, DT, CornerFreq):

    # Initialization of persistent variables
    if not hasattr(LPFilter, 'OutputSignalLast'):
        LPFilter.OutputSignalLast = InputSignal
        LPFilter.InputSignalLast = InputSignal

    # Define coefficients
    a1 = 2 + CornerFreq * DT
    a0 = CornerFreq * DT - 2
    b1 = CornerFreq * DT
    b0 = CornerFreq * DT

    # Filter
    OutputSignal = 1.0 / a1 * (-a0 * LPFilter.OutputSignalLast + b1 * InputSignal + b0 * LPFilter.InputSignalLast)

    # Save signals for next time step
    LPFilter.InputSignalLast = InputSignal
    LPFilter.OutputSignalLast = OutputSignal

    return OutputSignal

def Buffer(REWS, DT, T_buffer):
    if not hasattr(Buffer, 'REWS_f_Buffer'):
        nBuffer = 2000
        Buffer.REWS_f_Buffer = np.full(nBuffer, np.nan)

    # Initialize REWS_f_Buffer
    nBuffer = 2000  # Size of REWS_f_buffer, 25 seconds at 80 Hz
    Buffer.REWS_f_Buffer = np.roll(Buffer.REWS_f_Buffer, 1)
    Buffer.REWS_f_Buffer[0] = REWS

    # Index for entry at T_buffer, minimum 1, maximum nBuffer
    Idx = min(max(int(np.floor(T_buffer / DT-1)), 0), nBuffer-1)

    # Get buffered and filtered REWS from buffer
    REWS_b = Buffer.REWS_f_Buffer[Idx]

    return REWS_b
