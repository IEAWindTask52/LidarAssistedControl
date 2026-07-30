import numpy as np
import math

def LDP_v3(isValid, beamID, lineOfSightWindSpeed, DT, LDP):
    # Function to postprocess lidar data to get the rotor-effective wind speed
    # (REWS) equal to the LDP_v1/FFP_v1 without the need of compiling a DLL.
    # Code is intended to be as close as possible to the Fortran Code.
    # v3: similar to v1, but includes ignoring of signals with invalid data.
    # 2026 version to be applied in each time step.

    # Persistent variables
    if not hasattr(LDP_v3, 'PreviousBeamID'):
        LDP_v3.PreviousBeamID = -1                  # force WFR on first call

    if not hasattr(LDP_v3, 'REWS'):
        LDP_v3.REWS = 18                            # only necessary if first lineOfSightWindSpeed is not valid

    # If there is a new measurement perform wind field reconstruction
    if beamID != LDP_v3.PreviousBeamID:

        REWS_new = WindFieldReconstruction(
            lineOfSightWindSpeed,
            isValid,
            LDP['NumberOfBeams'],
            LDP['AngleToCenterline']
        )

        if not np.isnan(REWS_new):                  # update only if not NaN
            LDP_v3.REWS = REWS_new

        LDP_v3.PreviousBeamID = beamID              # update beamID

    # Low pass filter the REWS
    if LDP['FlagLPF']:
        REWS_f = LPFilter(
            LDP_v3.REWS,
            DT,
            LDP['omega_cutoff']
        )
    else:
        REWS_f = LDP_v3.REWS

    # Get buffered and filtered REWS from buffer
    REWS_b = Buffer(
        REWS_f,
        DT,
        LDP['T_buffer']
    )

    return REWS_b


def WindFieldReconstruction(v_los, isValid, NumberOfBeams, AngleToCenterline):
    # Python version of the subroutine WindFieldReconstruction
    # in LDP_v1_Subs.f90 extended to deal with invalid data.

    # Initialize u_est_Buffer
    if not hasattr(WindFieldReconstruction, 'u_est_Buffer'):
        WindFieldReconstruction.u_est_Buffer = np.full(
            NumberOfBeams,
            np.nan
        )

    # Estimate u component assuming perfect alignment
    if isValid:
        u_est = v_los / math.cos(math.radians(AngleToCenterline))
    else:
        u_est = np.nan

    # Update buffer for estimated u component
    WindFieldReconstruction.u_est_Buffer = np.concatenate((
        [u_est],
        WindFieldReconstruction.u_est_Buffer[0:NumberOfBeams - 1]
    ))

    # Calculate REWS from mean over all estimated u components
    ValidValues = WindFieldReconstruction.u_est_Buffer[
        ~np.isnan(WindFieldReconstruction.u_est_Buffer)
    ]

    if len(ValidValues) > 0:
        REWS = np.mean(ValidValues)
    else:
        REWS = np.nan

    return REWS


def LPFilter(InputSignal, DT, CornerFreq):
    # Python version of the function LPFilter in FFP_v1_Subs.f90

    # Initialization
    if not hasattr(LPFilter, 'OutputSignalLast'):
        LPFilter.OutputSignalLast = InputSignal
        LPFilter.InputSignalLast = InputSignal

    # Define coefficients
    a1 = 2 + CornerFreq * DT
    a0 = CornerFreq * DT - 2
    b1 = CornerFreq * DT
    b0 = CornerFreq * DT

    # Filter
    OutputSignal = (
        -a0 * LPFilter.OutputSignalLast
        + b1 * InputSignal
        + b0 * LPFilter.InputSignalLast
    ) / a1

    # Save signals for next time step
    LPFilter.InputSignalLast = InputSignal
    LPFilter.OutputSignalLast = OutputSignal

    return OutputSignal


def Buffer(REWS, DT, T_buffer):

    # Initialize REWS_f_Buffer
    nBuffer = 400                               # Size of REWS_f_buffer, max 20 seconds at 20 Hz [-]

    if not hasattr(Buffer, 'REWS_f_Buffer'):
        Buffer.REWS_f_Buffer = np.ones(nBuffer) * REWS

    # Update buffer for estimated u component
    Buffer.REWS_f_Buffer = np.concatenate((
        [REWS],
        Buffer.REWS_f_Buffer[0:nBuffer - 1]
    ))

    # Index for entry at T_buffer, minimum 1, maximum nBuffer
    Idx = min(max(math.floor(T_buffer / DT), 1), nBuffer)

    # Get buffered and filtered REWS from buffer
    # MATLAB index Idx corresponds to Python index Idx - 1
    REWS_b = Buffer.REWS_f_Buffer[Idx - 1]

    return REWS_b


def reset_LDP_v3():
    # Equivalent to MATLAB: clear LDP_v3

    if hasattr(LDP_v3, 'PreviousBeamID'):
        del LDP_v3.PreviousBeamID

    if hasattr(LDP_v3, 'REWS'):
        del LDP_v3.REWS

    if hasattr(WindFieldReconstruction, 'u_est_Buffer'):
        del WindFieldReconstruction.u_est_Buffer

    if hasattr(LPFilter, 'OutputSignalLast'):
        del LPFilter.OutputSignalLast

    if hasattr(LPFilter, 'InputSignalLast'):
        del LPFilter.InputSignalLast

    if hasattr(Buffer, 'REWS_f_Buffer'):
        del Buffer.REWS_f_Buffer
