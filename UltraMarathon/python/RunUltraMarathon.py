# Runs the "Ultra Marathon" for the LAC Summer Games 2026:
# Purpose:
# We want to learn how Lidar Assisted Control (LAC) can be applied to real
# world data. The data has been collected on April 5, 2026 on a turbine
# with 126 m rotor diameter and a 4-beam-pulsed lidar system.
# Details see instructions: https://doi.org/10.5281/zenodo.21728615.
# Initial result:
# Smallest Detectable Eddy Size is estimated as 1.738 D.
# Constraints OK. Cost for Summer Games 2026 is 94.96 %.

# setup
import numpy as np
import matplotlib.pyplot as plt
import rainflow
import sys

import h5py
from scipy.signal import coherence
from scipy.signal.windows import hamming

# add path to functions
sys.path.append('functions')
from NREL5MWDefaultParameter_SLOW import NREL5MWDefaultParameter_SLOW
from NREL5MWDefaultParameter_FBNREL import NREL5MWDefaultParameter_FBNREL
from FBController import FBController, reset_FBController
from LDP_v3 import LDP_v3, reset_LDP_v3
from SLOW import SLOW
from rpm2radPs import rpm2radPs
from radPs2rpm import radPs2rpm

# load data
with h5py.File('../data/DataSummerGames2026.mat', 'r') as Data:
    time                    = np.array(Data['time']).squeeze()
    lineOfSightWindSpeed    = np.array(Data['lineOfSightWindSpeed']).T
    isValid                 = np.array(Data['isValid']).T.astype(bool)
    beamID                  = np.array(Data['beamID']).squeeze()
    v_0                     = np.array(Data['v_0']).squeeze()

# simulation parameter (should not be changed)
Parameter   = NREL5MWDefaultParameter_SLOW()                            # turbine parameters
x_0         = np.array([rpm2radPs(12.1), 0.2, 0, np.deg2rad(13), 0])    # initial values for states x = [rotor speed, tower top displacement, tower top speed, pitch angle, pitch rate]
y_0         = np.array([rpm2radPs(12.1) * 97, np.deg2rad(13), 0, 5e6])  # initial values for measurements y = [generator speed, pitch angle, tower top acceleration, electrical power]
dt          = time[1] - time[0]                                         # [s]           time step size
n_t         = len(time)                                                 # [-]           number of time steps
m           = 4                                                         # [-]           Woehler Exponent for steel
N_REF       = 2e6 / (20 * 8760) * 12                                    # [-]           fraction of 2e6 in 20 years for 12 h

# other parameters (please add here your parameters): as example, the simple LDP from the Summer Games 2025 and a very simple feedforward controller is used
Parameter = NREL5MWDefaultParameter_FBNREL(Parameter)  # controller parameters
LDP = {
    'NumberOfBeams': 4,                         # [-]           Number of beams measuring at different directions
    'AngleToCenterline': 19.176,                # [deg]         Angle around centerline
    'FlagLPF': 1,                               # [0/1]         Enable low-pass filter
    'omega_cutoff': 0.13,                       # [rad/s]       Corner frequency (-3dB) of low-pass filter
    'T_buffer': 0.2                             # [s]           Buffer time for filtered REWS signal
}

IndexGate           = 0                         # [-]           MATLAB IndexGate = 1
GradientStaticPitch = np.deg2rad(1)             # [rad/(m/s)]   Gradient in static pitch curve
reset_LDP_v3()

# simulation feedback only (should not be changed)
# allocation and initialization
x_FB        = np.full((n_t, 5), np.nan)
y_FB        = np.full((n_t, 4), np.nan)
u_FB        = np.full((n_t, 2), np.nan)
x_FB[0, :]  = x_0                               # init states
y_FB[0, :]  = y_0                               # init measurements
reset_FBController()                            # clear persistent variables

# loop over time
for i_t in range(n_t - 1):

    # calculate feedback controller
    y_ThisStep          = y_FB[i_t, :]
    u_ThisStep          = FBController(y_ThisStep, 0, dt, Parameter)

    # simulate wind turbine
    x_ThisStep          = x_FB[i_t, :]
    d_ThisStep          = v_0[i_t]
    x_NextStep, y_NextStep = SLOW(x_ThisStep, u_ThisStep, d_ThisStep, dt, Parameter)

    # store simulation results
    u_FB[i_t, :]        = u_ThisStep
    x_FB[i_t + 1, :]    = x_NextStep
    y_FB[i_t + 1, :]    = y_NextStep

# calculate overspeed, energy, loads, power quality and pitch travel
MaxSpeed_FB     = np.max(y_FB[:, 0])
Energy_FB       = np.sum(y_FB[:, 3]) * dt
PowerStd_FB     = np.std(y_FB[:, 3], ddof=1)
M_yT_FB         = Parameter.Turbine.HubHeight * (Parameter.Turbine.c_eT * x_FB[:, 2] + Parameter.Turbine.k_eT * x_FB[:, 1])
c               = np.array([[cycle[2], cycle[0]] for cycle in rainflow.extract_cycles(M_yT_FB)])
TowerDEL_FB     = (np.sum(c[:, 1] ** m * c[:, 0]) / N_REF) ** (1 / m)
PitchTravel_FB  = np.sum(np.abs(np.diff(y_FB[:, 1])))

# simulation lidar-assisted: please only adjust code between >>> <<<!
# allocation and initialization
x_LA        = np.full((n_t, 5), np.nan)
y_LA        = np.full((n_t, 4), np.nan)
u_LA        = np.full((n_t, 2), np.nan)
v_0L        = np.full(n_t, np.nan)
x_LA[0, :]  = x_0                               # init states
y_LA[0, :]  = y_0                               # init measurements
reset_FBController()                            # clear persistent variables

# loop over time
for i_t in range(n_t - 1):

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # First discipline (Preview Quality): Provide v_0L[i_t] only based on current (i_t) or past lidar signals: isValid, beamID, lineOfSightWindSpeed
    # Second discipline (Load Reduction): Provide u_ThisStep only based on current (i_t) or past lidar signals and turbine signals from y_ThisStep

    # simple lidar data processing
    v_0L[i_t]           = LDP_v3(isValid[i_t, IndexGate], beamID[i_t], lineOfSightWindSpeed[i_t, IndexGate], dt, LDP)

    # calculate combined feedback-feedforward controller
    WindAcceleration    = (v_0L[i_t] - v_0L[max(i_t - 1, 0)]) / dt
    u_FF_ThisStep       = WindAcceleration * GradientStaticPitch  # simple collective pitch feedforward controller
    y_ThisStep          = y_LA[i_t, :]
    u_ThisStep          = FBController(y_ThisStep, u_FF_ThisStep, dt, Parameter)
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    # simulate wind turbine
    x_ThisStep          = x_LA[i_t, :]
    d_ThisStep          = v_0[i_t]
    x_NextStep, y_NextStep = SLOW(x_ThisStep, u_ThisStep, d_ThisStep, dt, Parameter)

    # store simulation results
    u_LA[i_t, :]        = u_ThisStep
    x_LA[i_t + 1, :]    = x_NextStep
    y_LA[i_t + 1, :]    = y_NextStep

# calculate overspeed, energy, loads, power quality and pitch travel
MaxSpeed_LA     = np.max(y_LA[:, 0])
Energy_LA       = np.sum(y_LA[:, 3]) * dt
PowerStd_LA     = np.std(y_LA[:, 3], ddof=1)
M_yT_LA         = Parameter.Turbine.HubHeight * (Parameter.Turbine.c_eT * x_LA[:, 2] + Parameter.Turbine.k_eT * x_LA[:, 1])
c               = np.array([[cycle[2], cycle[0]] for cycle in rainflow.extract_cycles(M_yT_LA)])
TowerDEL_LA     = (np.sum(c[:, 1] ** m * c[:, 0]) / N_REF) ** (1 / m)
PitchTravel_LA  = np.sum(np.abs(np.diff(y_LA[:, 1])))

# plot simulation results
IdxPlot = time <= 600

plt.figure(figsize=(10, 10))

plt.subplot(511)
plt.plot(time[IdxPlot], v_0[IdxPlot], label='rotor')
plt.plot(time[IdxPlot], v_0L[IdxPlot], label='lidar')
plt.grid()
plt.ylabel('[m/s]')
plt.legend(loc='best')

plt.subplot(512)
plt.plot(time[IdxPlot], np.rad2deg(u_FB[IdxPlot, 0]), label='FB')
plt.plot(time[IdxPlot], np.rad2deg(u_LA[IdxPlot, 0]), label='LA')
plt.grid()
plt.ylabel('pitch angle\n[deg]')
plt.legend(loc='best')

plt.subplot(513)
plt.plot(time[IdxPlot], u_FB[IdxPlot, 1] / 1e3)
plt.plot(time[IdxPlot], u_LA[IdxPlot, 1] / 1e3)
plt.grid()
plt.ylabel('generator torque\n[kNm]')

plt.subplot(514)
plt.plot(time[IdxPlot], radPs2rpm(x_FB[IdxPlot, 0]))
plt.plot(time[IdxPlot], radPs2rpm(x_LA[IdxPlot, 0]))
plt.grid()
plt.ylabel('rotor speed\n[rpm]')

plt.subplot(515)
plt.plot(time[IdxPlot], M_yT_FB[IdxPlot] / 1e6)
plt.plot(time[IdxPlot], M_yT_LA[IdxPlot] / 1e6)
plt.grid()
plt.ylabel('tower base bending moment\n[MNm]')
plt.xlabel('time [s]')

plt.tight_layout()

# estimate coherence and SDES  (should not be changed)
n_FFT = 2 ** 11

f_est, gamma_Sq_est = coherence(
    v_0[0:n_t - 1] - np.mean(v_0[0:n_t - 1]),
    v_0L[0:n_t - 1] - np.mean(v_0L[0:n_t - 1]),
    fs=1 / dt,
    window=hamming(n_FFT),
    noverlap=n_FFT // 2,
    nfft=n_FFT,
    detrend=False
)

k_est = 2 * np.pi * f_est / np.mean(v_0)

plt.figure()
plt.plot([1e-3, 1], [0.5, 0.5])
plt.plot(k_est, gamma_Sq_est, '-')
plt.grid()
plt.xscale('log')
plt.xlabel('wave number [rad/m]')
plt.ylabel('Coherence [-]')
plt.xlim([1e-3, 1])
plt.ylim([0, 1])

Idx     = np.arange(np.where(np.diff(gamma_Sq_est) > 0)[0][0] + 1)  # find monotonic descending values
if np.min(gamma_Sq_est[Idx]) <= 0.5 and np.max(gamma_Sq_est[Idx]) > 0.5:
    MCB     = np.interp(0.5, gamma_Sq_est[Idx][::-1], k_est[Idx][::-1])  # measurement coherence bandwidth
    SDES    = 2 * np.pi / MCB / 126
    print(f'Smallest Detectable Eddy Size is estimated as {SDES:#.4g} D.')
else:
    print('Smallest Detectable Eddy Size cannot be estimated since the monotonic descending values in the coherence do not cross 0.5.')

# evaluate simulation results (should not be changed)
Cost            = TowerDEL_LA / TowerDEL_FB
EnergyOK        = (Energy_FB - Energy_LA) <= 1 * 60 * 60 * 1e3  # energy loss over 12h up to 1kWh acceptable
MaxSpeedOK      = MaxSpeed_LA <= MaxSpeed_FB                    # same or less overspeed
PowerStdOK      = PowerStd_LA <= PowerStd_FB                    # same or better power quality
PitchTravelOK   = PitchTravel_LA <= PitchTravel_FB              # same or less pitch actuator duty

if EnergyOK and MaxSpeedOK and PowerStdOK and PitchTravelOK:
    print(f'Constraints OK. Cost for Summer Games 2026 is {Cost * 100:#.4g} %.')
elif not EnergyOK:
    print(f'Energy too low. Cost for Summer Games 2026 is {Cost * 100:#.4g} %.')
elif not MaxSpeedOK:
    print(f'Maximum rotor speed too high. Cost for Summer Games 2026 is {Cost * 100:#.4g} %.')
elif not PowerStdOK:
    print(f'Power standard deviation too high. Cost for Summer Games 2026 is {Cost * 100:#.4g} %.')
elif not PitchTravelOK:
    print(f'Pitch travel too high. Cost for Summer Games 2026 is {Cost * 100:#.4g} %.')

plt.show()
