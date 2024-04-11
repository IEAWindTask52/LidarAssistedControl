# IEA15MW_03: IEA 15 MW monopile + realistic wind preview  from a
# 4-beam pulsed lidar system measuring at 160 m. 
# Purpose:
# Here, we use a realistic wind preview to demonstrate that the collective
# pitch feedforward controller together with the correct filtering provides
# a reduction in rotor speed variation and the coherence is as expected.
# In this example, we assume frozen turbulence, only one 3D turbulent
# wind field (y,z,t) at rotor plane is generated.
# Result:
# Change in rotor speed standard deviation:  -49.5 %
# Cost for Summer Games 2024 ("18 m/s hurdles"):  0.444660 m/s

# Setup
import os
import shutil
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal.windows import hamming
from scipy.signal import welch
from scipy import signal
from scipy.interpolate import interp1d
from scipy.io import loadmat
import sys

sys.path.append('../PythonFunctions')
from FileOperations.ManipulateTXTFile import ManipulateTXTFile
from FileOperations.ReadFASTbinaryIntoStruct import ReadFASTbinaryIntoStruct
from FileOperations.ReadROSCOtextIntoStruct import ReadROSCOtextIntoDataframe
from PreProcessing.CalculateREWSfromWindField import CalulateREWSfromWindField

# Seeds (can be adjusted, but will provide different results)
nSeed = 6                                           # [-] number of stochastic turbulence field samples
Seed_vec = [i+18*100 for i in range(1, nSeed + 1)]  # [-] vector of seeds

# Parameters postprocessing (can be adjusted, but will provide different results)
t_start = 60                                        # [s] 	ignore data before for STD and spectra
TMax = 660                                          # [s]   total run time, same as in *.fst
DT = 0.0125                                         # [s]   time step, same as in *.fst
R = 120                                             # [m]  	rotor radius to calculate REWS
nBlock = 2                                          # [-]   number of blocks for spectra
Fs = 1 / DT                                         # [Hz]  sampling frequency
AnalysisTime = TMax - t_start                       # [s]   time to calculate spectra etc.
nDataPerBlock = int(AnalysisTime / nBlock * Fs)     # [-]  	data per block, here 2 blocks
vWindow = hamming(nDataPerBlock)                    # [-] 	window for estimation
nFFT = 2**(int(np.ceil(np.log2(nDataPerBlock))))    # [-]  	number of FFT, default: 2^nextpow2(nDataPerBlock)
nOverlap = nDataPerBlock / 2                        # [-]  	samples of overlap, default: 50% overlap

# Parameter for Cost (Summer Games 2024)
tau = 2                                             # [s]   time to overcome pitch actuator, from Example 1: tau = T_Taylor - T_buffer, since there T_filter = T_scan = 0

# Files (should not be changed)
TurbSimExeFile = 'TurbSim_x64.exe'
FASTexeFile = 'openfast_x64.exe'
SimulationName = 'IEA-15-240-RWT-Monopile_4BeamPulsed'
TurbSimTemplateFile = 'TurbSim2aInputFileTemplateIEA15MW.inp'
SimulationFolderFB = 'SimulationResults_FeedbackOnly'
SimulationFolderLAC = 'SimulationResults_4BeamPulsed'

if not os.path.exists('TurbulentWind'):
    os.makedirs('TurbulentWind')

if not os.path.exists(SimulationFolderFB):
    os.makedirs(SimulationFolderFB)

if not os.path.exists(SimulationFolderLAC):
    os.makedirs(SimulationFolderLAC)

# Preprocessing: generate turbulent wind field

# Copy the adequate TurbSim version to the example folder
shutil.copyfile(os.path.join('../TurbSim', TurbSimExeFile), os.path.join('TurbulentWind', TurbSimExeFile))

# Generate all wind fields
for iSeed in range(nSeed):
    Seed = Seed_vec[iSeed]
    WindFileName = f'URef_18_Seed_{Seed:02d}'
    TurbSimInputFile = os.path.join('TurbulentWind', f'{WindFileName}.ipt')
    TurbSimResultFile = os.path.join('TurbulentWind', f'{WindFileName}.wnd')
    if not os.path.exists(TurbSimResultFile):
        shutil.copyfile(TurbSimTemplateFile, TurbSimInputFile)
        ManipulateTXTFile(TurbSimInputFile, 'MyRandSeed1', str(Seed))  # adjust seed
        os.system(os.path.join('TurbulentWind', TurbSimExeFile) + ' ' + TurbSimInputFile)

# Clean up
os.remove(os.path.join('TurbulentWind', TurbSimExeFile))

# Processing: run simulations

# Copy the adequate OpenFAST version to the example folder
shutil.copyfile(os.path.join('../OpenFAST', FASTexeFile), FASTexeFile)

#  Simulate with all wind fields
for iSeed in range(nSeed):

    # Adjust the InflowWind file
    Seed = Seed_vec[iSeed]
    WindFileName = f'URef_18_Seed_{Seed:02d}'
    WindFileRoot = os.path.join('TurbulentWind', WindFileName)
    ManipulateTXTFile('IEA-15-240-RWT_InflowFile.dat', 'MyFilenameRoot', WindFileRoot)

    # Run FB
    FASTresultFile = os.path.join(SimulationFolderFB, f'{WindFileName}_FlagLAC_0.outb')
    ROSCOresultFile = os.path.join(SimulationFolderFB, f'{WindFileName}_FlagLAC_0.dbg')
    if not os.path.exists(FASTresultFile):
        ManipulateTXTFile('ROSCO_v2d6.IN', '1 ! FlagLAC', '0 ! FlagLAC')  # disable LAC
        os.system(FASTexeFile + ' ' + SimulationName + '.fst')
        shutil.move(SimulationName + '.outb', FASTresultFile)  # store .outb file
        shutil.move(SimulationName + '.RO.dbg', ROSCOresultFile)  # store rosco output file

    # Run FB+FF
    FASTresultFile = os.path.join(SimulationFolderLAC, f'{WindFileName}_FlagLAC_1.outb')
    ROSCOresultFile = os.path.join(SimulationFolderLAC, f'{WindFileName}_FlagLAC_1.dbg')
    if not os.path.exists(FASTresultFile):
        ManipulateTXTFile('ROSCO_v2d6.IN', '0 ! FlagLAC', '1 ! FlagLAC')  # enable LAC
        os.system(FASTexeFile + ' ' + SimulationName + '.fst')
        shutil.move(SimulationName + '.outb', FASTresultFile)  # store .outb file
        shutil.move(SimulationName + '.RO.dbg', ROSCOresultFile)  # store rosco output file

    # Reset the InflowWind file again
    ManipulateTXTFile('IEA-15-240-RWT_InflowFile.dat', WindFileRoot, 'MyFilenameRoot')

# Clean up
os.remove(FASTexeFile)

# Postprocessing: evaluate data

# Allocation
S_RotSpeed_FB_est = np.empty((nSeed, int(nFFT/2+1)))
S_RotSpeed_FBFF_est = np.empty((nSeed, int(nFFT/2+1)))
S_LL_est = np.empty((nSeed, int(nFFT/2+1)))
S_RR_est = np.empty((nSeed, int(nFFT/2+1)))
S_RL_est = np.empty((nSeed, int(nFFT/2+1)), dtype=complex)
STD_RotSpeed_FB = np.empty(nSeed)
STD_RotSpeed_FBFF = np.empty(nSeed)
c_filter = np.empty((nSeed, int(AnalysisTime*Fs*2+1)))
c_RL = np.empty((nSeed, int(AnalysisTime*Fs*2+1)))
MAE = np.empty(nSeed)                                               # mean absolute error [m/s]

# Loop over all seeds
for iSeed in range(nSeed):

    # Load data
    Seed = Seed_vec[iSeed]
    WindFileName = f'URef_18_Seed_{Seed:02d}'
    FASTresultFile = os.path.join(SimulationFolderFB, f'{WindFileName}_FlagLAC_0.outb')
    ROSCOresultFile = os.path.join(SimulationFolderFB, f'{WindFileName}_FlagLAC_0.dbg')
    FB = ReadFASTbinaryIntoStruct(FASTresultFile)
    R_FB = ReadROSCOtextIntoDataframe(ROSCOresultFile)
    FASTresultFile = os.path.join(SimulationFolderLAC, f'{WindFileName}_FlagLAC_1.outb')
    ROSCOresultFile = os.path.join(SimulationFolderLAC, f'{WindFileName}_FlagLAC_1.dbg')
    FBFF = ReadFASTbinaryIntoStruct(FASTresultFile)
    R_FBFF = ReadROSCOtextIntoDataframe(ROSCOresultFile)

    # Plot rotor speed
    plt.figure(f'Rotor speed seed {Seed}')
    plt.grid(True)
    plt.plot(FB['Time'], FB['RotSpeed'])
    plt.plot(FBFF['Time'], FBFF['RotSpeed'])
    plt.ylabel('RotSpeed [rpm]')
    plt.legend(['feedback only', 'feedback-feedforward'])
    plt.xlabel('time [s]')

    # Estimate rotor speed spectra
    f_est, S_RotSpeed_FB_est[iSeed, :] = signal.welch(
        signal.detrend(FB['RotSpeed'][FB['Time'] > t_start], type='constant'), fs=Fs, window=vWindow, noverlap=nOverlap,
        nfft=nFFT)
    _, S_RotSpeed_FBFF_est[iSeed, :] = signal.welch(
        signal.detrend(FBFF['RotSpeed'][FBFF['Time'] > t_start], type='constant'), fs=Fs, window=vWindow,
        noverlap=nOverlap, nfft=nFFT)

    # Calculate standard deviation rotor speed
    STD_RotSpeed_FB[iSeed] = np.std(FB['RotSpeed'][FB['Time'] > t_start])
    STD_RotSpeed_FBFF[iSeed] = np.std(FBFF['RotSpeed'][FBFF['Time'] > t_start])

    # Estimate auto- and cross-spectra of REWS
    TurbSimResultFile = 'TurbulentWind/URef_18_Seed_{:02d}.wnd'.format(Seed)
    REWS_WindField, Time_WindField = CalulateREWSfromWindField(TurbSimResultFile, R, 2)
    REWS_WindField_Fs = interp1d(Time_WindField.ravel(),REWS_WindField.ravel())(R_FBFF['Time']) # get REWS with the same time step as simulations
    _, S_LL_est[iSeed, :] = signal.welch(
        signal.detrend(R_FBFF['REWS'][R_FBFF['Time'] >= t_start], type='constant'),
        fs=Fs, window=vWindow, noverlap=nOverlap, nfft=nFFT)
    _, S_RR_est[iSeed, :] = signal.welch(
        signal.detrend(REWS_WindField_Fs[R_FBFF['Time'] >= t_start], type='constant'),
        fs=Fs, window=vWindow, noverlap=nOverlap, nfft=nFFT)
    _, S_RL_est[iSeed, :] = signal.csd(signal.detrend(REWS_WindField_Fs[R_FBFF['Time'] >= t_start], type='constant'),
                                       signal.detrend(R_FBFF['REWS'][R_FBFF['Time'] >= t_start],
                                                      type='constant'),
                                       fs=Fs, window=vWindow, noverlap=nOverlap, nfft=nFFT)

    # Estimate cross correlation between filtered and unfiltered REWS from lidar
    REWS_f_detrended = signal.detrend(R_FBFF['REWS_f'][R_FBFF['Time'] >= t_start], type='constant')
    REWS_detrended = signal.detrend(R_FBFF['REWS'][R_FBFF['Time'] >= t_start], type='constant')

    c_filter[iSeed, :] = (signal.correlate(REWS_f_detrended, REWS_detrended, mode='full', method='auto')
                                / (np.linalg.norm(REWS_f_detrended) * np.linalg.norm(REWS_detrended)))

    # Estimate cross correlation between rotor and lidar
    REWS_WindField_Fs_detrended = signal.detrend(REWS_WindField_Fs[R_FBFF['Time'] >= t_start], type='constant')
    REWS_b_detrended = signal.detrend(R_FBFF['REWS_b'][R_FBFF['Time'] >= t_start])

    c_RL[iSeed, :] = (signal.correlate(REWS_WindField_Fs_detrended, REWS_b_detrended, mode='full', method='auto')
                            / (np.linalg.norm(REWS_WindField_Fs_detrended * np.linalg.norm(REWS_b_detrended))))

    lags = np.arange(-AnalysisTime * Fs, AnalysisTime * Fs + 1)

    # Calculate mean absolute error
    REWS_WindField_Fs_shifted = interp1d(Time_WindField.ravel() - tau, REWS_WindField.ravel())(R_FBFF['Time']) # shift the REWS from wind field by tau (intented prediction time) into the future (lower times)
    Error = REWS_WindField_Fs_shifted - R_FBFF['REWS_b'] # error is  REWS from wind field shifted minus REWS from lidar filtered and buffered.
    MAE[iSeed] = np.mean(abs(Error[R_FBFF['Time'] >= t_start])) # only consider error after t_start

    # Plot REWS for absolute error
    plt.figure('REWS seed {}'.format(Seed))
    plt.subplot(311)
    plt.plot(R_FBFF['Time'], REWS_WindField_Fs, label='wind field')
    plt.plot(R_FBFF['Time'], R_FBFF['REWS'], label='lidar estimate')
    plt.ylabel('REWS [m/s]')
    plt.legend()
    plt.grid(True)
    plt.subplot(312)
    plt.plot(R_FBFF['Time'], REWS_WindField_Fs_shifted, label='wind field shifted')
    plt.plot(R_FBFF['Time'], R_FBFF['REWS_b'], label='lidar estimate filtered and buffered')
    plt.ylabel('REWS [m/s]')
    plt.legend()
    plt.grid(True)
    plt.subplot(313)
    plt.plot(R_FBFF['Time'], Error)
    plt.ylabel('error [m/s]')
    plt.xlabel('time [s]')
    plt.grid(True)

# Calculate mean coherence
gamma2_RL_mean_est = np.abs(np.mean(S_RL_est, axis=0)) ** 2 / np.mean(S_LL_est, axis=0) / np.mean(S_RR_est, axis=0)

# Get analytical correlation model
SpectralModelFileName = '../AnalyticalModel/LidarRotorSpectra_IEA15MW_4BeamPulsed.mat'  # model for 18 m/s
AnalyticalModel = loadmat(SpectralModelFileName)
AnalyticalModel['gamma2_RL'] = np.abs(AnalyticalModel['S_RL']) ** 2 / AnalyticalModel['S_RR'] / AnalyticalModel[
    'S_LL']

# Plot rotor speed spectra
plt.figure('Rotor speed spectra')
plt.grid(True)
plt.plot(f_est, np.mean(S_RotSpeed_FB_est, axis=0))
plt.plot(f_est, np.mean(S_RotSpeed_FBFF_est, axis=0))
plt.xscale('log')
plt.yscale('log')
plt.xlabel('frequency [Hz]')
plt.ylabel('Spectra RotSpeed [(rpm)^2/Hz]')
plt.legend(['FB-only Estimated', 'FBFF Estimated'])

# display results
print('Change in rotor speed standard deviation:  %4.1f %%\n' % (
        (np.mean(STD_RotSpeed_FBFF) / np.mean(STD_RotSpeed_FB) - 1) * 100))

# Plot REWS spectra
plt.figure('REWS spectra')
plt.grid(True)
plt.plot(AnalyticalModel['f'], AnalyticalModel['S_LL'])
plt.plot(AnalyticalModel['f'], AnalyticalModel['S_RR'])
plt.plot(f_est, np.mean(S_LL_est, axis=0))
plt.plot(f_est, np.mean(S_RR_est, axis=0))
plt.xscale('log')
plt.yscale('log')
plt.xlabel('frequency [Hz]')
plt.ylabel('Spectra REWS [(m/s)^2/Hz]')
plt.legend(['Lidar Analytical', 'Rotor Analytical', 'Lidar Estimated', 'Rotor Estimated'])

# Plot filter delay
c_filter_mean = np.mean(c_filter, axis=0)
c_max, idx_max = np.max(c_filter_mean), np.argmax(c_filter_mean)
T_filter = lags[idx_max] / Fs  # [s]       time delay by the filter
plt.figure('Filter delay')
plt.grid(True)
plt.plot(lags / Fs, c_filter_mean)
plt.plot(T_filter, c_max, 'o')
plt.xlim([0, 10])
plt.ylim([0.8, 1.0])
plt.xlabel('time [s]')
plt.ylabel('cross correlation [-]')

# Plot cross-correlation between rotor and lidar
c_RL_mean = np.mean(c_RL, axis=0)
c_max, idx_max = np.max(c_RL_mean), np.argmax(c_RL_mean)
T_RL = lags[idx_max] / Fs
plt.figure('Cross-correlation between rotor and lidar')
plt.grid(True)
plt.plot(lags / Fs, c_RL_mean)
plt.plot(T_RL, c_max, 'o')
plt.xlim([-5, 5])
plt.ylim([0.8, 1.0])
plt.xlabel('time [s]')
plt.ylabel('cross correlation [-]')

# Plot REWS coherence
plt.figure('REWS coherence')
plt.grid(True)
plt.plot(AnalyticalModel['f'], AnalyticalModel['gamma2_RL'])
plt.plot(f_est[1:], gamma2_RL_mean_est[1:])
plt.xscale('log')
plt.xlabel('frequency [Hz]')
plt.ylabel('Coherence REWS [-]')
plt.legend(['Analytical', 'Estimated'])
plt.show()

# Get parameters for FFP_v1_4BeamPulsed.in
G_RL = AnalyticalModel['S_RL']/AnalyticalModel['S_LL']                                          # [-]       transfer function
f_cutoff = interp1d(np.abs(G_RL.ravel()),AnalyticalModel['f'].ravel())(10**(-3/20))*2*np.pi     # [rad/s]   desired cutoff (-3dB) angular frequency
URef = 18                                                                                       # [m/s]     mean wind speed
x_L = 160                                                                                       # [m]       distance of lidar measurement
T_Taylor = x_L/URef                                                                             # [s]       travel time from lidar measurment to rotor
T_scan = 1                                                                                      # [s]       time of full lidar scan
T_buffer = T_Taylor-1/2*T_scan-T_filter-tau                                                     # [s]       time needed to buffer signal such that FF signal is applied with tau, see Schlipf2015, Equation (5.40)

# Calculation of Cost for Summer Games 2024
Cost = np.mean(MAE)
print('Cost for Summer Games 2024 ("18 m/s hurdles"): {:.6f}'.format(Cost))
