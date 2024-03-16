import numpy as np

def CalculatePitchTravel(data, time, start_time):
    pitch_rate = np.gradient(data,time)

    dt = time[1] - time[0]

    pitch_travel = np.sum(np.abs(pitch_rate[time >= start_time])) * dt

    return pitch_travel
