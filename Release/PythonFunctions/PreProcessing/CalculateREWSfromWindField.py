import numpy as np
from FileOperations.ReadBLgrid import ReadBLgrid



def CalulateREWSfromWindField(file_name, R, n_loop):
    # read in wind field
    velocity, y, z, nz, ny, dz, dy, dt, zHub, z1, SummVars, zOffset, z0 = ReadBLgrid(file_name)
    h = SummVars[0]
    Y, Z = np.meshgrid(y, z - h)
    distance_to_hub = np.sqrt(Y.flatten() ** 2 + Z.flatten() ** 2)
    is_in_rotor_disc = distance_to_hub <= R

    # get rotor-effective wind speed
    n_t_wf = velocity.shape[0]
    v_0_wf = np.empty((n_t_wf, 1))

    for i_t in range(n_t_wf):
        current_wind = np.squeeze(velocity[i_t, 0, :, :])
        wind_field = current_wind.flatten()[is_in_rotor_disc.flatten()]
        v_0_wf[i_t, 0] = np.mean(wind_field)

    # combine the REWS n_loop times
    t = dt * np.arange(0, n_t_wf * n_loop)
    v_0 = np.tile(v_0_wf, (n_loop, 1))

    return v_0, t



