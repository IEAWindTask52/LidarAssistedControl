import numpy as np


def WriteBLgrid(filename, velocity, dy, dz, dt, z_offset, z0, summ_vars):
    """
    Python translation of the MATLAB function WriteBLgrid.m (TurbSim / NREL).

    This function writes a 4D wind velocity field to a Bladed-style full-field
    binary wind file (.wnd) that can be used by OpenFAST/InflowWind.

    Parameters
    ----------
    filename : str
        Base name of the output .wnd file ('.wnd' will be added if missing).
    velocity : ndarray
        4D array with shape (nt, nffc, ny, nz):
            nt   : number of time steps
            nffc : number of wind components (usually 3: u, v, w)
            ny   : number of grid points in lateral direction
            nz   : number of grid points in vertical direction
    dy : float
        Grid spacing in lateral (y) direction [m].
    dz : float
        Grid spacing in vertical (z) direction [m].
    dt : float
        Time step [s].
    z_offset : float
        Reference height of the grid [m] = Z(1) + GridHeight / 2.0.
    z0 : float
        Roughness length [m].
    summ_vars : array-like
        Vector of 6 values from the summary file:
        [zHub, ClockwiseFlag, UBar, TI_u, TI_v, TI_w]
    """

    # Binary format for the velocity buffer (int16, little-endian)
    file_fmt = np.dtype("<i2")

    # Header constants
    fc = 4        # format code: 4 allows turbulence intensity in the header
    lat = 0.0     # latitude [deg]

    # Unpack summary variables (MATLAB: SummVars(1..6))
    summ_vars = np.asarray(summ_vars, dtype=float)
    z_hub = summ_vars[0]        # hub height [m] (not used further, kept for completeness)
    clockwise = summ_vars[1]    # >0 => clockwise rotation when looking downwind
    mffws = summ_vars[2]        # mean full-field wind speed
    ti_u = summ_vars[3]         # TI of u component [%]
    ti_v = summ_vars[4]         # TI of v component [%]
    ti_w = summ_vars[5]         # TI of w component [%]

    # Dimensions of the wind field
    velocity = np.asarray(velocity, dtype=float)
    nt, nffc, ny, nz = velocity.shape  # MATLAB: [nt, nffc, ny, nz] = size(velocity)

    # Bottom of the grid (not used later, kept for parity with MATLAB code)
    z1 = z_hub - dz * (nz - 1) / 2.0  # noqa: F841

    # Spatial step in x-direction based on mean wind speed
    dx = dt * mffws
    # Half the number of time steps
    nt_header = nt // 2

    # Normalize filename and strip existing ".wnd" if present
    if filename.lower().endswith(".wnd"):
        filename = filename[:-4]
    full_name = filename + ".wnd"

    # Open file for binary writing
    with open(full_name, "wb") as f:
        # ----------------------------------------------------
        # WRITE HEADER (Newer-style AeroDyn / Bladed format)
        # ----------------------------------------------------
        # int16: -99, fc
        np.array([-99, fc], dtype="<i2").tofile(f)
        # int32: number of components (nffc)
        np.array([nffc], dtype="<i4").tofile(f)
        # float32: latitude, z0, z_offset, TI_u, TI_v, TI_w
        np.array([lat, z0, z_offset, ti_u, ti_v, ti_w], dtype="<f4").tofile(f)
        # float32: dz, dy, dx
        np.array([dz, dy, dx], dtype="<f4").tofile(f)
        # int32: half the number of time steps
        np.array([nt_header], dtype="<i4").tofile(f)
        # float32: mean full-field wind speed
        np.array([mffws], dtype="<f4").tofile(f)
        # float32: three unused variables (Bladed) -> zeros
        np.zeros(3, dtype="<f4").tofile(f)
        # int32: two unused variables (Bladed) -> zeros
        np.zeros(2, dtype="<i4").tofile(f)
        # int32: nz, ny
        np.array([nz, ny], dtype="<i4").tofile(f)
        # int32: 3*(nffc-1) unused integers (Bladed) -> zeros
        np.zeros(3 * (nffc - 1), dtype="<i4").tofile(f)

        # ----------------------------------------------------
        # WRITE GRID DATA
        # ----------------------------------------------------
        # Scaling and offset as in MATLAB:
        # Scale    = 0.00001 * UBar * [TI_u, TI_v, TI_w]
        # Offset   = [UBar, 0, 0]
        scale = 0.00001 * mffws * np.array([ti_u, ti_v, ti_w], dtype=float)
        offset = np.array([mffws, 0.0, 0.0], dtype=float)

        if np.any(scale == 0.0):
            raise ValueError(
                "Scale contains zero values; turbulence intensities must be non-zero "
                "to avoid division by zero."
            )

        # Flip y-direction if rotation is clockwise (as in MATLAB: ny:-1:1)
        if clockwise > 0:
            y_idx = range(ny - 1, -1, -1)
        else:
            y_idx = range(ny)

        # Buffer for one time step: length nz * ny * nffc
        v_buf = np.zeros(nz * ny * nffc, dtype=file_fmt)

        # Loop over time steps and write scaled velocity data
        for it in range(nt):
            cnt = 0
            for iz in range(nz):
                for iy in y_idx:
                    for k in range(nffc):
                        val = (velocity[it, k, iy, iz] - offset[k]) / scale[k]
                        v_buf[cnt] = np.int16(np.round(val))
                        cnt += 1

            v_buf.tofile(f)
