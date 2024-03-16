import numpy as np
import struct

def ReadBLgrid(FileName):
    """
       Reads data from a BL grid file.
        Authors: Aravind Venkatachalapathy, Simon Weich
       Args:
           FileName (str): Name of the file to read (.wnd extension is optional).

       Returns:
           tuple: Contains the following elements:
               velocity (numpy.ndarray): 4-D array with dimensions (time, component, iy, iz) representing velocity data.
               y (numpy.ndarray): 1-D array of horizontal locations y(iy).
               z (numpy.ndarray): 1-D array of vertical locations z(iz).
               nz (int): Number of points in the vertical direction of the grid.
               ny (int): Number of points in the horizontal direction of the grid.
               dz (float): Distance between two points in the vertical dimension [m].
               dy (float): Distance between two points in the horizontal dimension [m].
               dt (float): Time interval between data points [s].
               zHub (float): Hub height [m].
               z1 (float): Vertical location of the bottom of the grid [m above ground level].
               SumVars (dict): Dictionary containing variables from the summary file (zHub, Clockwise, UBAR, TI_u, TI_v, TI_w).
       """
    # Initialize variables
    fileFmt  = 'int16'
    ConvFact = 1.0                                                              # results in meters and seconds

    str      = ['HUB HEIGHT', 'CLOCKWISE', 'UBAR', 'TI(U', 'TI(V', 'TI(W']      # MUST be in UPPER case
    numVars  = len(str)
    SummVars = np.zeros(numVars)

    # Check file extension
    if FileName[-4:].lower() == '.wnd':
        FileName = FileName[:-4]

    # Open the file
    with open(FileName + '.wnd', 'rb') as fid_wnd:
        nffc = np.fromfile(fid_wnd, dtype=np.int16, count=1)[0]                  # number of components

        if nffc != -99:  # AN OLD-STYLE AERODYN WIND FILE
          raise ValueError("Please use a newer-style AERODYN wind file.")
        else:  # THE NEWER-STYLE AERODYN WIND FILE
            fc = np.fromfile(fid_wnd, dtype=np.int16, count=1)[0]                # should be 4 to allow turbulence intensity to be stored in the header

            nffc = np.fromfile(fid_wnd, dtype=np.int32, count=1)[0]              # number of components (should be 3)
            lat = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]             # latitude (deg)
            z0 = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]              # Roughness length (m)
            zOffset = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]         # Reference height (m) = Z(1) + GridHeight / 2.0
            TI_U = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]            # Turbulence Intensity of u component (%)
            TI_V = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]            # Turbulence Intensity of v component (%)
            TI_W = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]            # Turbulence Intensity of w component (%)

            dz = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]              # delta z in m
            dy = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]              # delta y in m
            dx = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]              # delta x in m
            nt = np.fromfile(fid_wnd, dtype=np.int32, count=1)[0]                # half the number of time steps
            MFFWS = np.fromfile(fid_wnd, dtype=np.float32, count=1)[0]           # mean full-field wind speed

            fid_wnd.read(3 * 4)                                                  # unused variables (for BLADED)
            fid_wnd.read(2 * 4)                                                  # unused variables (for BLADED)
            nz = np.fromfile(fid_wnd, dtype=np.int32, count=1)[0]                # number of points in vertical direction
            ny = np.fromfile(fid_wnd, dtype=np.int32, count=1)[0]                # number of points in horizontal direction
            fid_wnd.read(3 * (nffc - 1) * 4)                                     # unused variables (for BLADED)

            SummVars[2:6] = [MFFWS, TI_U, TI_V, TI_W]

    nt = max([nt * 2, 1])
    dt = dx / MFFWS

    # READ THE SUMMARY FILE FOR SCALING FACTORS
    print('Reading the summary file....')

    indx = SummVars
    with open(FileName + '.sum', 'r') as fid_sum:
        while any(indx == 0):                                                   # MFFWS and the TIs should not be zero
            line = fid_sum.readline()
            if not line:
                # We reached the end of the file
                print('Reached the end of summary file without all necessary data.')
                return

            line = line.upper()
            findx = line.find('=') + 1  # first index
            if findx == 0:
                findx = 1
            lindx = len(line)  # last index

            i = 0
            while i < numVars:
                if indx[i] == 0:
                    k = line.find(str[i])
                    if k != -1:                                                  # we found a string we're looking for
                        indx[i] = k
                        k = line.find('%')
                        if k != -1:
                            lindx = max(findx, k - 1)

                        tmp = line[findx:lindx].split()[0]
                        try:
                            # Try to convert the string to a float
                            SummVars[i] = float(tmp)
                        except ValueError:
                            # If the conversion fails, check if the string starts with 'T'
                            if tmp[0].upper() == 'T':
                                SummVars[i] = 1
                            else:
                                SummVars[i] = -1  # use this for false instead of zero.
                i += 1

    # read the rest of the file to get the grid height offset, if it's there
    ZGoffset = 0.0
    with open(FileName + '.sum', 'r') as fid_sum:
        while True:
            line = fid_sum.readline()

            if not line:
                 break

            line = line.upper()
            findx = line.find('HEIGHT OFFSET')

            if findx != -1:
                lindx = len(line)
                findx = line.find('=') + 1
                ZGoffset = float(line[findx:lindx].split()[0])  # z grid offset
                break

    fid_sum.close()

    # READ THE GRID DATA FROM THE BINARY FILE
    print('Reading and scaling the grid data...')

    nv = nffc * ny * nz  # the size of one time step
    Scale = 0.00001 * SummVars[2] * SummVars[3:6]
    Offset = [SummVars[2], 0, 0]

    velocity = np.zeros((nt, nffc, ny, nz))

    if SummVars[1] > 0:  # clockwise rotation
        # flip the y direction....
        y_ix = list(range(ny, 0, -1))
    else:
        y_ix = list(range(1, ny + 1))

    with open(FileName + '.wnd', 'rb') as fid_wnd:
        fid_wnd.read(26 * 4)

        for it in range(nt):
            v = np.fromfile(fid_wnd, dtype=fileFmt, count=nv)
            if len(v) < nv:
                print(f'Could not read entire file: at grid record {it * nv + len(v)} of {nv * nt}')
                return

            cnt2 = 0
            for iz in range(nz):
                for iy in y_ix:
                    for k in range(nffc):
                        velocity[it, k, iy - 1, iz] = v[cnt2] * Scale[k] + Offset[k]
                        cnt2 += 1

    fid_wnd.close()

    y = np.array([i * dy - dy * (ny - 1) / 2 for i in range(ny)])

    zHub = SummVars[0]
    z1 = zHub - ZGoffset - dz * (nz - 1) / 2  # this is the bottom of the grid
    z = np.array([i * dz + z1 for i in range(nz)])

    print('Finished.')
    print('')
    return velocity, y, z, nz, ny, dz, dy, dt, zHub, z1, SummVars,zOffset, z0


