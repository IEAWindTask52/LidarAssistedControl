import numpy as np


def ReadFASTbinary(FileName, machinefmt='native'):
    FileFmtID = {'WithTime': 1, 'WithoutTime': 2, 'NoCompressWithoutTime': 3, 'ChanLen_In': 4}

    with open(FileName, 'rb') as fid:
        FileID = np.fromfile(fid, dtype=np.int16, count=1, sep='')[0]

        if FileID == FileFmtID['ChanLen_In']:
            LenName = np.fromfile(fid, dtype=np.int16, count=1, sep='')[0]
        else:
            LenName = 10

        NumOutChans = np.fromfile(fid, dtype=np.int32, count=1, sep='')[0]
        NT = np.fromfile(fid, dtype=np.int32, count=1, sep='')[0]

        if FileID == FileFmtID['WithTime']:
            TimeScl = np.fromfile(fid, dtype=np.float64, count=1, sep='')[0]
            TimeOff = np.fromfile(fid, dtype=np.float64, count=1, sep='')[0]
        else:
            TimeOut1 = np.fromfile(fid, dtype=np.float64, count=1, sep='')[0]
            TimeIncr = np.fromfile(fid, dtype=np.float64, count=1, sep='')[0]

        if FileID == FileFmtID['NoCompressWithoutTime']:
            ColScl = np.ones(NumOutChans)
            ColOff = np.zeros(NumOutChans)
        else:
            ColScl = np.fromfile(fid, dtype=np.float32, count=NumOutChans, sep='')
            ColOff = np.fromfile(fid, dtype=np.float32, count=NumOutChans, sep='')

        LenDesc = np.fromfile(fid, dtype=np.int32, count=1, sep='')[0]
        DescStrASCII = np.fromfile(fid, dtype=np.uint8, count=LenDesc, sep='')
        DescStr = DescStrASCII.tobytes().decode('utf-8').strip()

        ChanName = [None] * (NumOutChans + 1)
        for iChan in range(NumOutChans + 1):
            ChanNameASCII = np.fromfile(fid, dtype=np.uint8, count=LenName, sep='')
            ChanName[iChan] = ChanNameASCII.tobytes().decode('utf-8').strip()

        ChanUnit = [None] * (NumOutChans + 1)
        for iChan in range(NumOutChans + 1):
            ChanUnitASCII = np.fromfile(fid, dtype=np.uint8, count=LenName, sep='')
            ChanUnit[iChan] = ChanUnitASCII.tobytes().decode('utf-8').strip()

        print(f'Reading from the file {FileName} with heading: ')
        print(f'   "{DescStr}".')

        nPts = NT * NumOutChans
        Channels = np.zeros((NT, NumOutChans + 1))

        if FileID == FileFmtID['WithTime']:
            PackedTime = np.fromfile(fid, dtype=np.int32, count=NT, sep='')
            if len(PackedTime) < NT:
                raise ValueError(f'Could not read entire {FileName} file: read {len(PackedTime)} of {NT} time values.')

        if FileID == FileFmtID['NoCompressWithoutTime']:
            PackedData = np.fromfile(fid, dtype=np.float64, count=nPts, sep='')
        else:
            PackedData = np.fromfile(fid, dtype=np.int16, count=nPts, sep='')

        if len(PackedData) < nPts:
            raise ValueError(f'Could not read entire {FileName} file: read {len(PackedData)} of {nPts} values.')

        for it in range(NT):
            Channels[it, 1:] = (PackedData[NumOutChans * it: NumOutChans * (it + 1)] - ColOff) / ColScl

        if FileID == FileFmtID['WithTime']:
            Channels[:, 0] = (PackedTime - TimeOff) / TimeScl
        else:
            Channels[:, 0] = TimeOut1 + TimeIncr * np.arange(NT)

    return Channels, ChanName, ChanUnit, FileID, DescStr
