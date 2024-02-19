import os
import re

def ManipulateFastInputFile(TXTFile, Identifier, NewString):
    """
    String replacement in FAST Input file
    """
    FOLDER, NAME = os.path.split(TXTFile)
    NAME, EXT = os.path.splitext(NAME)
    TempTXTFile = os.path.join(FOLDER, NAME + '_temp' + EXT)

    n = 0
    with open(TXTFile, 'r') as fid, open(TempTXTFile, 'w+') as fidTemp:
        for line in fid:
            # make sure only whole word are found (case sensitive)
            match = re.search(r'\s' + Identifier + r'(\s|$)', line)
            if match is None:
                fidTemp.write(line)  # write the line as is, without adding a newline
            else:
                StartIdx = match.start() + 1
                NewLine = NewString + ' ' + line[StartIdx:]
                fidTemp.write(NewLine)  # write the new line, without adding a newline
                n += 1

    os.remove(TXTFile)
    os.rename(TempTXTFile, TXTFile)

    return n

