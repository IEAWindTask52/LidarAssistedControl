import os

def AddLineToTXTFile(txt_file, n_line, new_line):
    folder, name = os.path.split(txt_file)
    name, ext = os.path.splitext(name)
    temp_txt_file = os.path.join(folder, f'{name}_temp{ext}')

    with open(txt_file, 'r') as fid, open(temp_txt_file, 'w+') as fid_temp:
        # copy file up to n_line
        for i_line in range(n_line):
            s = fid.readline()
            fid_temp.write(s)

        # add new line
        fid_temp.write(new_line + '\n')

        # copy rest of file
        for s in fid:
            fid_temp.write(s)

    # delete original file and rename temp file
    os.remove(txt_file)
    os.rename(temp_txt_file, txt_file)
