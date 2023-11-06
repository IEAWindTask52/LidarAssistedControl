import os


def ManipulateTXTFile(txt_file, string_to_replace, new_string):
    folder, name_ext = os.path.split(txt_file)
    name, ext = os.path.splitext(name_ext)
    temp_txt_file = os.path.join(folder, name + '_temp' + ext)
    n = 0

    with open(txt_file, 'r') as fid, open(temp_txt_file, 'w') as fid_temp:
        for line in fid:
            s = line.strip()
            s_temp = s.replace(string_to_replace, new_string)
            fid_temp.write(s_temp + '\n')
            if s != s_temp:
                n += 1

    os.remove(txt_file)
    os.rename(temp_txt_file, txt_file)

    return n
# source: Matlab-Function (ManipulateTXTFile.m)