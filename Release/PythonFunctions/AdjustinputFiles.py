import os
import shutil
from ManipulateFastInputFile import ManipulateFastInputFile
from ManipulateTXTFile import ManipulateTXTFile
from AddLineToTXTFile import AddLineToTXTFile
def adjust_input_files(input_files, modifications, simulation_folder):
    n_file = len(input_files)
    n_modification = len(modifications)

    for i_file in range(n_file):
        template_file = input_files[i_file][0]
        new_file = input_files[i_file][1]
        shutil.copyfile(os.path.join(simulation_folder, template_file), os.path.join(simulation_folder, new_file))

    for i_modification in range(n_modification):
        i_file = int(modifications[i_modification][0])
        mode = modifications[i_modification][1]
        new_file = input_files[i_file][1]
        txt_file = os.path.join(simulation_folder, new_file)
        if mode == 'I':  # Identifier
            identifier = modifications[i_modification][2]
            new_string = modifications[i_modification][3]
            ManipulateFastInputFile(txt_file, identifier, new_string)
        elif mode == 'R':  # Replace
            string_to_replace = modifications[i_modification][2]
            new_string = modifications[i_modification][3]
            ManipulateTXTFile(txt_file, string_to_replace, new_string)
        elif mode == 'A':  # Add line
            n_line = int(modifications[i_modification][2])
            new_line = modifications[i_modification][3]
            AddLineToTXTFile(txt_file, n_line, new_line)
