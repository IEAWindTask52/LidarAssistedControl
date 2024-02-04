function AdjustInputFiles(InputFiles,Modifications,SimulationFolder)

n_File                  = size(InputFiles,1);
n_Modification          = size(Modifications,1);    

for i_File = 1:n_File
    TemplateFile        = InputFiles{i_File,1};
    NewFile             = InputFiles{i_File,2};
    copyfile(fullfile(SimulationFolder,TemplateFile),fullfile(SimulationFolder,NewFile));
end

for i_Modification = 1:n_Modification
    i_File                  = str2double(Modifications{i_Modification,1});
    Mode                    = Modifications{i_Modification,2};
    NewFile                 = InputFiles{i_File,2};
    TXTFile                 = fullfile(SimulationFolder,NewFile);
    switch Mode
        case 'I' % Identifier
            Identifier      = Modifications{i_Modification,3};
            NewString       = Modifications{i_Modification,4};
            ManipulateFastInputFile(TXTFile,Identifier,NewString);
        case 'R' % Replace
            StringToReplace = Modifications{i_Modification,3};
            NewString       = Modifications{i_Modification,4};
            ManipulateTXTFile(TXTFile,StringToReplace,NewString); 
        case 'A' % Add line
            nLine           = str2double(Modifications{i_Modification,3});
            NewLine         = Modifications{i_Modification,4};            
            AddLineToTXTFile(TXTFile,nLine,NewLine)
    end
end

end