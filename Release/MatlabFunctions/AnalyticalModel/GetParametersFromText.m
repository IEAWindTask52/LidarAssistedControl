% Scan input text file to get the specific variable
% e.g. input
% VariableName     = 'WE_FOPoles_v';
% ROSCOInFileName  = 'ROSCO2.IN';
function   VlueFound = GetParametersFromText(TextInFileName,VariableName)


fid             = fopen(TextInFileName);


    while ~feof(fid)
       s            = fgetl(fid);
       if contains(s,VariableName )

          s_temp    = extractBefore(s,VariableName);
          if        s_temp(end) == '!'
                    s_temp    = extractBefore(s_temp,'!');
          end
          VlueFound = cell2mat(textscan(s_temp,'%f'));
          break
       end      
    end


fclose(fid);