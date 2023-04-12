% Function: ManipulateTXTFile String replacement in .txt file
% -----------------------------
% Usage:
% n = ManipulateTXTFile(TXTFile,StringToReplace,NewString)
% -----------------------------
% Input:
% TXTFile           String with name of .txt file
% StringToReplace   String which has to be replaced
% NewString         String with replacement
% -----------------------------
% Output:
% n                 number of replacements
% -----------------------------
% Created: 
% David Schlipf on 05-Dec-2010
% (c) Universitaet Stuttgart and sowento GmbH
% ----------------------------------
function n = ManipulateTXTFile(TXTFile,StringToReplace,NewString)

[FOLDER,NAME,EXT]   = fileparts(TXTFile); 
TempTXTFile         = fullfile(FOLDER,[NAME,'_temp',EXT]);
fid                 = fopen(TXTFile);
fidTemp             = fopen(TempTXTFile,'w+');
n                   = 0;

while ~feof(fid)
   s            = fgetl(fid);
   sTemp        = strrep(s,StringToReplace,NewString);
   fprintf(fidTemp,'%s\r\n',sTemp);
   if ~strcmp(s,sTemp)
       n = n+1;
   end
end

fclose(fid);
fclose(fidTemp);
delete(TXTFile);
movefile(TempTXTFile,TXTFile);