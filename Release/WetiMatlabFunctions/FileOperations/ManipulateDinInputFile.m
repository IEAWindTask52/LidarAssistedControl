% Function: ManipulateDinInputFile String replacement in Rediscon input file.
% Main difference to ManipulateFastInputFile: Identifier is in front of values.
% -----------------------------
% Usage:
% n = ManipulateDinInputFile(TXTFile,Identifier,NewString)
% -----------------------------
% Input:
% TXTFile           String with name of .txt file
% Identifier        Identifier in FAST style input file 
% NewString         String with replacement
% -----------------------------
% Output:
% n                 number of replacements
% -----------------------------
% Created: 
% David Schlipf on 30-Mar-2025
% (c) WETI
% ----------------------------------
function n = ManipulateDinInputFile(TXTFile,Identifier,NewString)

[FOLDER,NAME,EXT]   = fileparts(TXTFile); 
TempTXTFile         = fullfile(FOLDER,[NAME,'_temp',EXT]);
fid                 = fopen(TXTFile);
fidTemp             = fopen(TempTXTFile,'w+');
n                   = 0;

while ~feof(fid)
    CurrentLine   	= fgetl(fid);
    % make sure only whole word are found (case sensitive)
    EndIdx        = regexp(CurrentLine, [Identifier,'(\s|$)'], 'end', 'once');
    if isempty(EndIdx)
        fprintf(fidTemp,'%s\r\n',CurrentLine);
    else
        NewLine = [CurrentLine(1:EndIdx),' ',NewString];
        fprintf(fidTemp,'%s\r\n',NewLine);
        n = n+1;
    end
end

fclose(fid);
fclose(fidTemp);
delete(TXTFile);
movefile(TempTXTFile,TXTFile);