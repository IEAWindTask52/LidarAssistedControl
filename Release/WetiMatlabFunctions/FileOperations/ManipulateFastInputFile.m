% Function: ManipulateFastInputFile String replacement in FAST Input file
% -----------------------------
% Usage:
% n = ManipulateFastInputFile(TXTFile,Identifier,NewString)
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
% David Schlipf on 20-Dec-2022
% (c) WETI
% ----------------------------------
function n = ManipulateFastInputFile(TXTFile,Identifier,NewString)

[FOLDER,NAME,EXT]   = fileparts(TXTFile); 
TempTXTFile         = fullfile(FOLDER,[NAME,'_temp',EXT]);
fid                 = fopen(TXTFile);
fidTemp             = fopen(TempTXTFile,'w+');
n                   = 0;

while ~feof(fid)
    CurrentLine   	= fgetl(fid);
    % make sure only whole word are found (case sensitive)
    StartIdx        = regexp(CurrentLine, ['\s',Identifier,'(\s|$)'], 'start', 'once')+1;
    if isempty(StartIdx)
        fprintf(fidTemp,'%s\r\n',CurrentLine);
    else
        NewLine = [NewString,' ',CurrentLine(StartIdx:end)];
        fprintf(fidTemp,'%s\r\n',NewLine);
        n = n+1;
    end
end

fclose(fid);
fclose(fidTemp);
delete(TXTFile);
movefile(TempTXTFile,TXTFile);