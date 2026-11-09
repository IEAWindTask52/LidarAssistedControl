% Function: OverwriteLineTXTFile overwrites line to .txt file
% -----------------------------
% Usage:
% OverwriteLineTXTFile(TXTFile,nLine,NewLine)
% -----------------------------
% Input:
% TXTFile           String with name of .txt file
% nLine             line where the new line is added
% NewLine           String with new line
% -----------------------------
function OverwriteLineTXTFile(TXTFile,nLine,NewLine)

[FOLDER,NAME,EXT]   = fileparts(TXTFile); 
TempTXTFile         = fullfile(FOLDER,[NAME,'_temp',EXT]);
fid                 = fopen(TXTFile);
fidTemp             = fopen(TempTXTFile,'w+');

% copy file up to before nLine
for iLine=1:nLine-1
   s            = fgetl(fid);
   fprintf(fidTemp,'%s\r\n',s);
end

% write new line
fprintf(fidTemp,'%s\r\n',NewLine);

% skip old line
fgetl(fid);

% copy rest of file
while ~feof(fid)
   s            = fgetl(fid);
   fprintf(fidTemp,'%s\r\n',s);
end

% close files
fclose(fid);
fclose(fidTemp);
delete(TXTFile);
movefile(TempTXTFile,TXTFile);