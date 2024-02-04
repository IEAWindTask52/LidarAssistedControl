% Function: AddLineToTXTFile add lines to .txt file
% -----------------------------
% Usage:
% AddLineToTXTFile(TXTFile,nLine,NewLine)
% -----------------------------
% Input:
% TXTFile           String with name of .txt file
% nLine             line after which the new line is added
% NewLine           String with new line
% -----------------------------
function AddLineToTXTFile(TXTFile,nLine,NewLine)

[FOLDER,NAME,EXT]   = fileparts(TXTFile); 
TempTXTFile         = fullfile(FOLDER,[NAME,'_temp',EXT]);
fid                 = fopen(TXTFile);
fidTemp             = fopen(TempTXTFile,'w+');

% copy file up to nLine
for iLine=1:nLine
   s            = fgetl(fid);
   fprintf(fidTemp,'%s\r\n',s);
end

% add new line
fprintf(fidTemp,'%s\r\n',NewLine);

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