% Function: GetValueFromFastInputFile 
% -----------------------------
% Usage:
% Value = GetValueFromFastInputFile(TXTFile,Identifier)
% -----------------------------
% Input:
% TXTFile           String with name of FAST style file
% Identifier        Identifier in FAST style input file 
% -----------------------------
% Output:
% Value             Value in front of Identifier
% -----------------------------
% Created: 
% David Schlipf on 21-Aug-2024
% (c) WETI
% ----------------------------------
function Value = GetValueFromFastInputFile(TXTFile,Identifier)

fid                 = fopen(TXTFile);

while ~feof(fid)
    CurrentLine   	= fgetl(fid);
    % make sure only whole word are found (case sensitive)
    StartIdx        = regexp(CurrentLine, ['\s',Identifier,'(\s|$)'], 'start', 'once')+1;
    if ~isempty(StartIdx)
        StrValue    = strtrim(CurrentLine(1:StartIdx-1)); % remove white space
        NumValue    = str2double(StrValue);
        if isnan(NumValue)
            Value   = StrValue;
        else
            Value   = NumValue;
        end
    end
end

fclose(fid);