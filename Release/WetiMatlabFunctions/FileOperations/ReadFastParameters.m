% Function: ReadFastParameters reads out parameters from FAST Input file
% -----------------------------
% Usage:
% [Value,n]  = ReadFastParameters(TxtFile,Identifier)
% -----------------------------
% Input:
% TxtFile           String with name of .txt file
% Identifier        Identifier in FAST style input file 
% -----------------------------
% Output:
% Value             String with Value     
% n                 number of replacements
% -----------------------------
% Created: 
% David Schlipf on 15-Jan-2023
% (c) WETI
% ----------------------------------
function [Value,n]  = ReadFastParameters(TxtFile,Identifier)
fid                 = fopen(TxtFile);
n                   = 0;

while ~feof(fid)
    CurrentLine   	= fgetl(fid);
    StartIdx        = strfind(CurrentLine,Identifier);
    if ~isempty(StartIdx)
        Value       = strtrim(CurrentLine(1:StartIdx-1));
        n           = n+1;
    end
end

fclose(fid);
end