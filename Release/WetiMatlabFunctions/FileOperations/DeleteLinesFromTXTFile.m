% Function: DeleteLinesFromTXTFile remove lines to a Text File
% -----------------------------
% SPB 30.01.2024
% Usage:
% inputFile = 'TXTFile.txt';
% linesToRemove = [2, 4, 7];
% 
% DeleteLinesFromTXTFile(TXTFile, linesToRemove);
% -----------------------------
% Input:
% TXTFile           String with name of .txt file
% linesToRemove     Lines that are going to be removed
% -----------------------------
function DeleteLinesFromTXTFile(TXTFile, linesToRemove)

    % Read the content of the input file
    fid = fopen(TXTFile, 'r');
    content = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);

    % Remove specified lines
    content{1}(linesToRemove) = [];

    % Write the modified content to the original file
    fid = fopen(TXTFile, 'w');
    fprintf(fid, '%s\n', content{1}{:});
    fclose(fid);
end