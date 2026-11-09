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
function AddLineToTXTFile(TXTFile, nLine, NewLine)

    % 1. Read the entire file into memory as a single text block
    fid = fopen(TXTFile, 'r');
    if fid == -1
        error('AddLineToTXTFile:CannotOpenFile', 'Cannot open file "%s" for reading.', TXTFile);
    end
    rawText = fread(fid, '*char')';
    fclose(fid);

    % 2. Split raw text into a cell array of lines (supports both \r\n and \n)
    lines = strsplit(rawText, {'\r\n', '\n'}, 'CollapseDelimiters', false);
    
    % Remove trailing empty cell if the file ended with a newline character
    if ~isempty(lines) && isempty(lines{end})
        lines(end) = [];
    end

    % 3. Insert the new line directly into the cell array without loops
    if nLine >= length(lines)
        lines = [lines, {NewLine}];
    elseif nLine <= 0
        lines = [{NewLine}, lines];
    else
        lines = [lines(1:nLine), {NewLine}, lines(nLine+1:end)];
    end

    % 4. Overwrite the file in one single vectorized operation
    fid = fopen(TXTFile, 'w');
    if fid == -1
        error('AddLineToTXTFile:CannotWriteFile', 'Cannot open file "%s" for writing.', TXTFile);
    end
    % Expanding lines{:} passes all lines as an argument list; fprintf applies 
    % '%s\r\n' sequentially at C-speed (ensures OpenFAST-compatible line endings)
    fprintf(fid, '%s\r\n', lines{:});
    fclose(fid);

end