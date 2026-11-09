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
% ----------------------------------
function n = ManipulateTXTFile(TXTFile, StringToReplace, NewString)

    % 1. Read the entire file into memory as a single text block
    fid = fopen(TXTFile, 'r');
    if fid == -1
        error('ManipulateTXTFile:CannotOpenFile', 'Cannot open file "%s" for reading.', TXTFile);
    end
    rawText = fread(fid, '*char')';
    fclose(fid);

    % 2. Split raw text into a cell array of lines (supports both \r\n and \n)
    lines = strsplit(rawText, {'\r\n', '\n'}, 'CollapseDelimiters', false);
    
    % Remove trailing empty cell if the file ended with a newline character
    if ~isempty(lines) && isempty(lines{end})
        lines(end) = [];
    end

    % 3. Vectorized string replacement across all lines at once
    % strrep natively supports cell arrays of character vectors at C-speed
    newLines = strrep(lines, StringToReplace, NewString);

    % 4. Count the number of modified lines (matches original logic)
    modifiedIdx = ~strcmp(lines, newLines);
    n = sum(modifiedIdx);

    % 5. Overwrite the file only if changes occurred (vectorized, no loop)
    if n > 0
        fid = fopen(TXTFile, 'w');
        if fid == -1
            error('ManipulateTXTFile:CannotWriteFile', 'Cannot open file "%s" for writing.', TXTFile);
        end
        % Expanding newLines{:} passes all lines as an argument list; fprintf applies 
        % '%s\r\n' sequentially at C-speed (ensures OpenFAST-compatible line endings)
        fprintf(fid, '%s\r\n', newLines{:});
        fclose(fid);
    end

end