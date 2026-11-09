% Function: ManipulateFastInputFile String replacement in FAST Input file
% -----------------------------
% Usage:
% n = ManipulateFastInputFile(TXTFile,Identifier,NewString)
% -----------------------------
% Input:
% TXTFile           String with name of .txt file
% Identifier        Identifier in FAST style input file (supports regex)
% NewString         String with replacement
% -----------------------------
% Output:
% n                 number of replacements
% ----------------------------------
function n = ManipulateFastInputFile(TXTFile, Identifier, NewString)

    % 1. Read the entire file into memory as a single text block
    fid = fopen(TXTFile, 'r');
    if fid == -1
        error('ManipulateFastInputFile:CannotOpenFile', 'Cannot open file "%s" for reading.', TXTFile);
    end
    rawText = fread(fid, '*char')';
    fclose(fid);

    % 2. Split raw text into a cell array of lines (supports both \r\n and \n)
    lines = strsplit(rawText, {'\r\n', '\n'}, 'CollapseDelimiters', false);
    
    % Remove trailing empty cell if the file ended with a newline character
    if ~isempty(lines) && isempty(lines{end})
        lines(end) = [];
    end

    % 3. Vectorized regex search across the entire cell array at once
    % This restores full regex support (e.g., 'BlPitch\((1|2|3)\)') while
    % remaining extremely fast by avoiding loops.
    pattern = ['\s', Identifier, '(\s|$)'];
    startIndices = regexp(lines, pattern, 'start', 'once');
    
    % Find all line indices that had a regex match
    candidateIdx = find(~cellfun(@isempty, startIndices));
    n = length(candidateIdx);

    % 4. Perform the string replacement directly on matching lines
    for i = 1:n
        idx = candidateIdx(i);
        currentLine = lines{idx};

        % Add 1 to step past the leading whitespace '\s' from the pattern
        startIdx = startIndices{idx} + 1;
        lines{idx} = [NewString, ' ', currentLine(startIdx:end)];
    end

    % 5. Overwrite the file only if changes occurred (vectorized, no loop)
    if n > 0
        fid = fopen(TXTFile, 'w');
        if fid == -1
            error('ManipulateFastInputFile:CannotWriteFile', 'Cannot open file "%s" for writing.', TXTFile);
        end
        % Expanding lines{:} passes all lines as an argument list; fprintf applies 
        % '%s\r\n' sequentially at C-speed (ensures OpenFAST-compatible line endings)
        fprintf(fid, '%s\r\n', lines{:});
        fclose(fid);
    end

end