function S = importROdbg_withUnits(filename)

    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    cleaner = onCleanup(@() fclose(fid));

    fgetl(fid);              % generated line
    headerLine = fgetl(fid); % names
    unitsLine  = fgetl(fid); % units

    rawNames = regexp(strtrim(headerLine), '\s{2,}', 'split');
    rawUnits = regexp(strtrim(unitsLine),  '\s{2,}', 'split');

    fieldNames = matlab.lang.makeValidName(strtrim(rawNames), ...
        'ReplacementStyle', 'delete');

    nCols = numel(fieldNames);
    fmt = repmat('%f', 1, nCols);

    data = textscan(fid, fmt, ...
        'Delimiter', '', ...
        'MultipleDelimsAsOne', true, ...
        'CollectOutput', true);

    M = data{1};

    S = struct();
    for k = 1:nCols
        S.(fieldNames{k}) = M(:,k);
    end

    % Store units in a sub-struct
    S.units = struct();
    for k = 1:min(nCols, numel(rawUnits))
        S.units.(fieldNames{k}) = strtrim(rawUnits{k});
    end
end