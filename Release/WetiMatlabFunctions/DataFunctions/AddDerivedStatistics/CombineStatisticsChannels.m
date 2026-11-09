function Statistics = CombineStatisticsChannels(Statistics,InputChannels,OutputChannels,options)
% Combines channels from Statistics into Statistics.
% Function for AddDerivedStatistics.

% inputs
arguments
    Statistics                  table
    InputChannels               cell
    OutputChannels              char
    options.method              {mustBeMember(options.method,{'mean','sum','substract'})} = 'mean'
    options.override    (1,1)   logical = 1;         
end

% Determine size
[nRowsInput, nColsInput] = size(InputChannels); % InputChannels is a cell array
[nOutput,~] = size(OutputChannels);

if nColsInput ~= nOutput
    error('Number of output channels must match the number of columns in InputChannels.');
end

% Loop over output channels (columns)
for iOutput = 1:nOutput
    % Get the list of input channels for this output (all rows in this column)
    inputList = InputChannels(:, iOutput);

    % Prepare dummy matrix to sum
    nStatistics = size(Statistics, 1);
    DummyMatrix = NaN(nStatistics, nRowsInput);

    % Fill dummy matrix with the columns
    for iRow = 1:nRowsInput
        DummyMatrix(:, iRow) = Statistics.(inputList{iRow}{1});
        % Remove original channels if override is true
        if options.override
            Statistics.(inputList{iRow}{1}) = [];
        end
    end
    switch options.method
        case 'sum'
            % Sum across rows (2nd dimension) → one column per observation
            result = sum(DummyMatrix, 2);
        case 'substract'
            % Sequential substraction along rows 
            weights = [1, -ones(1, size(DummyMatrix,2)-1)];
            result = DummyMatrix * weights.';
        case 'mean'
            % mean between the number of channels
            result = mean(DummyMatrix, 2);
        otherwise
            error('Selected method is not implemented!')
    end
    
    Statistics.(OutputChannels(iOutput,:)) = result;

end




end