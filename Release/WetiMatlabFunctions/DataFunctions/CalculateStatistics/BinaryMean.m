function Availability = BinaryMean(Data, x, NumBits)
% Calculates mean availability of a selected binary status bit.
% Flexible: choose number of bits to consider (default 10).
% Function for CalculateStatistics.

% inputs
arguments
    Data        (:,1) double
    x           (1,1) double {mustBeInteger, mustBeInRange(x,0,9)}
    NumBits     (1,1) double {mustBeInteger, mustBePositive} = 10
end
% check x is within valid range
if x < 0 || x > NumBits-1
    error('BinaryMean:InvalidX', 'x must be between 0 and NumBits-1 (%d)', NumBits-1);
end

% check for data exceeding max representable value
MaxValue = 2^NumBits - 1;
if max(Data) > MaxValue
    warning('BinaryMean:DataExceedsMax', ...
        'Some data exceed %d. Only the lower %d bits are considered.\nThe input data may be corrupted!', MaxValue, NumBits);
end

% mask lower NumBits bits and convert to logical matrix
Status = dec2bin(bitand(uint16(Data), MaxValue), NumBits) == '1';

% map x to column (0 -> MSB, NumBits-1 -> LSB) 
col = NumBits - x;

% mean availability
Availability = mean(Status(:, col));

end

