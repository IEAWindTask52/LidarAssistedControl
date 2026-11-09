function signed_val = RecastSignedInteger(unsigned_val, n_bits)
%RECASTSIGNEDINTEGER Convert an unsigned integer back to is signed value
%   Recast an unsigned integer back to its original signed value,
%   considering that Simulink uses the Two's complement for representing
%   signed integers.

arguments
unsigned_val (1,:) double
n_bits (1, 1) {mustBeMember(n_bits,[8, 16, 32])}
end
	
    threshold = 2^(n_bits-1);
    
    % Find negatives:
    negative = unsigned_val >= threshold;

    signed_val = unsigned_val;
    % Assign negatives:
    signed_val(negative) = signed_val(negative) - 2^n_bits;

end

