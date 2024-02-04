%% CreatePermutationMatrix
% Function: creates the permutation matrix of a variation
% 
%% Usage:
%
%  [nVariation nPermutation Permutation] = CreatePermutationMatrix(Variation)
%
%% Input:
%
% * PreProcessingVariation  - cell with PreProcessing variation
%                           1. column: char with identifier, e.g. 'URef'
%                           2. column: numeric values for idenifier, e.g. [4:2:24]
%                           3. column: format, e.g. '%02d' or []
%% Output:
%
% * nVariation          - number of variations
% * nPermutation        - number of permutations
% * Permutation         - permutation matrix [nPermutation x nVariation]


%% Code:
function [nVariation, nPermutation, Permutation] = CreatePermutationMatrix(PreProcessingVariation)

[nVariation, ~]                 = size(PreProcessingVariation);
VariationDepth                  = NaN(nVariation,1);
for iVariation                  = 1:nVariation
    VariationDepth(iVariation)  = length(PreProcessingVariation{iVariation,2});
end
nPermutation                    = prod(VariationDepth);

Permutation     = [];
for iVariation  = 1:nVariation
    Permutation	= [reshapeRows(Permutation,VariationDepth(iVariation)) ....
        repmat([1:VariationDepth(iVariation) ]',prod(VariationDepth(1:iVariation-1)),1)];
end
    
end

function Mout = reshapeRows(M,a)
% Function: reshapes a matrix having each row a-times
% Usage:
% Mout = reshapeRows(M,a)
% Input:
% * M               - Matrix
% * a               - number of multiple rows
% Output:
% * Mout            - output Matrix

sizeM   = size(M);
Mout    = reshape(repmat(M,1,a)',sizeM(2),sizeM(1)*a)';

end