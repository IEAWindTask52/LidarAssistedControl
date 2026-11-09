% Function: ManipulateYamlFile replacement in yaml file
% -----------------------------
% Usage:
% ManipulateYamlFile(TXTFile,Identifier,NewValue)
% -----------------------------
% Input:
% TXTFile           String with name of .txt file
% Identifier        Identifier in FAST style input file 
% NewValue          Value for replacement
% -----------------------------
% Output:
% -
% -----------------------------
% Created: 
% David Schlipf on 23-Nov-2024
% (c) WETI
% ----------------------------------
function ManipulateYamlFile(TXTFile,Identifier,NewValue)

% load data from yaml file
Data = yaml.loadFile(TXTFile);

% get field names from Identifier
FieldNames = split(Identifier,'.');
nFields    = size(FieldNames,1);

% replace value
switch nFields
    case 1
        Data.(FieldNames{1}) = NewValue;
    case 2 
        Data.(FieldNames{1}).(FieldNames{2}) = NewValue;
    case 3
        Data.(FieldNames{1}).(FieldNames{2}).(FieldNames{3}) = NewValue;
    case 4
        Data.(FieldNames{1}).(FieldNames{2}).(FieldNames{3}).(FieldNames{4}) = NewValue;    
    otherwise
        error(['Field ',Identifier,' not found.'])
end

% write data to yaml file
yaml.dumpFile(TXTFile, Data);

end