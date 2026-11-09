%TABLINE Create a line of tabulated text and numbers
%
%   Syntax:  String = tabline(Elements2disp,len)
%
%   Inputs:  Elements2disp   (cell)    Elements to display (character strings or
%                                         numbers)
%            len             (vec)     Desired length in output string
%
%            String          (char)    Output string
%
%   Example: disp(tabline({'Country','City','Population'},[10 10 15]))
%            disp(tabline({'Germany','Frankfurt',600000},[10 10 15]))
%            disp(tabline({'France','Lyon',200000},[10 10 15]))

function String = tabline(Elements2disp,len)

if nargin < 2, error('Too few input arguments'); end

if ~iscell(Elements2disp), error('Elements2disp must be cell array'); end
if length(Elements2disp) ~= length(len)
    error('Elements2disp and len must be identical size')
end

String = '';
for j=1:length(Elements2disp)
    CurrentElement = Elements2disp{j};
    if isnumeric(CurrentElement)
        CurrentString = num2str(CurrentElement);
    elseif ischar(CurrentElement)
        CurrentString = CurrentElement;
    else
        CurrentString = '';
    end
    if length(CurrentString) <= len(j)-1
        String = [String,CurrentString,blanks(len(j)-length(CurrentString))];
    else
        String = [String,CurrentString(1:len(j)-1),blanks(1)];
    end
end
    
