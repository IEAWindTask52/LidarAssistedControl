function [time,data,channelNames,Units] = ReadBladedOutput(headerFile)

arguments
    headerFile char
end
    
[pathstr, name, ext]    = fileparts(headerFile);
fileNumber              = regexprep(ext, "[^\d]", "");
dataInfo                = readHeaderInfo(headerFile);
dataFile                = fullfile(pathstr, [name, '.$' , fileNumber]);
data                    = readDataFile(dataFile, dataInfo.dataSizes, dataInfo.isBinary, dataInfo.dataFormat);
dataInfo.dataSizes(2)   = size(data, 2); % Correct size info if, e.g., there was for some reason less data stored in the file.
time                    = dataInfo.startTime + (0 : dataInfo.dataSizes(end)-1) * dataInfo.timeStep;
channelNames            = dataInfo.channelNames;
Units                   = dataInfo.Units;

end

%% slighly modified version from mallard (no path etc., not fully tested)
function data = readDataFile(file, dataSize, isBinary, dataFormat)

if dataSize(2) == 0
    data = zeros(0, dataSize(2));
    return
end

try
    if isBinary
        if length(dataSize) == 3
            dataSize2d = [dataSize(1) * dataSize(2), dataSize(3)];
        else
            dataSize2d = dataSize;
        end
        fileId = fopen(file);
        data = fread(fileId, dataSize2d, dataFormat);
        fclose(fileId);
    else
        data = readmatrix(file.string, "FileType", "text", "ConsecutiveDelimitersRule", "join", "LeadingDelimitersRule", "ignore");
        if length(dataSize) == 3
            data = reshape(data, dataSize(2), dataSize(1) * dataSize(3))';
            data = reshape(data, dataSize(3), dataSize(1) * dataSize(2));
        end
        data = data';
        if file.is(Extension=".$29")
            data = data(:, 1:dataSize(end)); % Only here because of the stange logging for external controllers.
        end
    end
catch cause
    MException("bladed:readOutput:ReadError", "Unable to read output file ""%s"".", file.string).addCause(cause).throw;
end

end

function result = readHeaderInfo(file)

headerText = readTextFile(file);

dataSize = extractNumericParameterValue(headerText, "DIMENS");
isBinary = extractParameterValue(headerText, "ACCESS") == "D";
dataGroup = removeQuotes(extractParameterValue(headerText, "GENLAB"));
startTime = extractNumericParameterValue(headerText, "MIN");
timeStep = extractNumericParameterValue(headerText, "STEP");
channelNames = split(removeQuotes(extractParameterValue(headerText, "VARIAB")), "' '");

% DS add units
UnitsBladed = split(extractParameterValue(headerText, "VARUNIT"));
Units       = replace(UnitsBladed,{'TT','L','T','A'},{'s^2','m','s','rad'}); % maybe more???

dataFormat = "real*4";
[~,~,ext] = fileparts(file);
if ext==".%29"
    
    % Workaround for buggy external controller outputs. (Bladed 4.8 only)
    if dataSize(2) == 0
        [~, summaryDataSizes, ~, ~, summaryStartTime, summaryTimeStep] = readHeaderInfo(file.setExtension(".%04"));
        simulationTime = summaryDataSizes(2) * summaryTimeStep;
        dataSize(2) = floor(simulationTime / timeStep);
    end
    
    % Workaround for strange logging behaviour seen in v4.9:
    % - The first log value is for step #1. There is not log value for step #0
    % - Ignore the last value, since its either a duplicate, if the
    % controller step size is a multiple of the simulation time, or a value
    % for which the time equals the controller end time, which is not
    % acually a controller step.
    startTime = timeStep;
    dataSize(end) = dataSize(end) - 1;   
    
    dataFormat = "real*8";   
elseif ext==".%296" || ext==".%297"
    % Workaround for buggy external controller debug log seen in Bladed 4.9
    if dataSize(2) == 0
        if ~isBinary
            dataFileLines = readTextFile(file.setExtension(file.extension.replace("%", "$")), Split=true);
            dataSize(2) = length(dataFileLines) - 1;
        else
            error("Not implemented")
        end
    end
end

if length(dataSize) == 3
    axisMethod = extractNumericParameterValue(headerText, "AXIMETH");
    if axisMethod == 1
        axisTick = split(removeQuotes(extractParameterValue(headerText, "AXITICK")), "' '");
    else
        axisTick = double(split(extractParameterValue(headerText, "AXIVAL")));
    end
    [X, Y] = meshgrid(axisTick, channelNames);
    channelNames = X(:) + " | " + Y(:);
end
channelNames = dataGroup + " | " + channelNames;

result = struct('channelNames', channelNames, 'dataSizes', dataSize, 'isBinary', isBinary, ...
    'dataFormat', dataFormat, 'startTime', startTime, 'timeStep', timeStep, 'Units', Units);
end

%% additional functions from mallard (readTextFile without path, not fully tested)
function content = readTextFile(file, options)

arguments
    file  char
    options.Start (1, 1) double {mustBeInteger, mustBePositive} = 1
    options.Lines (1, :) double {mustBeInteger, mustBePositive} = []
    options.SplitLines (1, 1) logical = false
end

if options.Start ~= 1 && ~isempty(options.Lines)
    error("Using both name-value arguments 'Start' and 'Lines' is not allowed.'"); end

if isempty(options.Lines) && options.Start == 1
    % todo: use fileread when start index ~= 1
    content = convertCharsToStrings(fileread(file));    
else
    fileId = fopen(file);
    content = "";
    currentIndex = 0;
    while ~feof(fileId)
        currentIndex = currentIndex + 1;
        line = fgets(fileId);
        if currentIndex < options.Start
            continue; end
        if ~isempty(options.Lines) 
            if currentIndex > options.Lines(end)
                break; end
            if ~any(currentIndex == options.Lines)
                continue; end
        end
        content = content + line;
    end
end

% Remove carriage returns.
content = regexprep(content, "\r", "");

% Split lines.
if options.SplitLines
    content = content.split(newline);
end
end

function value = extractNumericParameterValue(s, name)
arguments
    s (1, 1) string
    name (1, 1) string
end

match = extractParameterValue(s, name);
value = str2num(match);
if isempty(value)
    error("extractNumericParameterValue:NonNumericValue", "Unable to convert value string '%s' of parameter '%s' to a numeric value.", match, name);
end

end

function value = extractParameterValue(s, name, options)

arguments
    s (1, 1) string
    name (1, 1) string
    options.ValueFirst (1, 1) logical = false
end

if ~options.ValueFirst
    regexpPatters = "^\s*" + regexptranslate("escape", name) + "\s*(\s|=)\s*(?<value>[^=].*|)$";
else
    regexpPatters = "^\s*(?<value>[^\n]*|)\s*" + regexptranslate("escape", name) + "(\s|$)";
end

resultStruct = regexp(s, regexpPatters, "names", "once", "emptymatch", "lineanchors", "dotexceptnewline");

if isempty(resultStruct)
    error("extractParameterValue:ParamterNotFound", "Unable to find parameter ""%s"".", name); end
    
value = resultStruct.value.strip;

end

function s = removeQuotes(s)

% Removes leading and trailing apostrophes and quotation
% marks embracing a string. Does not remove unpaired marks.

arguments
    s string
end

s = arrayfun(@removeQuotes_scalar, s);

end

function s = removeQuotes_scalar(s)

foundSingle = startsWith(s, "'") && endsWith(s, "'");
foundDouble = startsWith(s, """") && endsWith(s, """");
if foundSingle || foundDouble
    s = extractAfter(s, 1);
    s = extractBefore(s, strlength(s));
end

end
