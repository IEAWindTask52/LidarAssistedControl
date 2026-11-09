function TimeResults = InterpolateError(TimeResults,ThisChannel,NewChannel,options)
% Interpolates signals with a given Error Code.
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults             cell
    ThisChannel             char
    NewChannel              char = ''
    options.ErrorCodeIn     {mustBeNumeric} = 0    
    options.ErrorCodeOut    {mustBeNumeric} = -999
    options.tolerance       {mustBeNumeric} = 1e-3  % tolerance to find values, since outb files are scaled                 
    options.method          {mustBeMember(options.method,{'previous','next','nearest','linear','spline','pchip','makima','meanOfNeighbors'})}  = 'linear'
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'
    options.AllowAllError   {mustBeNumericOrLogical} = false
end

% get dimensions
nDataFiles       = size(TimeResults,1);

% init display text
MyText              = "";

% loop over all files
for iDataFile = 1:nDataFiles
    % display if requested
    if strcmp(options.Display,'on')                
        fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
        MyText      = "Interpolating error for " + ThisChannel + ": " + iDataFile + "/" + nDataFiles;
        fprintf(MyText);
        if iDataFile==nDataFiles % new line
            fprintf('\n');
        end
    end 
    % get data
    Data        = TimeResults{iDataFile}.(ThisChannel).Data;
    % set data with error to NaN
    if ~isnan(options.ErrorCodeIn)
        IsBadData   = abs(Data-options.ErrorCodeIn)<=options.tolerance;
        if mean(IsBadData)==1 && options.AllowAllError % all data is bad
            Data(IsBadData) = options.ErrorCodeIn;
        else
            Data(IsBadData) = NaN;
        end
    end
    % interpolate data
    if strcmp(options.method,'meanOfNeighbors')
        Data = fillmeanneighbors(Data,options.method,'EndValues','nearest');
    else
        Data = fillmissing(Data,options.method,'EndValues','nearest');
    end
    % add error code if still NaN (all data is bad and or something went wrong with fillmissing)
    StillNaN        = isnan(Data);    
    Data(StillNaN)  = options.ErrorCodeOut;
    % add new channel or overwrite old one
    if ~isempty(NewChannel)
        Time  = TimeResults{iDataFile}.(ThisChannel).Time;
        NewTimeSeries           = timeseries(Data,Time,'Name',NewChannel);
        NewTimeSeries.DataInfo.Units        = TimeResults{iDataFile}.(ThisChannel).DataInfo.Units; % inherit units 
        NewTimeSeries.TimeInfo.StartDate    = TimeResults{iDataFile}.(ThisChannel).TimeInfo.StartDate; 
        TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);
    else
        TimeResults{iDataFile}.(ThisChannel).Data = Data;
    end   
end

end
%% Helper function for mean of neighbors 
function DataOut = fillmeanneighbors(DataIn, method, varargin)
% FILLMEANNEIGHBORS Fill missing values using mean of last and next available points
% This function has been creeated by ChatGPT
% Usage:
%   DataOut = fillmeanneighbors(DataIn,'meanOfNeighbors','EndValues','nearest')
%
% Inputs:
%   DataIn : numeric vector or matrix
%   method : 'meanOfNeighbors' (for compatibility)
%   Name-Value Pairs:
%       'EndValues' : 'nearest' (default), 'none'
%
% Output:
%   DataOut : same size as DataIn with NaNs replaced

% Parse input arguments
p = inputParser;
addRequired(p,'DataIn',@isnumeric)
addRequired(p,'method',@(x) ischar(x) && strcmp(x,'meanOfNeighbors'))
addParameter(p,'EndValues','nearest',@(x) any(validatestring(x,{'nearest','none'})))
parse(p,DataIn,method,varargin{:})
endValues = p.Results.EndValues;

DataOut = DataIn;

% Process each column independently
for col = 1:size(DataOut,2)
    vec = DataOut(:,col);
    nanIdx = isnan(vec);
    
    % Skip if no NaNs
    if all(~nanIdx)
        continue
    end
    % Skip if entire column is NaN
    if all(nanIdx)
        continue
    end
    
    % Find runs of consecutive NaNs
    d = diff([0; nanIdx; 0]);
    startIdx = find(d==1);
    endIdx   = find(d==-1)-1;
    
    for k = 1:length(startIdx)
        s = startIdx(k);
        e = endIdx(k);
        
        % previous value
        if s==1
            if strcmp(endValues,'nearest')
                prevVal = vec(e+1); % fill with next value
            else
                prevVal = NaN;
            end
        else
            prevVal = vec(s-1);
        end
        
        % next value
        if e==length(vec)
            if strcmp(endValues,'nearest')
                nextVal = vec(s-1); % fill with previous value
            else
                nextVal = NaN;
            end
        else
            nextVal = vec(e+1);
        end
        
        % fill gap
        if isnan(prevVal) && isnan(nextVal)
            fillVal = NaN; % can't fill
        elseif isnan(prevVal)
            fillVal = nextVal;
        elseif isnan(nextVal)
            fillVal = prevVal;
        else
            fillVal = (prevVal + nextVal)/2;
        end
        
        vec(s:e) = fillVal;
    end
    
    DataOut(:,col) = vec;
end

end