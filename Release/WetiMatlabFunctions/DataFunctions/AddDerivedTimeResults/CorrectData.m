function TimeResults = CorrectData(TimeResults,ThisChannel,NewChannel,options)
% Corrects data with with array-allocation-bug and the first-values-bug.
% Usually, only turbine data before the correction in the DataAcquisitionApp 
% are affected by the array-allocation-bug. The first-values-bug still
% remains. This can be checked by comparing the last value from one file to
% the first value in the next file, e.g. 
% TimeResults{1}.activepow.Data(end) vs TimeResults{2}.activepow.Data(1)
% Function for AddDerivedTimeResults.

% inputs
arguments
    TimeResults                 cell
    ThisChannel                 char
    NewChannel                  char = ''
    options.DetectionMode       char {mustBeMember(options.DetectionMode,{'outlier','compare'})} = 'compare'
    options.tolerance           {mustBeNumeric} = 1e-3  % tolerance to find values, since outb files are scaled             
    options.window              {mustBeInteger} = 20    % number of data points to detect outliers
    options.DetectionChannel    char = 'windspeed2'       % best channel since always changing, should be corrected last! 
    options.MaxAllowedDuration  duration        = minutes(12) % worst case for data files which are still ok: e.g. HH:09 and HH:21
    options.LastStartTime       datetime        = datetime('09-Oct-2023 12:20:00') % bug fixed after this file
    options.IdxFirstValues      {mustBeInteger} = [1,2] % first two values are always strange
    options.Display             char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% get dimensions
nDataFiles          = size(TimeResults,1);

% init display text
MyText              = "";

%% new
switch options.DetectionMode
    case 'outlier'
        for iDataFile = 1:nDataFiles
            % get data
            Time            = TimeResults{iDataFile}.(ThisChannel).Time;
            Data            = TimeResults{iDataFile}.(ThisChannel).Data;
            % detect bad data 
            IsBadData       = isoutlier(Data,"movmedian",options.window);
            % flag first values
            IsBadData(options.IdxFirstValues)   = true;            
            % set bad data to NaN
            Data(IsBadData) = NaN;
            % interpolate data
            Data            = fillmissing(Data,'linear','EndValues','nearest');  
            % add new channel or overwrite old one
            if ~isempty(NewChannel)
                NewTimeSeries           = timeseries(Data,Time,'Name',NewChannel);
                NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(ThisChannel).DataInfo.Units; % inherit units 
                TimeResults{iDataFile}  = TimeResults{iDataFile}.addts(NewTimeSeries);
            else
                TimeResults{iDataFile}.(ThisChannel).Data = Data;
            end 
        end
    case 'compare'
        for iDataFile = 1:nDataFiles
            % display if requested
            if strcmp(options.Display,'on')                
                fprintf(repmat('\b',1,strlength(MyText))); % remove previous text
                MyText      = "Correcting " + ThisChannel + ": " + iDataFile + "/" + nDataFiles;
                fprintf(MyText);
                if iDataFile==nDataFiles % new line
                    fprintf('\n');
                end
            end            
            % get start time
            NewStartTime        = datetime(TimeResults{iDataFile}.Name(end-17:end-5),'InputFormat','yyyyMMdd''T''HHmm');            
            % get data
            OriginalData        = TimeResults{iDataFile}.(ThisChannel).Data;            
            % only apply for files before array-allocation-bug was fixed
            if NewStartTime<=options.LastStartTime
                % detect bad data 
                if iDataFile==1||NewStartTime-OldStartTime>options.MaxAllowedDuration % first file, also after restart (cannot compared to previous, only outlier method possible)
                    IsBadData           = isoutlier(OriginalData,"movmedian",options.window);
                    % get data from DetectionChannel
                    OldDetectionTime    = TimeResults{iDataFile}.(options.DetectionChannel).Time;
                    OldDetectionData    = TimeResults{iDataFile}.(options.DetectionChannel).Data;                    
                else % check if NewDetectionData has same value as OldDetectionData at the same time
                    % get data from DetectionChannel
                    NewDetectionTime    = TimeResults{iDataFile}.(options.DetectionChannel).Time;
                    NewDetectionData    = TimeResults{iDataFile}.(options.DetectionChannel).Data;
                    if size(NewDetectionData,1)==size(OldDetectionData,1) % equal size
                        IsBadData       = abs((OldDetectionData-NewDetectionData)./OldDetectionData)<=options.tolerance;
                    else % different size (e.g. if last or first data points are missing)
                        IdxNew          = ismember(NewDetectionTime,OldDetectionTime);
                        IdxOld          = ismember(OldDetectionTime,NewDetectionTime);
                        IsBadData       = abs((OldDetectionData(IdxOld)-NewDetectionData(IdxNew))./OldDetectionData(IdxOld))<=options.tolerance;
                    end 
                    % update OldDetectionTime and OldDetectionData
                    OldDetectionData            = NewDetectionData;
                    OldDetectionTime            = NewDetectionTime;                    
                end
            else
                IsBadData                       = false(size(OriginalData));
            end
            % flag first values
            IsBadData(options.IdxFirstValues)   = true;
            % set bad data to NaN
            OriginalData(IsBadData)             = NaN;
            % interpolate data
            CorrectedData                       = fillmissing(OriginalData,'linear','EndValues','nearest');            
            % update OldStartTime
            OldStartTime                        = NewStartTime;
            % add new channel or overwrite old one
            if ~isempty(NewChannel)                
                Time                            = TimeResults{iDataFile}.(ThisChannel).Time;                 
                NewTimeSeries                   = timeseries(CorrectedData,Time,'Name',NewChannel);
                NewTimeSeries.DataInfo.Units    = TimeResults{iDataFile}.(ThisChannel).DataInfo.Units; % inherit units   
                TimeResults{iDataFile}          = TimeResults{iDataFile}.addts(NewTimeSeries);
            else
                TimeResults{iDataFile}.(ThisChannel).Data = CorrectedData;
            end 
        end
end