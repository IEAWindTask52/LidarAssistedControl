function FrequencyResults = CalculateFrequencyResults(TimeResults,PostProcessingConfig,options)
% TODO: Parallel Processing. "parfor iChannel" and "parfor iDataFile" are
% much slower. 

% inputs
arguments
    TimeResults             cell
    PostProcessingConfig    struct 
    options.Display         char {mustBeMember(options.Display,{'off','on'})} = 'off'
end

% return, if not defined or empty
if ~isfield(PostProcessingConfig,'CalculateFrequencyResults')||isempty(PostProcessingConfig.CalculateFrequencyResults)
    FrequencyResults = struct;
    return
end

% get dimensions
nCalculation    = size(PostProcessingConfig.CalculateFrequencyResults,1);
CalculationIDs  = PostProcessingConfig.CalculateFrequencyResults(:,1);
Functions  	    = PostProcessingConfig.CalculateFrequencyResults(:,2);
ChannelCells    = PostProcessingConfig.CalculateFrequencyResults(:,3);
nDataFiles      = size(TimeResults,1);
FrequencyResults= struct;

% loop over calculations
for iCalculation = 1:nCalculation

    % extract
    ThisCalculationID           = CalculationIDs{iCalculation};
    ThisFunction                = Functions{iCalculation};
    ThisChannelCell             = ChannelCells{iCalculation};
    nChannel                    = size(ThisChannelCell,1);

    % loop over Channels
    for iChannel = 1:nChannel       
        mChannel    = size(ThisChannelCell,2);
        if  mChannel==1 % function only applied to one channel
            % define Variable name etc.
            ThisChannel             = ThisChannelCell{iChannel};
            Variable                = [ThisCalculationID,'_',ThisChannel]; 
            % display if requested
            if strcmp(options.Display,'on')                
                MyText      = "Processing " + Variable + "\n";
                fprintf(MyText);
            end 
            % loop over data
            for iDataFile = 1:nDataFiles
                ThisTimeSeries      = TimeResults{iDataFile}.(ThisChannel);
                Data                = ThisTimeSeries.Data;
                Time                = ThisTimeSeries.Time;
                FrequencyResults.(Variable)(iDataFile) = ThisFunction(Data,Time);
            end
        else     
            % define Variable name etc.
            TempCell            = strcat('_',ThisChannelCell(iChannel,:));
            Variable            = [ThisCalculationID,strcat(TempCell{:})];
            % display if requested
            if strcmp(options.Display,'on')                
                MyText      = "Processing " + Variable + "\n";
                fprintf(MyText);
            end 
            % loop over data            
            for iDataFile = 1:nDataFiles
                Data                = cell(mChannel,1);
                Time                = cell(mChannel,1); 
                for jChannel = 1:mChannel
                    ThisChannel     = ThisChannelCell{iChannel,jChannel};
                    ThisTimeSeries  = TimeResults{iDataFile}.(ThisChannel);
                    Data{jChannel}  = ThisTimeSeries.Data;
                    Time{jChannel}  = ThisTimeSeries.Time;
                end
                FrequencyResults.(Variable)(iDataFile) = ThisFunction(Data,Time);
            end
        end
    end
end

% additional information
FrequencyResults.nDataFiles = nDataFiles;

end