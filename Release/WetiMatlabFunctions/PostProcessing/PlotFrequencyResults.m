function PlotFrequencyResults(FrequencyResults,PostProcessingConfig)

%% BasicFrequencyPlot
% main difference to BasicTimePlot:
% - FrequencyResults instead of TimeResults
% - plot y over x instead of plot time series
% - xlabel needs to be defined for each figure, ylabel for each channel
if isfield(PostProcessingConfig.Plots,'BasicFrequencyPlot')
    nFigures        = length(PostProcessingConfig.Plots.BasicFrequencyPlot);    
    for iFigure=1:nFigures
        PlotConfig  = PostProcessingConfig.Plots.BasicFrequencyPlot{iFigure};
        if PlotConfig.Enable            
            figure('Name',['BasicFrequencyPlot ',num2str(iFigure)])
            nChannels               = size(PlotConfig.Channels,1);
            % determine IndicesConsideredDataFiles and nConsideredDataFiles
            if isfield(PlotConfig,'IndicesConsideredDataFiles') % plot only considered files
                IndicesConsideredDataFiles  = PlotConfig.IndicesConsideredDataFiles;
                nConsideredDataFiles        = length(IndicesConsideredDataFiles);
            else
                nConsideredDataFiles        = FrequencyResults.nDataFiles;
                IndicesConsideredDataFiles  = 1:nConsideredDataFiles;                
            end            
            % make one subplot for each channel            
            for iChannel = 1:nChannels
                ThisChannel         = PlotConfig.Channels{iChannel};
                x                   = PlotConfig.x;
                y                   = PlotConfig.yCell{iChannel};
                subplot(nChannels,1,iChannel)                    
                hold on; box on; grid on               
                % plot data
                for iConsideredDataFile=1:nConsideredDataFiles
                    iDataFile               = IndicesConsideredDataFiles(iConsideredDataFile);
                    if isfield(PlotConfig,'PlotCommand')
                        switch PlotConfig.PlotCommand
                            case 'bar'
                                bar(FrequencyResults.(ThisChannel)(iDataFile).(x),...
                                    FrequencyResults.(ThisChannel)(iDataFile).(y))                                                                                                                    
                            otherwise
                                error('PlotConfig.PlotCommand %s not defined.',PlotConfig.PlotCommand)
                        end
                    else
                        plot(FrequencyResults.(ThisChannel)(iDataFile).(x),...
                             FrequencyResults.(ThisChannel)(iDataFile).(y))                                                                                    
                    end
                end                
                % ylabel: should be defined for each Channel
                if isfield(PlotConfig,'ylabelCell')
                    ylabel(PlotConfig.ylabelCell{iChannel})
                end
                % plot title: fixed to channels
                title(ThisChannel,'Interpreter','none')
                % plot legend
                if isfield(PlotConfig,'legend')
                    legend(PlotConfig.legend,'Interpreter','none','location','best')
                end                
                % set gca properties
                if isfield(PlotConfig,'gca')
                    PropertyFields      = fieldnames(PlotConfig.gca);
                    for iProperty=1:size(PropertyFields,1)
                        Property    = getfield(PlotConfig.gca,PropertyFields{iProperty});
                        set(gca,PropertyFields{iProperty},Property);
                    end
                end                 
            end                
            % xlabel: same for each figure
            if isfield(PlotConfig,'xlabel')
                xlabel(PlotConfig.xlabel)
            end
        end
    end
end

%% ComparisonFrequencyPlot
% main difference to ComparisonTimePlot:
% - FrequencyResults instead of TimeResults
% - plot y over x instead of Data over Time
% - xlabel needs to be defined for each figure, ylabel for each channel
if isfield(PostProcessingConfig.Plots,'ComparisonFrequencyPlot')
    nFigures     = length(PostProcessingConfig.Plots.ComparisonFrequencyPlot);    
    for iFigure=1:nFigures
        PlotConfig      = PostProcessingConfig.Plots.ComparisonFrequencyPlot{iFigure};
        if PlotConfig.Enable
            figure('Name',['ComparisonFrequencyPlot ',num2str(iFigure)])
            nChannels        = size(PlotConfig.Channels,1);
            % determine IndicesConsideredDataFiles and nConsideredDataFiles
            if isfield(PlotConfig,'IndicesConsideredDataFiles') % plot only considered files
                IndicesConsideredDataFiles  = PlotConfig.IndicesConsideredDataFiles;
                nConsideredDataFiles        = length(IndicesConsideredDataFiles);
            else
                nConsideredDataFiles        = FrequencyResults.nDataFiles;
                IndicesConsideredDataFiles  = 1:nConsideredDataFiles;                
            end 
            % make one subplot for each considered data file       
            for iConsideredDataFile=1:nConsideredDataFiles
                subplot(nConsideredDataFiles,1,iConsideredDataFile)
                hold on; box on; grid on                
                ylabel(PlotConfig.ylabel)
                iDataFile               = IndicesConsideredDataFiles(iConsideredDataFile);
                x                       = PlotConfig.x;
                y                       = PlotConfig.y;
                for iChannel = 1:nChannels
                    ThisChannel  = PlotConfig.Channels{iChannel};
                    if isfield(PlotConfig,'PlotCommand')
                        switch PlotConfig.PlotCommand
                            case 'bar'
                                bar(FrequencyResults.(ThisChannel)(iDataFile).(x),...
                                    FrequencyResults.(ThisChannel)(iDataFile).(y))                                                                                                                    
                            otherwise
                                error('PlotConfig.PlotCommand %s not defined.',PlotConfig.PlotCommand)
                        end
                    else
                        plot(FrequencyResults.(ThisChannel)(iDataFile).(x),...
                             FrequencyResults.(ThisChannel)(iDataFile).(y))                                                                                    
                    end                      
                end
                % ylabel: need to be defined due to different channels 
                if isfield(PlotConfig,'ylabel')
                    ylabel(PlotConfig.ylabel)
                end                
                % plot title
                if isfield(PlotConfig,'title')
                    title(PlotConfig.title{iConsideredDataFile},'Interpreter','none')
                end
                % plot legend: fixed to Channels
                legend(PlotConfig.Channels,'Interpreter','none','location','best')              
                % set gca properties
                if isfield(PlotConfig,'gca')
                    PropertyFields      = fieldnames(PlotConfig.gca);
                    for iProperty=1:size(PropertyFields,1)
                        Property    = getfield(PlotConfig.gca,PropertyFields{iProperty});
                        set(gca,PropertyFields{iProperty},Property);
                    end
                end             
            end           
            % xlabel: same for each figure
            if isfield(PlotConfig,'xlabel')
                xlabel(PlotConfig.xlabel)
            end
        end
    end
end

end