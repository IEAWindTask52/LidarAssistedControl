function PlotTimeResults(TimeResults,PostProcessingConfig)

%% BasicTimePlot
if isfield(PostProcessingConfig,'Plots') && isfield(PostProcessingConfig.Plots,'BasicTimePlot')
    nFigures        = length(PostProcessingConfig.Plots.BasicTimePlot);    
    for iFigure=1:nFigures
        PlotConfig  = PostProcessingConfig.Plots.BasicTimePlot{iFigure};
        if PlotConfig.Enable
            figure('Name',['BasicTimePlot ',num2str(iFigure)])
            nChannels               = size(PlotConfig.Channels,1);
            % determine IndicesConsideredDataFiles and nConsideredDataFiles
            if isfield(PlotConfig,'IndicesConsideredDataFiles') % plot only considered files
                IndicesConsideredDataFiles  = PlotConfig.IndicesConsideredDataFiles;
                nConsideredDataFiles        = length(IndicesConsideredDataFiles);
            else
                nConsideredDataFiles        = size(TimeResults,1);
                IndicesConsideredDataFiles  = 1:nConsideredDataFiles;                
            end
            % make one subplot for each channel
            for iChannel = 1:nChannels
                ThisChannel     = PlotConfig.Channels{iChannel};
                subplot(nChannels,1,iChannel)                    
                hold on; box on; grid on  
                % yScale
                if isfield(PlotConfig,'yScaleCell')
                    yScale      = PlotConfig.yScaleCell{iChannel};
                elseif isfield(PlotConfig,'yScale')
                    yScale      = PlotConfig.yScale;
                else
                    yScale      = 1;
                end                  
                % plot data
                for iConsideredDataFile=1:nConsideredDataFiles
                    iDataFile               = IndicesConsideredDataFiles(iConsideredDataFile);
                    plot(TimeResults{iDataFile}.(ThisChannel).Time,yScale*TimeResults{iDataFile}.(ThisChannel).Data)                                                                                
                end  
                % ylabel: can be defined for each subplot or globally,
                % default unit
                if isfield(PlotConfig,'ylabelCell')
                    ylabel(PlotConfig.ylabelCell{iChannel})
                elseif isfield(PlotConfig,'ylabel')
                    ylabel(PlotConfig.ylabel)
                else
                    ylabel(['[',TimeResults{iDataFile}.(ThisChannel).DataInfo.Units,']'])
                end 
                % title: can be defined for each subplot or globally
                % default Channel
                if isfield(PlotConfig,'titleCell')
                    title(PlotConfig.titleCell{iChannel},'Interpreter','none')
                elseif isfield(PlotConfig,'title')
                    title(PlotConfig.title,'Interpreter','none')
                else
                    title(ThisChannel,'Interpreter','none')
                end                   
                % plot legend
                if isfield(PlotConfig,'legend')
                    legend(PlotConfig.legend,'Interpreter','none')
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
            % xlabel: fixed to time
            xlabel('time [s]')
            % link axes
            linkaxes(findobj(gcf, 'Type', 'Axes'), 'x')
        end
    end
end

%% ComparisonTimePlot
if isfield(PostProcessingConfig,'Plots') && isfield(PostProcessingConfig.Plots,'ComparisonTimePlot')
    nFigures     = length(PostProcessingConfig.Plots.ComparisonTimePlot);    
    for iFigure=1:nFigures
        PlotConfig      = PostProcessingConfig.Plots.ComparisonTimePlot{iFigure};
        if PlotConfig.Enable
            figure('Name',['ComparisonTimePlot ',num2str(iFigure)])
            nChannels        = size(PlotConfig.Channels,1);
            % determine IndicesConsideredDataFiles and nConsideredDataFiles
            if isfield(PlotConfig,'IndicesConsideredDataFiles') % plot only considered files
                IndicesConsideredDataFiles  = PlotConfig.IndicesConsideredDataFiles;
                nConsideredDataFiles        = length(IndicesConsideredDataFiles);
            else
                nConsideredDataFiles        = size(TimeResults,1);
                IndicesConsideredDataFiles  = 1:nConsideredDataFiles;                
            end
            % make one subplot for each considered data file       
            for iConsideredDataFile=1:nConsideredDataFiles                    
                subplot(nConsideredDataFiles,1,iConsideredDataFile)
                hold on; box on; grid on                
                iDataFile   = IndicesConsideredDataFiles(iConsideredDataFile);
                % plot data
                for iChannel = 1:nChannels
                    % yScale
                    if isfield(PlotConfig,'yScaleCell')
                        yScale      = PlotConfig.yScaleCell{iChannel};
                    elseif isfield(PlotConfig,'yScale')
                        yScale      = PlotConfig.yScale;
                    else
                        yScale      = 1;
                    end                    
                    ThisChannel  = PlotConfig.Channels{iChannel};
                    plot(TimeResults{iDataFile}.(ThisChannel).Time,yScale*TimeResults{iDataFile}.(ThisChannel).Data)   
                end
                % ylabel: need to be defined due to different channels 
                if isfield(PlotConfig,'ylabel')
                    ylabel(PlotConfig.ylabel)
                end
                % plot title
                if isfield(PlotConfig,'titleCell')
                    title(PlotConfig.titleCell{iConsideredDataFile},'Interpreter','none')
                elseif isfield(PlotConfig,'title')
                    title(PlotConfig.title,'Interpreter','none')
                end                 
                % plot legend: fixed to Channels
                legend(PlotConfig.Channels,'Interpreter','none')              
                % set gca properties
                if isfield(PlotConfig,'gca')
                    PropertyFields      = fieldnames(PlotConfig.gca);
                    for iProperty=1:size(PropertyFields,1)
                        Property    = getfield(PlotConfig.gca,PropertyFields{iProperty});
                        set(gca,PropertyFields{iProperty},Property);
                    end
                end             
            end 
            % xlabel: fixed to time
            xlabel('time [s]')
            % link axes
            linkaxes(findobj(gcf, 'Type', 'Axes'), 'x')
        end
    end
end

%% Other more advanced time plots
end