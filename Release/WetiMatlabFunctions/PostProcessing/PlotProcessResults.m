function PlotProcessResults(ProcessResults,PostProcessingConfig)

%% BasicPlot
if isfield(PostProcessingConfig,'Plots') && isfield(PostProcessingConfig.Plots,'BasicProcessPlot')
    nFigures        = length(PostProcessingConfig.Plots.BasicProcessPlot);    
    for iFigure=1:nFigures
        PlotConfig  = PostProcessingConfig.Plots.BasicProcessPlot{iFigure};
        if PlotConfig.Enable            
            figure('Name',['BasicProcessPlot ',num2str(iFigure)])
            [nSubplots,nChannels] = size(PlotConfig.Channels);         
            % loop over nSubplots, lines in PlotConfig.Channels             
            for iSubplot = 1:nSubplots
                subplot(nSubplots,1,iSubplot)                    
                hold on; box on; grid on 
                % loop over Channels, columns in PlotConfig.Channels 
                for iChannel = 1:nChannels
                    ThisChannel         = PlotConfig.Channels{iSubplot,iChannel};
                    % x and y: can be defined for each subplot or globally
                    if isfield(PlotConfig,'xCell')
                        x               = PlotConfig.xCell{iSubplot,iChannel};
                    elseif isfield(PlotConfig,'x')
                        x               = PlotConfig.x;
                    end
                    if isfield(PlotConfig,'yCell')
                        y               = PlotConfig.yCell{iSubplot,iChannel};
                    elseif isfield(PlotConfig,'y')
                        y               = PlotConfig.y;
                    end                                      
                    % determine Indices and nLines
                    if isfield(PlotConfig,'IndicesCell')
                        Indices         = PlotConfig.IndicesCell{iSubplot,iChannel};
                        nLines          = length(Indices);
                    elseif isfield(PlotConfig,'Indices')
                        Indices         = PlotConfig.Indices;
                        nLines          = length(Indices); 
                    else
                        nLines          = length(ProcessResults.(ThisChannel));
                        Indices         = 1:nLines;                
                    end
                    % AdditionalPlotInputs
                    if isfield(PlotConfig,'AdditionalPlotInputs')
                        AdditionalPlotInputs = PlotConfig.AdditionalPlotInputs;
                    else
                        AdditionalPlotInputs = {};
                    end  
                    % yScale
                    if isfield(PlotConfig,'yScaleCell')
                        yScale      = PlotConfig.yScaleCell{iSubplot,iChannel};
                    elseif isfield(PlotConfig,'yScale')
                        yScale      = PlotConfig.yScale;
                    else
                        yScale      = 1;
                    end                     
                    % plot data
                    for iLine = 1:nLines
                        Index           = Indices(iLine);
                        plot(ProcessResults.(ThisChannel)(Index).(x),...
                             ProcessResults.(ThisChannel)(Index).(y)*yScale,AdditionalPlotInputs{:})                                                                                    
                    end  
                end
                % xlabel: can be defined for each subplot or globally
                if isfield(PlotConfig,'xlabelCell')
                    xlabel(PlotConfig.xlabelCell{iSubplot})
                elseif isfield(PlotConfig,'xlabel')
                    xlabel(PlotConfig.xlabel)
                end 
                % ylabel: can be defined for each subplot or globally
                if isfield(PlotConfig,'ylabelCell')
                    ylabel(PlotConfig.ylabelCell{iSubplot})
                elseif isfield(PlotConfig,'ylabel')
                    ylabel(PlotConfig.ylabel)
                end                
                % title: can be defined for each subplot or globally
                if isfield(PlotConfig,'titleCell')
                    title(PlotConfig.titleCell{iSubplot},'Interpreter','none')
                elseif isfield(PlotConfig,'title')
                    title(PlotConfig.title,'Interpreter','none')
                end                
                % legend: can be defined for each subplot or globally
                if isfield(PlotConfig,'legendCell')
                    legend(PlotConfig.legendCell{iSubplot,:},'Interpreter','none','location','best')
                elseif isfield(PlotConfig,'legend')
                    legend(PlotConfig.legend,'Interpreter','none','location','best')
                end                               
                % set global gca properties
                if isfield(PlotConfig,'gca')
                    PropertyFields      = fieldnames(PlotConfig.gca);
                    for iProperty=1:size(PropertyFields,1)
                        Property    = getfield(PlotConfig.gca,PropertyFields{iProperty});
                        set(gca,PropertyFields{iProperty},Property);
                    end
                end                 
            end                
            % xlabelLast: same for each figure
            if isfield(PlotConfig,'xlabelLast')
                xlabel(PlotConfig.xlabelLast)
            end
        end
    end
end
