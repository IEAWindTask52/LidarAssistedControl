function PlotStatistics(Statistics,PostProcessingConfig)

%% BasicStatisticsPlot
if isfield(PostProcessingConfig.Plots,'BasicStatisticsPlot')
    nFigures            = length(PostProcessingConfig.Plots.BasicStatisticsPlot);    
    for iFigure=1:nFigures
        PlotConfig      = PostProcessingConfig.Plots.BasicStatisticsPlot{iFigure};
        if PlotConfig.Enable
            nSubplots = size(PlotConfig.Variables,1);
            figure('Name',['BasicStatisticsPlot ',num2str(iFigure)])
            for iSubplot = 1:nSubplots
                ThisVariable  = PlotConfig.Variables{iSubplot};
                subplot(nSubplots,1,iSubplot)                    
                hold on; box on; grid on
                % plot legend
                if isfield(PlotConfig,'scale')
                    scale = PlotConfig.scale;
                else
                    scale = 1;                    
                end                
                plot(PlotConfig.x,Statistics.(ThisVariable)*scale)
                % ylabel: can be defined for each subplot or globally
                if isfield(PlotConfig,'ylabelCell')
                    ylabel(PlotConfig.ylabelCell{iSubplot})
                elseif isfield(PlotConfig,'ylabel')
                    ylabel(PlotConfig.ylabel)
                end
                % plot title: fixed to variables
                title(ThisVariable,'Interpreter','none')
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
            % xlabel
            if isfield(PlotConfig,'xlabel')
                xlabel(PlotConfig.xlabel)
            end              
        end
    end
end

%% ComparisonStatisticsPlot
if isfield(PostProcessingConfig.Plots,'ComparisonStatisticsPlot')
    nFigures         = length(PostProcessingConfig.Plots.ComparisonStatisticsPlot);    
    for iFigure=1:nFigures
        PlotConfig      = PostProcessingConfig.Plots.ComparisonStatisticsPlot{iFigure};
        if PlotConfig.Enable
            figure('Name',['ComparisonStatisticsPlot ',num2str(iFigure)])                   
            hold on; box on; grid on                
            nVariable   = size(PlotConfig.Variables,1);
            for iVariable = 1:nVariable
                ThisVariable  = PlotConfig.Variables{iVariable};
                plot(PlotConfig.x,Statistics.(ThisVariable))
            end
            % xlabel
            if isfield(PlotConfig,'xlabel')
                xlabel(PlotConfig.xlabel)
            end
            % ylabel
            if isfield(PlotConfig,'ylabel')
                ylabel(PlotConfig.ylabel)
            end            
            % plot title
            if isfield(PlotConfig,'title')
                title(PlotConfig.title,'Interpreter','none')
            end
            % plot legend: fixed to variables
            if isfield(PlotConfig,'legend')
                legend(PlotConfig.legend,'Interpreter','none')
            else
                legend(PlotConfig.Variables,'Interpreter','none')              
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
    end
end

%% Other more advanced plots
end