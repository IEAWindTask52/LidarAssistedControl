function PlotTimeResults(TimeResults,PostProcessingConfig)

%% BasicTimePlot
if isfield(PostProcessingConfig.Plots,'BasicTimePlot')
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
                % plot data
                for iConsideredDataFile=1:nConsideredDataFiles
                    iDataFile               = IndicesConsideredDataFiles(iConsideredDataFile);
                    plot(TimeResults{iDataFile}.(ThisChannel))                                                                                
                end
                % ylabel: use unit from last considered file
                ylabel(['[',TimeResults{iDataFile}.(ThisChannel).DataInfo.Units,']'])
                % plot title: fixed to channels
                title(ThisChannel,'Interpreter','none')
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
        end
    end
end

%% ComparisonTimePlot
if isfield(PostProcessingConfig.Plots,'ComparisonTimePlot')
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
                for iChannel = 1:nChannels
                    ThisChannel  = PlotConfig.Channels{iChannel};
                    plot(TimeResults{iConsideredDataFile}.(ThisChannel))  
                end
                % ylabel: need to be defined due to different channels 
                if isfield(PlotConfig,'ylabel')
                    ylabel(PlotConfig.ylabel)
                end
                % plot title
                if isfield(PlotConfig,'title')
                    title(PlotConfig.title{iDataFile},'Interpreter','none')
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
        end
    end
end

%% Other more advanced time plots
end