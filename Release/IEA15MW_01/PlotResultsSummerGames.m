% -------------------------------------------------------------------------
%
%   [Description]
%   This script displays and saves the results of the LAC SummerGames 2024 
%   30s sprint submitted by students and researchers.
%
% -------------------------------------------------------------------------
%% Setup

clearvars; close all; clc
addpath(genpath('..\..\..\WetiMatlabFunctions'))
addpath(genpath('ResultsStudents'))
addpath(genpath('ResultsResearchers'))

SimulationName  = 'IEA-15-240-RWT-Monopile';
PlotType        = 'students';                    % [str]     'default', 'students'

%% Read/Plot/Save Data

switch PlotType
    case 'default'
        % Read Data
        FB          = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
        SOW         = ReadFASTbinaryIntoStruct([SimulationName,'_Sowento.outb']);
        FUAS        = ReadFASTbinaryIntoStruct([SimulationName,'_FUAS.outb']);
        
        % Plot Data
        nSubplots       = 5;
        FontSize        = 6;

        figure
        subplot(nSubplots, 1, 1)
        hold on; grid on; box on;
        plot(FB.Time,   FB.Wind1VelX,   LineWidth=1.5)
        plot(SOW.Time,  SOW.VLOS01LI,   LineWidth=1.5)
        legend('Wind1VelX','VLOS01LI','Fontsize',FontSize)
        ylabel({'WindSpeed', '[m/s]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 2)
        hold on; grid on; box on;
        plot(FB.Time,   FB.BldPitch1,   LineWidth=1.5)
        plot(SOW.Time,  SOW.BldPitch1,  LineWidth=1.5)
        plot(FUAS.Time, FUAS.BldPitch1, LineWidth=1.5)
        ylabel({'BldPitch1'; '[deg]'},'Fontsize', FontSize);
        set(gca,'Fontsize', FontSize);

        subplot(nSubplots, 1, 3)
        hold on; grid on; box on;
        plot(FB.Time,   FB.GenTq/1e3,   LineWidth=1.5)
        plot(SOW.Time,  SOW.GenTq/1e3,  LineWidth=1.5)
        plot(FUAS.Time, FUAS.GenTq/1e3, LineWidth=1.5)
        legend('FB', 'SOW', 'FUAS', 'NumColumns', 2)
        ylabel({'GenTq';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 4)
        hold on; grid on; box on;
        plot(FB.Time,   FB.RotSpeed,    LineWidth=1.5)
        plot(SOW.Time,  SOW.RotSpeed,   LineWidth=1.5)
        plot(FUAS.Time, FUAS.RotSpeed,  LineWidth=1.5)
        ylabel({'RotSpeed';'[rpm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 5)
        hold on; grid on; box on;
        plot(FB.Time,   FB.TwrBsMyt/1e3,    LineWidth=1.5)
        plot(SOW.Time,  SOW.TwrBsMyt/1e3,   LineWidth=1.5)
        plot(FUAS.Time, FUAS.TwrBsMyt/1e3,  LineWidth=1.5)
        ylabel({'TwrBsMyt';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        xlabel('time [s]','Fontsize',FontSize);
        linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
        xlim([5 30])

        % Save Plot
        ResizeAndSaveFigure(10,10,'SprintDefault.pdf')
        

    case 'students'
        % Read Data
        FB          = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
        FUAS        = ReadFASTbinaryIntoStruct([SimulationName,'_FUAS.outb']);
        %UDELAR_1    = ReadFASTbinaryIntoStruct([SimulationName,'_UDELAR_1.outb']);
        %UDELAR_2    = ReadFASTbinaryIntoStruct([SimulationName,'_UDELAR_2.outb']);
        %OUC         = ReadFASTbinaryIntoStruct([SimulationName,'_OCU.outb']);

        % Plot Data
        nSubplots       = 5;
        FontSize        = 6;

        figure
        subplot(nSubplots, 1, 1)
        hold on; grid on; box on;
        plot(FB.Time,       FB.Wind1VelX,       LineWidth=1.5)
        plot(FUAS.Time,     FUAS.VLOS01LI,      LineWidth=1.5)
        legend('Wind1VelX','VLOS01LI','Fontsize',FontSize)
        ylabel({'WindSpeed', '[m/s]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 2)
        hold on; grid on; box on;
        plot(FB.Time,       FB.BldPitch1,       LineWidth=1.5)
        plot(FUAS.Time,     FUAS.BldPitch1,     LineWidth=1.5)
        %plot(UDELAR_1.Time, UDELAR_1.BldPitch1, LineWidth=1.5)
        %plot(UDELAR_2.Time, UDELAR_2.BldPitch1, LineWidth=1.5)
        %plot(OUC.Time,      OUC.BldPitch1,      LineWidth=1.5)
        ylabel({'BldPitch1'; '[deg]'},'Fontsize', FontSize);
        set(gca,'Fontsize', FontSize);

        subplot(nSubplots, 1, 3)
        hold on; grid on; box on;
        plot(FB.Time,       FB.GenTq/1e3,       LineWidth=1.5)
        plot(FUAS.Time,     FUAS.GenTq/1e3,     LineWidth=1.5)
        %plot(UDELAR_1.Time, UDELAR_1.GenTq/1e3, LineWidth=1.5)
        %plot(UDELAR_2.Time, UDELAR_2.GenTq/1e3, LineWidth=1.5)
        %plot(OUC.Time,      OUC.GenTq/1e3,      LineWidth=1.5)
        legend('FB', 'FUAS', 'UDELAR_1', 'UDELAR_2', 'OUC', 'NumColumns', 2)
        ylabel({'GenTq';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 4)
        hold on; grid on; box on;
        plot(FB.Time,       FB.RotSpeed,        LineWidth=1.5)
        plot(FUAS.Time,     FUAS.RotSpeed,      LineWidth=1.5)
        %plot(UDELAR_1.Time, UDELAR_1.RotSpeed,  LineWidth=1.5)
        %plot(UDELAR_2.Time, UDELAR_2.RotSpeed,  LineWidth=1.5)
        %plot(OUC.Time,      OUC.RotSpeed,       LineWidth=1.5)
        ylabel({'RotSpeed';'[rpm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 5)
        hold on; grid on; box on;
        plot(FB.Time,       FB.TwrBsMyt/1e3,        LineWidth=1.5)
        plot(FUAS.Time,     FUAS.TwrBsMyt/1e3,      LineWidth=1.5)
        %plot(UDELAR_1.Time, UDELAR_1.TwrBsMyt/1e3,  LineWidth=1.5)
        %plot(UDELAR_2.Time, UDELAR_2.TwrBsMyt/1e3,  LineWidth=1.5)
        %plot(OUC.Time,      OUC.TwrBsMyt/1e3,       LineWidth=1.5)
        ylabel({'TwrBsMyt';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        xlabel('time [s]','Fontsize',FontSize);
        linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
        xlim([5 30])

        % Save Plot
        ResizeAndSaveFigure(10,10,'SprintStudents.pdf')
end
