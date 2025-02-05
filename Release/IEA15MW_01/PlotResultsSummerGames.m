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
PlotType        = 'students';                    % [str]     'students', 'researchers'

%% Read/Plot/Save Data

switch PlotType
    case 'researchers'
        % Read Data
        FB          = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
        SOW         = ReadFASTbinaryIntoStruct([SimulationName,'_Sowento.outb']);
        SJTU        = ReadFASTbinaryIntoStruct([SimulationName,'_SJTU.outb']);
        UPV         = ReadFASTbinaryIntoStruct([SimulationName,'_UPV.outb']);

        
        % Plot Data
        nSubplots       = 5;
        FontSize        = 6;

        figure
        subplot(nSubplots, 1, 1)
        hold on; grid on; box on;
        plot(FB.Time,   FB.Wind1VelX)
        plot(SOW.Time,  SOW.VLOS01LI)
        legend('Wind1VelX','VLOS01LI','Fontsize',FontSize)
        ylabel({'WindSpeed', '[m/s]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 2)
        hold on; grid on; box on;
        plot(FB.Time,   FB.BldPitch1)
        plot(SOW.Time,  SOW.BldPitch1)
        plot(SJTU.Time, SJTU.BldPitch1)
        plot(UPV.Time,  UPV.BldPitch1)
        ylabel({'BldPitch1'; '[deg]'},'Fontsize', FontSize);
        set(gca,'Fontsize', FontSize);

        subplot(nSubplots, 1, 3)
        hold on; grid on; box on;
        plot(FB.Time,   FB.GenTq/1e3)
        plot(SOW.Time,  SOW.GenTq/1e3)
        plot(SJTU.Time, SJTU.GenTq/1e3)
        plot(UPV.Time,  UPV.GenTq/1e3)        
        legend('FB', 'sowento', 'SJTU', 'UPV', 'NumColumns', 2)
        ylabel({'GenTq';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 4)
        hold on; grid on; box on;
        plot(FB.Time,   FB.RotSpeed)
        plot(SOW.Time,  SOW.RotSpeed)
        plot(SJTU.Time, SJTU.RotSpeed)
        plot(UPV.Time,  UPV.RotSpeed)
        ylabel({'RotSpeed';'[rpm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 5)
        hold on; grid on; box on;
        plot(FB.Time,   FB.TwrBsMyt/1e3)
        plot(SOW.Time,  SOW.TwrBsMyt/1e3)
        plot(SJTU.Time, SJTU.TwrBsMyt/1e3)
        plot(UPV.Time,  UPV.TwrBsMyt/1e3)
        ylabel({'TwrBsMyt';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        xlabel('time [s]','Fontsize',FontSize);
        linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
        xlim([5 30])

        % Save Plot
        ResizeAndSaveFigure(12,10,'SprintResearchers.pdf')
        

    case 'students'
        % Read Data
        FB          = ReadFASTbinaryIntoStruct([SimulationName,'_FB.outb']);
        UDELAR_1    = ReadFASTbinaryIntoStruct([SimulationName,'_UDELAR_1.outb']);
        UDELAR_2    = ReadFASTbinaryIntoStruct([SimulationName,'_UDELAR_2.outb']);
        FUAS        = ReadFASTbinaryIntoStruct([SimulationName,'_FUAS.outb']);
        OUC         = ReadFASTbinaryIntoStruct([SimulationName,'_OUC.outb']);

        % Plot Data
        nSubplots       = 5;
        FontSize        = 6;

        figure
        subplot(nSubplots, 1, 1)
        hold on; grid on; box on;
        plot(FB.Time,       FB.Wind1VelX)
        plot(FUAS.Time,     FUAS.VLOS01LI)
        legend('Wind1VelX','VLOS01LI','Fontsize',FontSize)
        ylabel({'WindSpeed', '[m/s]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 2)
        hold on; grid on; box on;
        plot(FB.Time,       FB.BldPitch1)
        plot(FUAS.Time,     FUAS.BldPitch1)
        plot(UDELAR_1.Time, UDELAR_1.BldPitch1)
        plot(UDELAR_2.Time, UDELAR_2.BldPitch1)
        plot(OUC.Time,      OUC.BldPitch1)
        ylabel({'BldPitch1'; '[deg]'},'Fontsize', FontSize);
        set(gca,'Fontsize', FontSize);

        subplot(nSubplots, 1, 3)
        hold on; grid on; box on;
        plot(FB.Time,       FB.GenTq/1e3)
        plot(FUAS.Time,     FUAS.GenTq/1e3)
        plot(UDELAR_1.Time, UDELAR_1.GenTq/1e3)
        plot(UDELAR_2.Time, UDELAR_2.GenTq/1e3)
        plot(OUC.Time,      OUC.GenTq/1e3)
        legend('FB', 'FUAS', 'Udelar1', 'Udelar2', 'OUC', 'NumColumns', 2)
        ylabel({'GenTq';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 4)
        hold on; grid on; box on;
        plot(FB.Time,       FB.RotSpeed)
        plot(FUAS.Time,     FUAS.RotSpeed)
        plot(UDELAR_1.Time, UDELAR_1.RotSpeed)
        plot(UDELAR_2.Time, UDELAR_2.RotSpeed)
        plot(OUC.Time,      OUC.RotSpeed)
        ylabel({'RotSpeed';'[rpm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        subplot(nSubplots, 1, 5)
        hold on; grid on; box on;
        plot(FB.Time,       FB.TwrBsMyt/1e3)
        plot(FUAS.Time,     FUAS.TwrBsMyt/1e3)
        plot(UDELAR_1.Time, UDELAR_1.TwrBsMyt/1e3)
        plot(UDELAR_2.Time, UDELAR_2.TwrBsMyt/1e3)
        plot(OUC.Time,      OUC.TwrBsMyt/1e3)
        ylabel({'TwrBsMyt';'[MNm]'},'Fontsize',FontSize);
        set(gca,'Fontsize',FontSize);

        xlabel('time [s]','Fontsize',FontSize);
        linkaxes(findobj(gcf, 'Type', 'Axes'),'x');
        xlim([5 30])

        % Save Plot
        ResizeAndSaveFigure(12,10,'SprintStudents.pdf')
end
