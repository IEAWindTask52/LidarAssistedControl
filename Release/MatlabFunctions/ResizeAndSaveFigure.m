function  ResizeAndSaveFigure( Width,Height,FileName)

set(0,'units','centimeters');
ScreenSize  = get(0,'ScreenSize');

NewPosition = [(ScreenSize(3)-Width)/2,...
                    (ScreenSize(4)-Height)/2,...
                    Width,...
                    Height];
set(gcf,'units','centimeters');                
set(gcf,'position',NewPosition);
set(gcf,'PaperPositionMode','manual');
set(gcf,'PaperUnits','centimeter');
set(gcf,'PaperSize',[Width Height]);
set(gcf,'PaperPosition',[0 0 Width Height]);
saveas(gcf,FileName)

end

