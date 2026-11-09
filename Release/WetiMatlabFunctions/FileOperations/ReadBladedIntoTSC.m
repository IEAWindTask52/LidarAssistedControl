function TSC = ReadBladedIntoTSC(projectFile, mode, options)
% standalone version of readOutput from mallard, needs only ReadBladedOutput

arguments
    projectFile char
    mode {mustBeMember(mode, ["default", "all", "only"])} = "default"
    options.Ids (1, :) {mustBeInteger, mustBePositive} = [];
end

DEFAULT_IDS = [4, 5, 6, 7, 8, 9, 14, 26, 29, 69];

switch mode
    case "default"
        ids = unique([DEFAULT_IDS, options.Ids]);
    case "all"
        ids = 1:300;
    case "only"
        ids = unique(options.Ids);
end

[pathstr, name] = fileparts(projectFile);
FileNameRoot    = fullfile(pathstr,name);
FileInfo        = dir([FileNameRoot,'.*']);
IsHeaderFile    = contains(cellstr(char(FileInfo.name)),'.%');
headerFiles     = fullfile(cellstr(char(FileInfo(IsHeaderFile).folder)),...
                           cellstr(char(FileInfo(IsHeaderFile).name)));

for headerFile = headerFiles'
    
    [~, ~, ext]         = fileparts(headerFile);
    fileNumberString    = regexprep(string(ext), "[^\d]", "");
    if ~any(double(fileNumberString) == ids)
        continue; end
    
    [time,data,channelNames,Units] = ReadBladedOutput(headerFile);

    % create a tscollection object
    if ~exist("TSC")
        TSC                 = tscollection(time);
        TSC.Name            = FileNameRoot; % set name 
        TSC.TimeInfo.Units  = 'seconds';
    end

    % loop over channels
    nChannel            = size(channelNames,1);
    for iChannel = 1:nChannel
        % create time series
        TS                  = timeseries(channelNames{iChannel});
        TS.Time             = time;
        TS.Data             = data(iChannel,:)';
        TS.DataInfo.Units   = char(Units(iChannel));
        % add to tscollection object 
        TSC = TSC.addts(TS);
    end

end