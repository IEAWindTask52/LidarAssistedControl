function [FrequencyResults,Statistics] = CombineResultFiles(ResultFiles,options)

% inputs
arguments
    ResultFiles         cell
    options.StartTime   datetime = datetime(-inf,0,0)
    options.EndTime     datetime = datetime(inf,0,0)
end

% dimensions
nResultFiles    = length(ResultFiles);

% load data
for iResultFile = 1:nResultFiles
    Data{iResultFile} = load(ResultFiles{iResultFile},'FrequencyResults','Statistics');
end

% combine Statistics
Statistics      = Data{1}.Statistics;
for iResultFile = 2:nResultFiles
    Statistics  = outerjoin(Statistics,Data{iResultFile}.Statistics,'MergeKeys',true);
end

% shorten Statistics
if options.EndTime~=datetime(inf,0,0)||options.StartTime~=datetime(-inf,0,0)
    if any(ismember(Statistics.Properties.VariableNames,'Time'))
        Considered  = Statistics.Time>=options.StartTime & Statistics.Time<options.EndTime;
    else
        warning('Option StartTime/EndTime not applied, since Statistics.Time is missing.')
        Considered  = true(height(Statistics),1);
    end
else
    Considered  = true(height(Statistics),1);
end
Statistics  = Statistics(Considered,:);

% combine FrequencyResults
FieldNames      = fieldnames(Data{1}.FrequencyResults);
nFieldNames     = size(FieldNames,1);
for iResultFile = 1:nResultFiles
    for iFieldName = 1:nFieldNames
        ThisFieldName = FieldNames{iFieldName};
        if iResultFile==1
            FrequencyResults.(ThisFieldName) = Data{iResultFile}.FrequencyResults.(ThisFieldName);
        else
            if isfield(Data{iResultFile}.FrequencyResults,ThisFieldName)
                FrequencyResults.(ThisFieldName) = [FrequencyResults.(ThisFieldName),Data{iResultFile}.FrequencyResults.(ThisFieldName)];
            end
        end
    end
end

% shorten FrequencyResults
if sum(Considered)<sum(FrequencyResults.nDataFiles)
    for iFieldName = 1:nFieldNames
        ThisFieldName = FieldNames{iFieldName};
        if ~strcmp(ThisFieldName,'nDataFiles')
            FrequencyResults.(ThisFieldName) = FrequencyResults.(ThisFieldName)(Considered);
        end
    end
end
FrequencyResults.nDataFiles = sum(Considered); 

end