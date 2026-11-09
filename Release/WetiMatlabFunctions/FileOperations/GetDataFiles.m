function DataFiles = GetDataFiles(FirstFileName,LastFileName,Folder,FileExtension,options)
% inputs
arguments
    FirstFileName           char
    LastFileName            char
    Folder                  char
    FileExtension           char    = 'dat'
    options.MinFileSize     double  = []
end

switch FileExtension
    case('dat')
        FileInfo        = dir(fullfile(Folder, '**', '*.dat')); % finds all dat files in folder and subfolders  
        AllFiles        = cellstr(char(FileInfo(:).name));
        IdxFirst        = find(strcmp(AllFiles,[FirstFileName,'.dat']));
        IdxLast         = find(strcmp(AllFiles,[LastFileName ,'.dat']));
        if isempty(IdxFirst)
            error('First file %s.dat not found.',FirstFileName)
        end
        if isempty(IdxLast)
            error('Last file %s.dat not found.',LastFileName)
        end
        nFiles          = length(AllFiles);
        Considered      = [1:nFiles]' >= IdxFirst & [1:nFiles]' <= IdxLast;
        % check File Size
        if ~isempty(options.MinFileSize)
           Considered   = Considered & cat(1,FileInfo(:).bytes) >= options.MinFileSize;
        end
        % get DateFiles
        DataFiles   = fullfile( cellstr(char(FileInfo(Considered).folder)),...
                                cellstr(char(FileInfo(Considered).name  )) ); 
        case('CSV')
        FileInfo        = dir(fullfile(Folder, '**', '*.CSV')); % finds all csv files in folder and subfolders  
        AllFiles        = cellstr(char(FileInfo(:).name));
        IdxFirst        = find(strcmp(AllFiles,[FirstFileName,'.CSV']));
        IdxLast         = find(strcmp(AllFiles,[LastFileName ,'.CSV']));
        if isempty(IdxFirst)
            error('First file %s.dat not found.',FirstFileName)
        end
        if isempty(IdxLast)
            error('Last file %s.dat not found.',LastFileName)
        end
        nFiles          = length(AllFiles);
        Considered      = (1:nFiles)' >= IdxFirst & (1:nFiles)' <= IdxLast;
        % check File Size
        if ~isempty(options.MinFileSize)
            Considered = Considered & cat(1,FileInfo(:).bytes) >= options.MinFileSize;
        end
        % get DateFiles
        DataFiles = fullfile(   cellstr(char(FileInfo(Considered).folder)), ...
                                cellstr(char(FileInfo(Considered).name  )) );

    otherwise
        error('Only dat files implemented so far')
end