# File Organization and Main Idea

This set of matlab functions provides tools to process simulation and measurement data.
The tools are intented to be as flexible as possible. 

## DataFunctions

### AddDerivedTimeResults
These files are called by 

`TimeResults = AddDerivedTimeResults(TimeResults,PostProcessingConfig)`

Here, `AddDerivedTimeResults` loops over all requested calculations. 
The calculations are specified in the lines of the cell 
`PostProcessingConfig.AddDerivedTimeResults`.
Following columns need to provided:
1. `Function`: a function handle with `@(TimeResults)`.

The main idea is to add more channels to `TimeResults`.
These functions have in common that they need as input `TimeResults`.
Additional information can be provided with the `options` input or after `(TimeResults,..,options)`. 
All functions should loop over all data files and provide the updated `TimeResults`.

The following example adds a channel with the pitch rate:

~~~
PostProcessingConfig.AddDerivedTimeResults ={
    @(TimeResults) ApplyFunctionToOneChannel(TimeResults,'BldPitch1',@(Time,Data) gradient(Data,Time),'BldPitchRate1',Unit='deg/s')
    }; 
~~~

The functions can be also called and tested individually, e.g. by 
~~~
TimeResults = ApplyFunctionToOneChannel(TimeResults,'BldPitch1',@(Time,Data) gradient(Data,Time),'BldPitchRate1',Unit='deg/s'); 
~~~

The following functions could also be loaded by `AddDerivedTimeResults` and also add further channels to `TimeResults`. 
They are for now in the `PostProcessing` folder, since the filenames are usually depending on the data files:
- AddLogToTimeResults
- AddREWSfromWindFieldToTimeResults
- AddRoscoToTimeResults

### CalculateStatistics
These files are called by 

`Statistics = CalculateStatistics(TimeResults,PostProcessingConfig)`

Here, `CalculateStatistics` loops over all requested calculations.
The calculations are specified in the lines of the cell 
`PostProcessingConfig.CalculateStatistics`.
Following columns need to provided:
1. `StatisticsID`: a string used for the variable. 
2. `Function`: a function handle with `@(Data,Time)`.
3. `ChannelCell`: a cell with the channel names to which the operation should be applied.

The main idea is to provide functions which run calculations for specific channels for each data file.
These functions have in common that they have as input `Data` and possibly `Time`.
Additional information can be provided with the `options` input or after `(Data,Time,..,options)`.

The following example calculates the mean of the channels `Wind1VelX` and `TwrBsMyt` for each data file.
A column using the `StatisticsID` and the channel name will be added for each channel, e.g. `Statistics.mean_TwrBsMyt`. 
Since `mean` is a build-in function, no additional function needs to be written. 
Further, the Damage Equivalent Loads (DEL) are calculated for the channels `TwrBsMyt` and `LSShftTq`.
The table will then contain further columns with e.g. `Statistics.DEL_4_TwrBsMyt`.

~~~
StartTime               = 60;       % [s]           time to start evaluation (all signals should be settled) 
WoehlerExponentSteel    = 4;        % [-]           typical value for steel
PostProcessingConfig.CalculateStatistics = { 
    'mean'          @(Data,Time)mean(Data(Time>=StartTime)) {'Wind1VelX';'TwrBsMyt'}
    'DEL_4'         @(Data,Time)CalculateDEL(Data(Time>=StartTime),Time(Time>=StartTime),WoehlerExponentSteel)      {'TwrBsMyt';'LSShftTq'}
    }; 
~~~

The functions can be also called and tested individually, e.g. by 
~~~
Data                    = TimeResults{1}.TwrBsMyt.Data;
Time                    = TimeResults{1}.TwrBsMyt.Time;
StartTime               = 60;       % [s]           time to start evaluation (all signals should be settled) 
WoehlerExponentSteel    = 4;        % [-]           typical value for steel
DEL_4_TwrBsMyt          = CalculateDEL(Data(Time>=StartTime),Time(Time>=StartTime),WoehlerExponentSteel);
~~~

The main difference to `CalculateFrequencyResults` are listed below.

### CalculateFrequencyResults
These files are called by 

`FrequencyResults = CalculateFrequencyResults(TimeResults,PostProcessingConfig)`

Here, `CalculateFrequencyResults` loops over all requested calculations. 
The calculations are specified in the lines of the cell 
`PostProcessingConfig.CalculateFrequencyResults`.
Following columns need to provided:
1. `CalculationID`: a string used for the variable. 
2. `Function`: a function handle with `@(Data,Time)`.
3. `ChannelCell`: a cell with the channel names to which the operation should be applied.

The main idea is to provide functions which run calculations for specific channels for each data file.
These functions have in common that they have as input `Data` and usually `Time`.
Calculations can be either
 - "single-channel": `Data`  and `Time` are `(:,1) double`, 
    the `ChannelCell` is then a `nChannel` x 1 cell, see e.g. `EstimateAutoSpectrum`.
 - "multi-channel": `Data`  and `Time` are (for 2 channels) a `(2,1) cell` containing time and data vectors,
    the `ChannelCell` is then a `nChannel` x 2 cell, see e.g. `EstimateCrossSpectrum`.

Additional information can be provided with the `options` input or after `(Data,Time,..,options)`. 

All functions should provide a free output struct, which is then stored in the `FrequencyResults`.

The following example estimates the auto-spectra of the channels `TwrBsMyt` and `RotSpeed` for each data file.
Here, struct arrays will be generated using the `CalculationID` and the channel name 
e.g. `FrequencyResults.AutoSpectrum_RotSpeed` containing `S` and `f` for each data file.
Further, the cross-spectra between the channels `REWS_WindField` and `v_0_est` 
as well as between `v_0_est` and `REWS` is estimated.

~~~
StartTime                   = 60;       % [s]           time to start evaluation (all signals should be settled) 
Step                        = 50;
nData                       = 600*100/Step; % assuming 100 Hz and 10 min
nBlocks                     = 4;
window                      = nData/nBlocks;
AutoSpectrumChannels        = {'TwrBsMyt';'RotSpeed'};
CrossSpectrumCombinations   = {'REWS_WindField','v_0_est';'v_0_est','REWS'};
PostProcessingConfig.CalculateFrequencyResults = {
    'AutoSpectrum'      @(Data,Time)EstimateAutoSpectrum(Data,Time,Step,StartTime=StartTime,window=window)  AutoSpectrumChannels;
    'CrossSpectrum'     @(Data,Time)EstimateCrossSpectrum(Data,Time,Step,StartTime=StartTime,window=window) CrossSpectrumCombinations;    
    };
~~~

The functions can be also called and tested individually, e.g. by 
~~~
Data         = TimeResults{1}.RotSpeed.Data;
Time         = TimeResults{1}.RotSpeed.Time;
StartTime    = 60;       % [s]           time to start evaluation (all signals should be settled)
Step         = 50;
nData        = 600*100/Step; % assuming 80 Hz and 10 min
nBlocks      = 4;
window       = nData/nBlocks;
Output       = EstimateAutoSpectrum(Data,Time,Step,StartTime=StartTime,window=window);
~~~

The main differences to `CalculateStatistics` are:
- `Statistics` is a table, `FrequencyResults` is a struct.
- Ony one channel can be processed by `CalculateStatistics` at once (might change in the future, but currently seems to be resonable).
- Every calculation of `CalculateStatistics` only can provide one scalar value as output, 
  the output of `CalculateFrequencyResults` is very flexible.

However, `Statistics` and `FrequencyResults` are clearly connected to each data file, in contrast to `ProcessResults`.

### CalculateProcessResults
These files are called by 

`ProcessResults = CalculateProcessResults(FrequencyResults,Statistics,PostProcessingConfig)`

Here, `CalculateProcessResults` loops over all requested calculations. 
The calculations are specified in the lines of the cell 
`PostProcessingConfig.CalculateProcessResults`.
Following columns need to provided:
1. `Function`: a function handle with `@(ProcessResults,FrequencyResults,Statistics)`.

The main idea is to provide functions which run calculations using `FrequencyResults`, `Statistics` 
and also previous calculations already in `ProcessResults`. 
While `ProcessResults` always needs to be provided (to keep previous calculations), 
`FrequencyResults` and `Statistics` are optional.  

Additional information can be provided with the `options` input or after `(ProcessResults,FrequencyResults,Statistics,..,options)`.
A common additional input is the name of the `FilterID`, which groups data files for certain calculations.

All functions provide free fields in the struct `ProcessResults`. 

The following example averages the auto-spectra of the channels `TwrBsMyt` over 6 seeds for wind speed bin.
Here, a data filter is first generated which provides the information, which data files correspond to which wind speed bin.
The data filter is also used to calculate the life-time weighted DEL. 
The generated fields are in this case `ProcessResults.LTW_DEL_4_TwrBsMyt` and `ProcessResults.LTW_DEL_4_TwrBsMyt_PerBin` 
containing the DELs as well as as a struct array 'ProcessResults.mean_AutoSpectrum_TwrBsMyt'
containing `S` and `f` for every data filter.

~~~
PreProcessingVariation  = { 'URef',[4:2:24],'%02d';
                            'Seed',[1:6]   ,'%02d'};
WoehlerExponentSteel    = 4;        % [-]           typical value for steel                            
PostProcessingConfig.CalculateProcessResults = {
    @(ProcessResults,FrequencyResults,Statistics) DataFilterPreProcessing(ProcessResults,PreProcessingVariation)
    @(ProcessResults,FrequencyResults,Statistics) CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,'DEL_4_TwrBsMyt',WoehlerExponent=4);
    @(ProcessResults,FrequencyResults,Statistics) EstimateAveragedAutoSpectra(ProcessResults,FrequencyResults,'AutoSpectrum_TwrBsMyt')    
        };
~~~

The functions can be also called and tested individually, e.g. by 
~~~
PreProcessingVariation  = { 'URef',[4:2:24],'%02d';
                            'Seed',[1:6]   ,'%02d'};
WoehlerExponentSteel    = 4;        % [-]           typical value for steel  
ProcessResults = DataFilterPreProcessing(struct(),PreProcessingVariation);
ProcessResults = CalculateLifeTimeWeightedDEL(ProcessResults,Statistics,'DEL_4_TwrBsMyt',WoehlerExponent=4);
ProcessResults = EstimateAveragedAutoSpectra(ProcessResults,FrequencyResults,'AutoSpectrum_TwrBsMyt');
~~~

### Others
These are functions which are called by any other of the functions above.

# Programming Style Guide

## MATLAB
Based on the [MATLAB Style Guidelines 2.0](https://de.mathworks.com/matlabcentral/fileexchange/46056-matlab-style-guidelines-2-0): 

### Naming
* For functions, use an imperative (`CalculateMean`).
* Use `ALL_CAPS_WITH_UNDERSCORES` for constants.

Deviations from the Matlab Style Guide
* When using acronyms, capitalize all the letters of the acronym. Thus HTTPServerError is better than HttpServerError. Based on [PEP8](https://peps.python.org/pep-0008/).
* Use `UpperCamelCase` for variables and functions. 
This simplfies the coding a lot, since we don't need to differentiate between structs and variables 
and the naming of `TimeResults` is consistent with `PlotTimeResults` other than `timeResults` and `plotTimeResults`. 
Exceptions:
  * first letter is a single letter, e.g. `iDataFiles`,`nFunctions`
  * variable represents a matlab command, e.g. `PlotConfig.ylabel`
* Deviate from the nameing conventions if it improves understandability (e.g. math symbols).