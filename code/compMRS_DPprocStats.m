% compMRS_DPprocStats.m
% Diana Rotaru, Columbia University & Medical University of Vienna, 2025
% Mark Mikkelsen, Weill Cornell Medicine, 2025
%
% USAGE:
% [out]=compMRS_DPprocStats(allDPs_parentfolder)
%
% DESCRIPTION:
% Computes and compiles processing statistics across all data packets.
% Extracts relevant quantitative processing outputs and summarizes them
% into a single overview for quality control and downstream evaluation.
%
% INPUTS:
% allDPs_parentfolder = directory containing all data packets
% The scripts needs to be ran within the folder containing the xlsx, and
% mat files needed for information extraction
%
% OUTPUTS:
% out = excel file containing processing statistics

clear; clc;
% dir_name = mfilename("fullpath");
% dir_name = fileparts(fileparts(dir_name));
% data_dir = fullfile(dir_name,'data','supplementary');
%
% %% File names
% xlsxFile = fullfile(data_dir,'CoMP-MRS_participantSpreadsheet.xlsx');
% matFile  = fullfile(data_dir,'CoMP-MRS.mat');
% siteFile = fullfile(data_dir,'CoMP-MRS-sites.xlsx');
% outCsv   = fullfile(data_dir,'CoMP_MRS_Rstats_input_v1.csv');

cd('A:\CoMP-MRS\Data\data\supplementary')
%% File names
xlsxFile = 'CoMP-MRS_participantSpreadsheet.xlsx';
matFile  = 'CoMP-MRS.mat';
siteFile = 'CoMP-MRS-sites.xlsx';
outCsv   = 'CoMP_MRS_Rstats_input.csv';

%% Exceptions
% DP05-sub-02 corrupted data
% DP07-sub-01 rejected due to distortions; SNR/LW ~ 0.3;
% DP16-sub-02 corrupted data
% DP17-sub01, sub-04 discard due to poor quality
% DP21-sub-01 discard due to distortions near NAA
% DP22-sub-04, sub-07 fix phase
% DP29-sub-02, sub-03, maybe sub-05 too fix phase + frequency, sub-01 and sub-04 have voxel sizes of 3.0 x 1.5 x 3.0 and 3.1 x 2.1 x 3.0 instead of 2.5 x 1.5 x 3.0
% DP30-sub-08 fix phase
% DP33-no data
% DP35-no data

%% Differences
% DP04-sub-01 very high LW => SNR/LW ~ 0.7, x5-10 smaller than the rest
% DP06-sub-07 voxel size is 3.1 x 0.9 x 1.3 instead of 3.1 x 0.9 x 1.4
% DP07-sub-06 very low SNR/LW similar to sub01, ~0.8
% DP08-sub-01, sub-03, sub-04 3.8 x 2.3 x 1.8 instead of 3.8 x 2.5 x 1.8;  sub-06, sub-07, sub-08 have SNR/LW ratios x5, x4, x8 times higher than the others
% DP09-sub06 SNR/LW ~ 0.4 and x8-10 smaller than other ratios; sub-08 SNR/LW ~ 0.7 and x6-8 smaller than other ratios;
% DP13-sub03, sub-04 - high lactate
% DP14-all high lactate
% DP17-sub01 L-hippocampus, all others R-hippocampus
% DP18-sub-02 no weight
% DP19-all different voxel size
% DP20-all high lactate
% DP22-sub-04, sub-07 fix phase
% DP23-all high taurine
% DP25-all SNR/LW differences of an order of magnitude due to low SNR
% DP27-only sub-06 ses-02and sub-08 se-s02 are 2.3 x 1.2 x 2.0 instead of 2.5 x 1.2 x 2.0; not relevant here
% DP29-sub-02, sub-03, maybe sub-05 too fix phase + frequency, sub-01 and sub-04 have voxel sizes of 3.0 x 1.5 x 3.0 and 3.1 x 2.1 x 3.0 instead of 2.5 x 1.5 x 3.0
% DP30-sub-08 fix phase, sub-01-05 high lactate, sub-03 low SNR/LW
% DP36-all very low SNR/LW ratios

%% ------------------------------------------------------------------------
% Read Excel file
%% ------------------------------------------------------------------------
opts = detectImportOptions(xlsxFile, 'NumHeaderLines', 9);
opts.VariableNamesRange = '10:10';
opts.DataRange = '11:10000';

fullTable = readtable(xlsxFile, opts);

%% ------------------------------------------------------------------------
% Extract selected columns
%% ------------------------------------------------------------------------
% in 'CoMP-MRS_participantSpreadsheet.xlsx'
% 1  = data.packet.ID
% 3  = animal.ID
% 6  = animal.species
% 7  = animal.strain
% 9  = animal.ses-01.age
% 11 = animal.sex
% 12 = animal.ses-01.weight
% 28 = MRI.vendor
% 29 = MRI.field.strength
% 35 = MRS.pulse.sequence
% 36 = MRS.brain.region
% 37 = MRS.VoI.size
% 44 = MRS.n.ave
selectedCols = [1 3 6 7 9 11 12 28 29 35 36 37 44];
selectedData = fullTable(:, selectedCols);

%% ------------------------------------------------------------------------
% Fill DP labels
%% ------------------------------------------------------------------------
rawDP = fullTable{:,1};

if iscell(rawDP)
    dpLabels = string(rawDP);
elseif isstring(rawDP)
    dpLabels = rawDP;
else
    dpLabels = string(rawDP);
end

dpLabels(dpLabels == "") = missing;
dpLabels = fillmissing(dpLabels, 'previous');
dpLabels = regexprep(dpLabels, '\s*\(.*?\)', '');
dpLabels = strtrim(dpLabels);

%% ------------------------------------------------------------------------
% Keep only DP01-DP36
%% ------------------------------------------------------------------------
nRows = height(fullTable);
dpNumAll = nan(nRows,1);

for i = 1:nRows
    token = regexp(dpLabels(i), 'DP\s*0*(\d+)', 'tokens', 'once');
    if ~isempty(token)
        dpNumAll(i) = str2double(token{1});
    end
end

validIdx = ~isnan(dpNumAll) & dpNumAll <= 36;

fullTable    = fullTable(validIdx,:);
selectedData = selectedData(validIdx,:);
dpLabels     = dpLabels(validIdx);

nRows = height(fullTable);

%% ------------------------------------------------------------------------
% Subject numbering within DP
%% ------------------------------------------------------------------------
subjectNumberWithinDP = zeros(nRows,1);

currentDP = "";
countInCurrentDP = 0;

for i = 1:nRows
    if dpLabels(i) ~= currentDP
        currentDP = dpLabels(i);
        countInCurrentDP = 1;
    else
        countInCurrentDP = countInCurrentDP + 1;
    end
    subjectNumberWithinDP(i) = countInCurrentDP;
end

%% ------------------------------------------------------------------------
% Build participant table
% NOTE:
% Pull by original spreadsheet column number, not by position in selectedData.
%% ------------------------------------------------------------------------
participantTable = table();

participantTable.PacketID      = local_get_selected_column(selectedData, selectedCols, 1);
participantTable.AnimalID      = string(local_get_selected_column(selectedData, selectedCols, 3));
participantTable.AnimalSpecies = local_get_selected_column(selectedData, selectedCols, 6);
participantTable.AnimalStrain  = local_get_selected_column(selectedData, selectedCols, 7);
participantTable.AnimalAge     = local_get_selected_column(selectedData, selectedCols, 9);
participantTable.AnimalSex     = local_get_selected_column(selectedData, selectedCols, 11);
participantTable.AnimalWeight  = local_get_selected_column(selectedData, selectedCols, 12);

participantTable.MRvendor      = local_get_selected_column(selectedData, selectedCols, 28);
participantTable.MRfield       = local_get_selected_column(selectedData, selectedCols, 29);
participantTable.MRsequence    = local_get_selected_column(selectedData, selectedCols, 35);
participantTable.MRbrainregion = local_get_selected_column(selectedData, selectedCols, 36);
participantTable.VoISize       = local_get_selected_column(selectedData, selectedCols, 37);
participantTable.MRaverages    = local_get_selected_column(selectedData, selectedCols, 44);

%% ------------------------------------------------------------------------
% Add extra fields from fullTable by header name
%% ------------------------------------------------------------------------
participantTable.MRsoftwareversion = local_get_fulltable_column(fullTable, "MRI.software.version");
participantTable.MRcoildetail      = local_get_fulltable_column(fullTable, "MRI.coil.detail");
participantTable.MRSsw             = local_get_fulltable_column(fullTable, "MRS.sw");
participantTable.MRSnpts           = local_get_fulltable_column(fullTable, "MRS.n.pts");
participantTable.MRSTE             = local_get_fulltable_column(fullTable, "MRS.TE");
participantTable.MRSTR             = local_get_fulltable_column(fullTable, "MRS.TR");
participantTable.MRSshimmethod     = local_get_fulltable_column(fullTable, "MRS.shim.method");

%% ------------------------------------------------------------------------
% Standardize string values
%% ------------------------------------------------------------------------
participantTable.MRbrainregion = string(participantTable.MRbrainregion);
participantTable.MRvendor = string(participantTable.MRvendor);
participantTable.AnimalStrain = string(participantTable.AnimalStrain);
participantTable.MRSshimmethod = string(participantTable.MRSshimmethod);

% MRbrainregion
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, ...
    "Right hippocampus", "Rhippocampus");
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, ...
    "Left hippocampus", "Lhippocampus");
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, ...
    "Right striatum", "Rstriatum");
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, ...
    "Left striatum", "Lstriatum");

% MRvendor
participantTable.MRvendor = strrep(participantTable.MRvendor, ...
    "Agilent/Varian", "Varian");
participantTable.MRvendor = strrep(participantTable.MRvendor, ...
    "Varian/Agilent", "Varian");

% AnimalStrain
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, ...
    "C57BL/6J", "C57BL-6J");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, ...
    "C57BL/6", "C57BL-6");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, ...
    "balb/c", "BALB-c");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, ...
    "BALB/c", "BALB-c");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, ...
    "B6129SF2/J", "B6129SF2-J");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, ...
    "FVB/N", "FVB-N");

% MRSshimmethod
participantTable.MRSshimmethod = strrep(participantTable.MRSshimmethod, ...
    "FASTMAP/FASTESTMAP", "FASTMAP-FASTESTMAP");

%% ------------------------------------------------------------------------
% Standardize MRfield
% Replace all values between 9 and 9.4 with 9.4
%% ------------------------------------------------------------------------
participantTable.MRfield = str2double(string(participantTable.MRfield));
idxField = participantTable.MRfield >= 9 & participantTable.MRfield < 9.4;
participantTable.MRfield(idxField) = 9.4;

%% ------------------------------------------------------------------------
% Compute MRvoxelvolume
%% ------------------------------------------------------------------------
VoISizeProduct = nan(nRows,1);

for i = 1:nRows
    val = participantTable.VoISize(i);

    if iscell(val)
        val = string(val{1});
    else
        val = string(val);
    end

    if strlength(val) == 0 || ismissing(val)
        continue
    end

    parts = regexp(val, '[xX\*\s]+', 'split');
    nums = str2double(parts);
    nums = nums(~isnan(nums));

    if ~isempty(nums)
        VoISizeProduct(i) = prod(nums);
    end
end

participantTable = addvars(participantTable, VoISizeProduct, ...
    'After', 'VoISize', ...
    'NewVariableNames', 'MRvoxelvolume');

%% ------------------------------------------------------------------------
% Load MAT file
%% ------------------------------------------------------------------------
% in case the DP does not have a separate water scan, then the out_auto and
% outw_auto variables should be used to extract LW, SNR, etc.
matData = load(matFile);

if ~isfield(matData, 'out')
    error('MAT file does not contain variable "out"');
end

out = matData.out;

if isfield(matData, 'out_auto')
    out_auto = matData.out_auto;
else
    out_auto = [];
end

nDPinMat = size(out,2);

%% ------------------------------------------------------------------------
% Extract LW / SNR / SNR_LW_ratio
% First try matData.out
% If empty/missing, fall back to matData.out_auto
%
% Exceptions:
% DP05-sub-02 -> force NaN, fail
% DP16-sub-02 -> force NaN, fail
%
% For these DPs, later subjects are shifted by -1 in the MAT data because
% sub-02 is missing/corrupted there.
%% ------------------------------------------------------------------------
LW = nan(nRows,1);
SNR = nan(nRows,1);
SNR_LW_ratio = nan(nRows,1);

CompCheck = repmat("pass", nRows, 1);

for i = 1:nRows
    token = regexp(dpLabels(i), 'DP\s*0*(\d+)', 'tokens', 'once');
    if isempty(token)
        continue
    end

    dpNumber = str2double(token{1});
    subjNum  = subjectNumberWithinDP(i);

    if isnan(dpNumber) || dpNumber < 1 || dpNumber > nDPinMat
        continue
    end

    % ---- Hard-coded exceptions for corrupted sub-02 ----
    if (dpNumber == 5 && subjNum == 2) || (dpNumber == 16 && subjNum == 2)
        LW(i) = NaN;
        SNR(i) = NaN;
        SNR_LW_ratio(i) = NaN;
        CompCheck(i) = "fail";
        continue
    end

    % ---- Adjust MAT subject index for DPs where sub-02 is missing ----
    matSubjNum = subjNum;
    if (dpNumber == 5 || dpNumber == 16) && subjNum > 2
        matSubjNum = subjNum - 1;
    end

    metricStruct = [];

    % ---- First try out ----
    try
        dpCell = out{1, dpNumber};

        if matSubjNum <= size(dpCell,1)
            subjEntry = dpCell{matSubjNum,1};
            candidateStruct = local_extract_struct(subjEntry);

            if isstruct(candidateStruct)
                metricStruct = candidateStruct;
            end
        end
    catch
    end

    % Read values from out if present
    lw_val = NaN;
    snr_val = NaN;
    ratio_val = NaN;

    if ~isempty(metricStruct)
        if isfield(metricStruct,'LW') && ~isempty(metricStruct.LW)
            lw_val = metricStruct.LW;
        end
        if isfield(metricStruct,'SNR') && ~isempty(metricStruct.SNR)
            snr_val = metricStruct.SNR;
        end
        if isfield(metricStruct,'SNR_LW_ratio') && ~isempty(metricStruct.SNR_LW_ratio)
            ratio_val = metricStruct.SNR_LW_ratio;
        end
    end

    % ---- Use out_auto only for values still missing ----
    needsFallback = isnan(lw_val) || isnan(snr_val) || isnan(ratio_val);

    if needsFallback && ~isempty(out_auto) && dpNumber <= size(out_auto,2)
        try
            dpCell_auto = out_auto{1, dpNumber};

            if matSubjNum <= size(dpCell_auto,1)
                subjEntry_auto = dpCell_auto{matSubjNum,1};
                candidateStruct_auto = local_extract_struct(subjEntry_auto);

                if isstruct(candidateStruct_auto)
                    if isnan(lw_val) && isfield(candidateStruct_auto,'LW') && ~isempty(candidateStruct_auto.LW)
                        lw_val = candidateStruct_auto.LW;
                    end
                    if isnan(snr_val) && isfield(candidateStruct_auto,'SNR') && ~isempty(candidateStruct_auto.SNR)
                        snr_val = candidateStruct_auto.SNR;
                    end
                    if isnan(ratio_val) && isfield(candidateStruct_auto,'SNR_LW_ratio') && ~isempty(candidateStruct_auto.SNR_LW_ratio)
                        ratio_val = candidateStruct_auto.SNR_LW_ratio;
                    end
                end
            end
        catch
        end
    end

    LW(i) = lw_val;
    SNR(i) = snr_val;
    SNR_LW_ratio(i) = ratio_val;
end

%% ------------------------------------------------------------------------
% Add DP + metrics
%% ------------------------------------------------------------------------
participantTable.DP = dpLabels;
participantTable.SubjectInDP = subjectNumberWithinDP;
participantTable.LW = LW;
participantTable.SNR = SNR;
participantTable.SNR_LW_Ratio = SNR_LW_ratio;

participantTable = movevars(participantTable, {'DP','SubjectInDP'}, 'Before', 1);

%% ------------------------------------------------------------------------
% Add CompID
%% ------------------------------------------------------------------------
nRowsFinal = height(participantTable);
compIDs = strcat("compMRS", compose("%03d", (1:nRowsFinal)'));

participantTable = addvars(participantTable, compIDs, ...
    'Before', 1, ...
    'NewVariableNames', 'CompID');

%% ------------------------------------------------------------------------
% Add SiteID (S01, S02, ...)
%% ------------------------------------------------------------------------
siteTable = readtable(siteFile);

siteDP = string(siteTable{:,1});
siteRaw = string(siteTable{:,2});

for i = 1:numel(siteDP)
    token = regexp(siteDP(i), 'DP\s*0*(\d+)', 'tokens', 'once');
    if ~isempty(token)
        siteDP(i) = "DP" + compose("%02d", str2double(token{1}));
    end
end

[uniqueSites, ~, siteIdx] = unique(siteRaw, 'stable');
standardSiteIDs = "S" + compose("%02d", (1:numel(uniqueSites))');
siteStandardLookup = standardSiteIDs(siteIdx);

participantTable.DP = string(participantTable.DP);

SiteID = strings(height(participantTable),1);
[isMatch, loc] = ismember(participantTable.DP, siteDP);
SiteID(isMatch) = siteStandardLookup(loc(isMatch));

participantTable = addvars(participantTable, SiteID, ...
    'After', 'CompID', ...
    'NewVariableNames', 'SiteID');

%% ------------------------------------------------------------------------
% Sort rows
%% ------------------------------------------------------------------------
participantTable = sortrows(participantTable, {'DP','SubjectInDP'});

%% ------------------------------------------------------------------------
% Remove unnecessary columns
%% ------------------------------------------------------------------------
participantTable.PacketID = [];
participantTable.VoISize  = [];

%% ------------------------------------------------------------------------
% Safe cleaning
%% ------------------------------------------------------------------------
for k = 1:width(participantTable)
    if iscell(participantTable{:,k})
        try
            participantTable.(k) = string(participantTable.(k));
        catch
        end
    end
end

%% ------------------------------------------------------------------------
% Add CompCheck
%% ------------------------------------------------------------------------
participantTable = addvars(participantTable, CompCheck, ...
    'NewVariableNames', 'CompCheck');

%% ------------------------------------------------------------------------
% Export CSV
%% ------------------------------------------------------------------------
writetable(participantTable, outCsv, ...
    'Delimiter', ',', ...
    'QuoteStrings', true);

%% ------------------------------------------------------------------------
% Preview
%% ------------------------------------------------------------------------
disp('Done. Output saved to:')
disp(outCsv)

disp(head(participantTable))

%% ------------------------------------------------------------------------
% Local functions
%% ------------------------------------------------------------------------
function col = local_get_selected_column(selectedData, selectedCols, originalColNumber)
idx = find(selectedCols == originalColNumber, 1);

if isempty(idx)
    error('Requested original column %d is not present in selectedCols.', originalColNumber);
end

col = selectedData{:, idx};
end

function col = local_get_fulltable_column(tbl, candidateName)
varNames = string(tbl.Properties.VariableNames);
normVars = local_normalize_names(varNames);
normTarget = local_normalize_names(string(candidateName));

idx = find(normVars == normTarget, 1);

if isempty(idx)
    error('Could not find column "%s" in fullTable.', string(candidateName));
end

col = tbl{:, idx};
end

function out = local_normalize_names(x)
out = lower(string(x));
out = regexprep(out, '[^a-z0-9]', '');
end

function s = local_extract_struct(entry)
s = [];

if isstruct(entry)
    s = entry;
    return
end

if iscell(entry)
    if isempty(entry)
        return
    end

    firstVal = entry{1};

    if isstruct(firstVal)
        s = firstVal;
        return
    end

    if iscell(firstVal) && ~isempty(firstVal)
        innerVal = firstVal{1};
        if isstruct(innerVal)
            s = innerVal;
            return
        end
    end
end
end