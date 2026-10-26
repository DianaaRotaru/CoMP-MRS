% compMRS_DPprocStats_withPulseShapeParameters.m
% Diana Rotaru, Columbia University & Medical University of Vienna, 2025
% Mark Mikkelsen, Weill Cornell Medicine, 2025

clear; clc;

outputDir = 'A:\CoMP-MRS\CoMP-MRS-DGR\stats\ProcSpectralQuality-ISMRM2027\data\input_table\v4_20261025';
if ~isfolder(outputDir), error('Output/input folder not found: %s', outputDir); end
cd(outputDir);
methodDir = fullfile(outputDir, 'method-files');

%% File names
xlsxFile       = 'CoMP-MRS_participantSpreadsheet.xlsx';
matFile        = 'CoMP-MRS-final.mat';
siteFile       = 'CoMP-MRS-sites.xlsx';
outCsv         = fullfile(outputDir, 'CoMP_MRS_Rstats_input.csv');
outSummaryXlsx = fullfile(outputDir, 'CoMP_MRS_summary_table.xlsx');

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
% 30 = MRI.console
% 31 = MRI.software.version
% 32 = MRI.coil.setup
% 35 = MRS.pulse.sequence
% 36 = MRS.brain.region
% 37 = MRS.VoI.size
% 38 = MRS.sw
% 39 = MRS.n.pts
% 40 = MRS.TE
% 41 = MRS.TR
% 44 = MRS.n.ave
% 55 = MRS.excite.pulse.type
% 56 = MRS.excite.pulse.duration
% 57 = MRS.refoc.pulse.type
% 58 = MRS.refoc.pulse.duration
% 62 = MRS.shim.method
selectedCols = [1 3 6 7 9 11 12 28 29 30 31 32 35 36 37 38 39 40 41 44 55 56 57 58 62];
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
% Subject numbering within DP (internal only)
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
participantTable.MRconsole     = local_get_selected_column(selectedData, selectedCols, 30);
participantTable.MRcoil        = local_get_selected_column(selectedData, selectedCols, 32);
participantTable.MRsequence    = local_get_selected_column(selectedData, selectedCols, 35);
participantTable.MRbrainregion = local_get_selected_column(selectedData, selectedCols, 36);
participantTable.MRVoxSize       = local_get_selected_column(selectedData, selectedCols, 37);
participantTable.MRaverages    = local_get_selected_column(selectedData, selectedCols, 44);

%% ------------------------------------------------------------------------
% Add extra fields from fullTable by header name
%% ------------------------------------------------------------------------
participantTable.MRsoftwareversion = local_get_fulltable_column(fullTable, "MRI.software.version");
participantTable.MRSsw             = local_get_fulltable_column(fullTable, "MRS.sw");
participantTable.MRSnpts           = local_get_fulltable_column(fullTable, "MRS.n.pts");
participantTable.MRSTE             = local_get_fulltable_column(fullTable, "MRS.TE");
participantTable.MRSTR             = local_get_fulltable_column(fullTable, "MRS.TR");
participantTable.MRSshim           = local_get_fulltable_column(fullTable, "MRS.shim.method");
participantTable.MRSexcitePulseType     = local_get_selected_column(selectedData, selectedCols, 55);
participantTable.MRSexcitePulseDuration = local_get_selected_column(selectedData, selectedCols, 56);
participantTable.MRSrefocPulseType      = local_get_selected_column(selectedData, selectedCols, 57);
participantTable.MRSrefocPulseDuration  = local_get_selected_column(selectedData, selectedCols, 58);

%% ------------------------------------------------------------------------
% Standardize / normalize field types and values
%% ------------------------------------------------------------------------
participantTable.AnimalSpecies     = string(participantTable.AnimalSpecies);
participantTable.AnimalStrain      = string(participantTable.AnimalStrain);
participantTable.AnimalSex         = string(participantTable.AnimalSex);
participantTable.MRvendor          = string(participantTable.MRvendor);
participantTable.MRconsole         = string(participantTable.MRconsole);
participantTable.MRSexcitePulseType     = string(participantTable.MRSexcitePulseType);
participantTable.MRSrefocPulseType      = string(participantTable.MRSrefocPulseType);
participantTable.MRSexcitePulseDuration = string(participantTable.MRSexcitePulseDuration);
participantTable.MRSrefocPulseDuration  = string(participantTable.MRSrefocPulseDuration);
participantTable.MRcoil            = string(participantTable.MRcoil);
participantTable.MRsequence        = string(participantTable.MRsequence);
participantTable.MRbrainregion     = string(participantTable.MRbrainregion);
participantTable.MRVoxSize           = string(participantTable.MRVoxSize);
participantTable.MRsoftwareversion = string(participantTable.MRsoftwareversion);
participantTable.MRSshim           = string(participantTable.MRSshim);

%% ------------------------------------------------------------------------
% Read DP acquisition headers; retain original spreadsheet values for audit
% Bruker method files and Varian procpar files are handled independently.
% Missing method files: all Method_* fields remain "NA".
%% ------------------------------------------------------------------------
methodFields = {'MRSsw','MRSnpts','MRSTE','MRSTR', ...
    'MRSexcitePulseType','MRSexcitePulseDuration', ...
    'MRSrefocPulseType','MRSrefocPulseDuration', ...
    'MRSexciteFlipAngle','MRSrefocFlipAngle', ...
    'MRSexciteSharpness','MRSrefocSharpness', ...
    'MRSexciteBwfac','MRSrefocBwfac'};
methodNames = "Method_" + string(methodFields);
for j = 1:numel(methodFields)
    participantTable.(char(methodNames(j))) = repmat("NA", height(participantTable), 1);
end
participantTable.MethodFileStatus = repmat("NA",height(participantTable),1);
participantTable.MethodFileName = repmat("NA",height(participantTable),1);

% Keep an auditable per-DP comparison before overwriting canonical fields.
methodAudit = table();
for dpNumber = 1:36
    idx = find(dpNumAll(validIdx) == dpNumber);
    if isempty(idx), continue; end
    dpName = sprintf('DP%02d',dpNumber);
    methodPath = fullfile(methodDir, [dpName '-method']);
    procparPath = fullfile(methodDir, [dpName '-procpar']);
    status = "missing file";
    values = repmat("NA",1,numel(methodFields));
    filename = "NA";
    if isfile(methodPath)
        filename = string([dpName '-method']);
        try
            values = local_read_bruker_metadata(methodPath);
            % ParaVision 5: leave Sharpness and Bwfac unavailable.
            pvVersion = string(participantTable.MRsoftwareversion(idx));
            if any(~cellfun('isempty', regexp(cellstr(pvVersion), ...
                    '(?i)(?:(?:PV|ParaVision)\s*)?5(?:\.|\b)', 'once')))
                values(11:14) = "NA";
            end
            status = "parsed Bruker";
        catch ME
            status = "parse error: " + string(ME.message);
        end
    elseif isfile(procparPath)
        filename = string([dpName '-procpar']);
        try
            values = local_read_varian_metadata(procparPath);
            status = "parsed Varian";
        catch ME
            status = "parse error: " + string(ME.message);
        end
    end
    participantTable.MethodFileStatus(idx) = status;
    participantTable.MethodFileName(idx) = filename;
    for j = 1:numel(methodFields)
        participantTable.(char(methodNames(j)))(idx) = values(j);
        if ismember(methodFields{j}, {'MRSexciteFlipAngle','MRSrefocFlipAngle', ...
                'MRSexciteSharpness','MRSrefocSharpness', ...
                'MRSexciteBwfac','MRSrefocBwfac'})
            original = "NA"; % no spreadsheet column for these new fields
        else
            original = string(participantTable.(methodFields{j})(idx));
        end
        original = original(~ismissing(original) & strlength(strtrim(original))>0);
        if isempty(original), original = "NA"; else, original = original(1); end
        methodAudit = [methodAudit; table(string(dpName), string(methodFields{j}), ...
            original, values(j), status, ...
            'VariableNames', {'DP','Field','Spreadsheet','MethodFile','Status'})]; %#ok<AGROW>
    end
end
methodAudit.Discrepancy = false(height(methodAudit),1);
for q = 1:height(methodAudit)
    a = strtrim(methodAudit.Spreadsheet(q));
    b = strtrim(methodAudit.MethodFile(q));
    if ~ismissing(a) && ~ismissing(b) && a~="NA" && b~="NA"
        na = str2double(a); nb = str2double(b);
        if ~isnan(na) && ~isnan(nb)
            methodAudit.Discrepancy(q) = abs(na-nb)>max(1e-6,1e-3*max(abs([na,nb])));
        else
            methodAudit.Discrepancy(q) = ~strcmpi(a,b);
        end
    end
end

%% ------------------------------------------------------------------------
% Acquisition files are authoritative. For VARIAN datasets ONLY, fill
% missing procpar-extracted fields from the participant spreadsheet.
% Preserve procpar values when available; never overwrite with spreadsheet.
% Bruker missing fields, PV5 Sharpness/Bwfac, and unavailable flip angles
% remain NA. Method_* columns retain raw acquisition-file extraction.
% Spreadsheet fallback applies only to columns actually in the spreadsheet.
%% ------------------------------------------------------------------------
% Preserve original spreadsheet columns before replacing canonical values.
spreadsheetAcq = cell(1,numel(methodFields));
for j = 1:numel(methodFields)
    if ismember(methodFields{j}, {'MRSexciteFlipAngle','MRSrefocFlipAngle', ...
            'MRSexciteSharpness','MRSrefocSharpness', ...
            'MRSexciteBwfac','MRSrefocBwfac'})
        spreadsheetAcq{j} = repmat("NA",height(participantTable),1);
    else
        spreadsheetAcq{j} = string(participantTable.(methodFields{j}));
    end
end

isVarianDP = contains(lower(string(participantTable.MRvendor)),"varian") | ...
    contains(lower(string(participantTable.MRvendor)),"agilent") | ...
    endsWith(lower(string(participantTable.MethodFileName)),"-procpar");

for j = 1:numel(methodFields)
    canonical = methodFields{j};
    extracted = char(methodNames(j));
    fromFile = string(participantTable.(extracted));
    fromSheet = spreadsheetAcq{j};
    missingFileValue = ismissing(fromFile) | strlength(strtrim(fromFile))==0 | ...
        strcmpi(strtrim(fromFile),"NA") | strcmpi(strtrim(fromFile),"NaN") | ...
        strcmpi(strtrim(fromFile),"<missing>");
    validSheetValue = ~ismissing(fromSheet) & strlength(strtrim(fromSheet))>0 & ...
        ~strcmpi(strtrim(fromSheet),"NA") & ~strcmpi(strtrim(fromSheet),"NaN") & ...
        ~strcmpi(strtrim(fromSheet),"<missing>");
    useSpreadsheet = isVarianDP & missingFileValue & validSheetValue;
    fromFile(useSpreadsheet) = fromSheet(useSpreadsheet);
    participantTable.(canonical) = fromFile;
end
% Method_* columns continue to show raw method/procpar values, even when
% the canonical column is populated from the participant spreadsheet.

%% ------------------------------------------------------------------------
% Add Cryoprobe column (yes/no based on MRcoil)
%% ------------------------------------------------------------------------
coilStr = lower(string(participantTable.MRcoil));

isCryo = contains(coilStr, "cryo") | contains(coilStr, "cryoprobe");

Cryoprobe = repmat("FALSE", height(participantTable), 1);
Cryoprobe(isCryo) = "TRUE";

participantTable.Cryoprobe = Cryoprobe;

participantTable = movevars(participantTable, 'Cryoprobe', 'After', 'MRcoil');

% MRbrainregion
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, "Right hippocampus", "Rhippocampus");
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, "Left hippocampus", "Lhippocampus");
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, "Right striatum", "Rstriatum");
participantTable.MRbrainregion = strrep(participantTable.MRbrainregion, "Left striatum", "Lstriatum");

% MRvendor
participantTable.MRvendor = strrep(participantTable.MRvendor, "Agilent/Varian", "Varian");
participantTable.MRvendor = strrep(participantTable.MRvendor, "Varian/Agilent", "Varian");

% AnimalStrain
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, "C57BL/6J", "C57BL-6J");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, "C57BL/6", "C57BL-6");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, "balb/c", "BALB-c");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, "BALB/c", "BALB-c");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, "B6129SF2/J", "B6129SF2-J");
participantTable.AnimalStrain = strrep(participantTable.AnimalStrain, "FVB/N", "FVB-N");

% MRSshim
participantTable.MRSshim = strrep(participantTable.MRSshim, "FASTMAP/FASTESTMAP", "FASTMAP-FASTESTMAP");

%% ------------------------------------------------------------------------
% Standardize MRfield
%% ------------------------------------------------------------------------
participantTable.MRfield = str2double(string(participantTable.MRfield));
idxField = participantTable.MRfield >= 9 & participantTable.MRfield < 9.4;
participantTable.MRfield(idxField) = 9.4;

%% ------------------------------------------------------------------------
% Compute MRvoxelvolume
%% ------------------------------------------------------------------------
VoISizeProduct = nan(nRows,1);

for i = 1:nRows
    val = participantTable.MRVoxSize(i);

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
    'After', 'MRVoxSize', ...
    'NewVariableNames', 'MRvoxelvolume');


%% ------------------------------------------------------------------------
% Load MAT file
%% ------------------------------------------------------------------------
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
%% ------------------------------------------------------------------------
LW = nan(nRows,1);
LW_hz = nan(nRows,1);
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

    % Exceptions
    if (dpNumber == 16 && subjNum == 2) || (dpNumber == 17 && subjNum == 1)
        CompCheck(i) = "fail";
        continue
    end

    matSubjNum = subjNum;
    if (dpNumber == 16) && subjNum > 2
        matSubjNum = subjNum - 1;
    end

    metricStruct = [];

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

    lw_val = NaN;
    lwhz_val = NaN;
    snr_val = NaN;
    ratio_val = NaN;

    if ~isempty(metricStruct)
        if isfield(metricStruct,'LW') && ~isempty(metricStruct.LW), lw_val = metricStruct.LW; end
        if isfield(metricStruct,'LW_hz') && ~isempty(metricStruct.LW_hz), lwhz_val = metricStruct.LW_hz; end
        if isfield(metricStruct,'SNR') && ~isempty(metricStruct.SNR), snr_val = metricStruct.SNR; end
        if isfield(metricStruct,'SNR_LW_ratio') && ~isempty(metricStruct.SNR_LW_ratio), ratio_val = metricStruct.SNR_LW_ratio; end
    end

    needsFallback = isnan(lw_val) || isnan(lwhz_val) || isnan(snr_val) || isnan(ratio_val);

    if needsFallback && ~isempty(out_auto) && dpNumber <= size(out_auto,2)
        try
            dpCell_auto = out_auto{1, dpNumber};

            if matSubjNum <= size(dpCell_auto,1)
                subjEntry_auto = dpCell_auto{matSubjNum,1};
                candidateStruct_auto = local_extract_struct(subjEntry_auto);

                if isstruct(candidateStruct_auto)
                    if isnan(lw_val) && isfield(candidateStruct_auto,'LW') && ~isempty(candidateStruct_auto.LW), lw_val = candidateStruct_auto.LW; end
                    if isnan(lwhz_val) && isfield(candidateStruct_auto,'LW_hz') && ~isempty(candidateStruct_auto.LW_hz), lwhz_val = candidateStruct_auto.LW_hz; end
                    if isnan(snr_val) && isfield(candidateStruct_auto,'SNR') && ~isempty(candidateStruct_auto.SNR), snr_val = candidateStruct_auto.SNR; end
                    if isnan(ratio_val) && isfield(candidateStruct_auto,'SNR_LW_ratio') && ~isempty(candidateStruct_auto.SNR_LW_ratio), ratio_val = candidateStruct_auto.SNR_LW_ratio; end
                end
            end
        catch
        end
    end

    LW(i) = lw_val;
    LW_hz(i) = lwhz_val;
    SNR(i) = snr_val;
    SNR_LW_ratio(i) = ratio_val;
end

%% ------------------------------------------------------------------------
% Add DP + metrics
%% ------------------------------------------------------------------------
participantTable.DP = dpLabels;
participantTable.SubjectInDP = subjectNumberWithinDP; % internal only
participantTable.LW = LW;
participantTable.LW_hz = LW_hz;
participantTable.SNR = SNR;
participantTable.SNR_LW_Ratio = SNR_LW_ratio;

participantTable = movevars(participantTable, {'DP','SubjectInDP'}, 'Before', 1);

%% ------------------------------------------------------------------------
% Add CompID
%% ------------------------------------------------------------------------
participantTable.CompID = strcat("compMRS", compose("%03d", (1:nRows)'));

%% ------------------------------------------------------------------------
% Add SiteID (robust DP ↔ site mapping)
%% ------------------------------------------------------------------------
siteTable = readtable(siteFile);

% Extract DP numbers from BOTH tables (robust to formatting)
siteDP_raw = string(siteTable{:,1});
participantDP_raw = string(participantTable.DP);

siteDP_num = nan(height(siteTable),1);
for i = 1:height(siteTable)
    token = regexp(siteDP_raw(i), 'DP\s*0*(\d+)', 'tokens', 'once');
    if ~isempty(token)
        siteDP_num(i) = str2double(token{1});
    end
end

participantDP_num = nan(height(participantTable),1);
for i = 1:height(participantTable)
    token = regexp(participantDP_raw(i), 'DP\s*0*(\d+)', 'tokens', 'once');
    if ~isempty(token)
        participantDP_num(i) = str2double(token{1});
    end
end

% Get site names
siteNames = string(siteTable{:,2});

% Convert site names → standardized SiteID (S01, S02, …)
[uniqueSites, ~, siteIdx] = unique(siteNames, 'stable');
standardSiteIDs = "S" + compose("%02d", (1:numel(uniqueSites))');

siteLookup = standardSiteIDs(siteIdx);

% Assign SiteID by DP number (SAFE MATCHING)
SiteID = strings(height(participantTable),1);

for i = 1:height(participantTable)
    dpNum = participantDP_num(i);
    matchIdx = find(siteDP_num == dpNum, 1);

    if ~isempty(matchIdx)
        SiteID(i) = siteLookup(matchIdx);
    else
        SiteID(i) = ""; % leave empty if no match
    end
end

participantTable.SiteID = SiteID;

%% ------------------------------------------------------------------------
% Sort rows
%% ------------------------------------------------------------------------
participantTable = sortrows(participantTable, {'DP','SubjectInDP'});

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
% Create summary table
% PI, location, n_fail, comments removed
%% ------------------------------------------------------------------------
uniqueDPs = unique(string(participantTable.DP), 'stable');
nDP = numel(uniqueDPs);

summaryTable = table();
summaryTable.dataset = uniqueDPs;
summaryTable.site = strings(nDP,1);
summaryTable.species = strings(nDP,1);
summaryTable.animal_strain = strings(nDP,1);
summaryTable.age_mean = nan(nDP,1);
summaryTable.age_SD = nan(nDP,1);
summaryTable.sex = strings(nDP,1);
summaryTable.vendor = strings(nDP,1);
summaryTable.field_strength = nan(nDP,1);
summaryTable.software_version = strings(nDP,1);
summaryTable.console = strings(nDP,1);
summaryTable.excite_pulse_type = strings(nDP,1);
summaryTable.excite_pulse_duration = strings(nDP,1);
summaryTable.refoc_pulse_type = strings(nDP,1);
summaryTable.refoc_pulse_duration = strings(nDP,1);
summaryTable.excite_flip_angle = strings(nDP,1);
summaryTable.refoc_flip_angle = strings(nDP,1);
summaryTable.excite_sharpness = strings(nDP,1);
summaryTable.refoc_sharpness = strings(nDP,1);
summaryTable.excite_bwfac = strings(nDP,1);
summaryTable.refoc_bwfac = strings(nDP,1);
summaryTable.coil_setup = strings(nDP,1);
summaryTable.sequence = strings(nDP,1);
summaryTable.sw = nan(nDP,1);
summaryTable.n_pts = nan(nDP,1);
summaryTable.TE = nan(nDP,1);
summaryTable.TR = nan(nDP,1);
summaryTable.shim_method = strings(nDP,1);
summaryTable.number_of_averages = nan(nDP,1);
summaryTable.region = strings(nDP,1);
summaryTable.voxel_size = strings(nDP,1);
summaryTable.voxel_volume = nan(nDP,1);
summaryTable.LW_mean = nan(nDP,1);
summaryTable.LW_hz_mean = nan(nDP,1);
summaryTable.SNR_mean = nan(nDP,1);
summaryTable.SNR_LW_ratio_mean = nan(nDP,1);

for j = 1:numel(methodFields)
    summaryTable.(char(methodNames(j))) = repmat("NA",nDP,1);
end
summaryTable.MethodFileStatus = repmat("NA",nDP,1);
summaryTable.MethodFileName = repmat("NA",nDP,1);

for d = 1:nDP
    dp = uniqueDPs(d);
    subT = participantTable(string(participantTable.DP) == dp,:);

    for j = 1:numel(methodFields)
        summaryTable.(char(methodNames(j)))(d) = local_first(subT.(char(methodNames(j))));
    end
    summaryTable.excite_flip_angle(d) = local_first(subT.MRSexciteFlipAngle);
    summaryTable.refoc_flip_angle(d) = local_first(subT.MRSrefocFlipAngle);
    summaryTable.excite_sharpness(d) = local_first(subT.MRSexciteSharpness);
    summaryTable.refoc_sharpness(d) = local_first(subT.MRSrefocSharpness);
    summaryTable.excite_bwfac(d) = local_first(subT.MRSexciteBwfac);
    summaryTable.refoc_bwfac(d) = local_first(subT.MRSrefocBwfac);
    summaryTable.MethodFileStatus(d) = local_first(subT.MethodFileStatus);
    summaryTable.MethodFileName(d) = local_first(subT.MethodFileName);
    summaryTable.site(d) = local_first(subT.SiteID);
    summaryTable.species(d) = local_first(subT.AnimalSpecies);
    summaryTable.animal_strain(d) = local_first(subT.AnimalStrain);
    summaryTable.vendor(d) = local_first(subT.MRvendor);
    summaryTable.software_version(d) = local_first(subT.MRsoftwareversion);
    summaryTable.console(d) = local_first(subT.MRconsole);
    summaryTable.excite_pulse_type(d) = local_first(subT.MRSexcitePulseType);
    summaryTable.excite_pulse_duration(d) = local_first(subT.MRSexcitePulseDuration);
    summaryTable.refoc_pulse_type(d) = local_first(subT.MRSrefocPulseType);
    summaryTable.refoc_pulse_duration(d) = local_first(subT.MRSrefocPulseDuration);
    summaryTable.coil_setup(d) = local_first(subT.MRcoil);
    summaryTable.sequence(d) = local_first(subT.MRsequence);
    summaryTable.shim_method(d) = local_first(subT.MRSshim);
    summaryTable.region(d) = local_first(subT.MRbrainregion);
    summaryTable.voxel_size(d) = local_first(subT.MRVoxSize);

    ageVals = str2double(string(subT.AnimalAge));
    ageVals = ageVals(~isnan(ageVals));
    if ~isempty(ageVals)
        summaryTable.age_mean(d) = mean(ageVals);
        if numel(ageVals) > 1
            summaryTable.age_SD(d) = std(ageVals,0);
        else
            summaryTable.age_SD(d) = 0;
        end
    end

    sexVals = upper(strtrim(string(subT.AnimalSex)));
    nF = sum(sexVals == "F");
    nM = sum(sexVals == "M");
    if nF > 0 && nM > 0
        summaryTable.sex(d) = sprintf('%dF, %dM', nF, nM);
    elseif nF > 0
        summaryTable.sex(d) = sprintf('%dF', nF);
    elseif nM > 0
        summaryTable.sex(d) = sprintf('%dM', nM);
    else
        summaryTable.sex(d) = "";
    end

    fieldVals = subT.MRfield;
    fieldVals = fieldVals(~isnan(fieldVals));
    if ~isempty(fieldVals), summaryTable.field_strength(d) = fieldVals(1); end

    vvVals = subT.MRvoxelvolume;
    vvVals = vvVals(~isnan(vvVals));
    if ~isempty(vvVals), summaryTable.voxel_volume(d) = vvVals(1); end

    lwVals = subT.LW;
    lwVals = lwVals(~isnan(lwVals));
    if ~isempty(lwVals), summaryTable.LW_mean(d) = mean(lwVals); end

    lwhzVals = subT.LW_hz;
    lwhzVals = lwhzVals(~isnan(lwhzVals));
    if ~isempty(lwhzVals), summaryTable.LW_hz_mean(d) = mean(lwhzVals); end

    snrVals = subT.SNR;
    snrVals = snrVals(~isnan(snrVals));
    if ~isempty(snrVals), summaryTable.SNR_mean(d) = mean(snrVals); end

    ratioVals = subT.SNR_LW_Ratio;
    ratioVals = ratioVals(~isnan(ratioVals));
    if ~isempty(ratioVals), summaryTable.SNR_LW_ratio_mean(d) = mean(ratioVals); end

    swVals = str2double(string(subT.MRSsw));
    swVals = swVals(~isnan(swVals));
    if ~isempty(swVals), summaryTable.sw(d) = swVals(1); end

    nptsVals = str2double(string(subT.MRSnpts));
    nptsVals = nptsVals(~isnan(nptsVals));
    if ~isempty(nptsVals), summaryTable.n_pts(d) = nptsVals(1); end

    teVals = str2double(string(subT.MRSTE));
    teVals = teVals(~isnan(teVals));
    if ~isempty(teVals), summaryTable.TE(d) = teVals(1); end

    trVals = str2double(string(subT.MRSTR));
    trVals = trVals(~isnan(trVals));
    if ~isempty(trVals), summaryTable.TR(d) = trVals(1); end

    navVals = str2double(string(subT.MRaverages));
    navVals = navVals(~isnan(navVals));
    if ~isempty(navVals), summaryTable.number_of_averages(d) = navVals(1); end
end

%% ------------------------------------------------------------------------
% Remove columns from final subject-level output
%% ------------------------------------------------------------------------
participantTable.PacketID = [];
participantTable.SubjectInDP = [];

%% ------------------------------------------------------------------------
% Put CompID and SiteID first
%% ------------------------------------------------------------------------
participantTable = movevars(participantTable, {'CompID','SiteID'}, 'Before', 1);

%% ------------------------------------------------------------------------
% Format summary table numeric columns to 2 decimals
%% ------------------------------------------------------------------------
for k = 1:width(summaryTable)
    if isnumeric(summaryTable{:,k})
        summaryTable{:,k} = round(summaryTable{:,k}, 2);
    end
end

%% ------------------------------------------------------------------------
% Reorder FULL summary table columns (core first, rest after)
%% ------------------------------------------------------------------------

coreCols = { ...
    'dataset','site','species','strain','sex','vendor','B0',...
    'console','software_version','sequence','excite_pulse_type',...
    'excite_pulse_duration','refoc_pulse_type','refoc_pulse_duration',...
    'coil','shim','averages',...
    'region','voxel_size','voxel_volume','sw','TE','TR','n_points' ...
};

% Keep only those that actually exist (safety)
coreCols = coreCols(ismember(coreCols, summaryTable.Properties.VariableNames));

% Get remaining columns automatically
otherCols = setdiff(summaryTable.Properties.VariableNames, coreCols, 'stable');

% Final order: core first, then everything else
summaryTable = summaryTable(:, [coreCols, otherCols]);

%% ------------------------------------------------------------------------
% Create SHORT summary table (selected columns only)
%% ------------------------------------------------------------------------
%% This section of the code needs updates to match the formats and entries#
%% in the table in figure 4 (boxplots)
summaryShort = table();

summaryShort.dataset           = summaryTable.dataset;
summaryShort.site              = summaryTable.site;
summaryShort.species           = summaryTable.species;
summaryShort.animal_strain     = summaryTable.animal_strain;
summaryShort.sex               = summaryTable.sex;
summaryShort.vendor            = summaryTable.vendor;

% Rename field_strength → B0
summaryShort.B0                = summaryTable.field_strength;

summaryShort.software_version  = summaryTable.software_version;
summaryShort.console           = summaryTable.console;
summaryShort.excite_pulse_type = summaryTable.excite_pulse_type;
summaryShort.excite_pulse_duration = summaryTable.excite_pulse_duration;
summaryShort.refoc_pulse_type = summaryTable.refoc_pulse_type;
summaryShort.refoc_pulse_duration = summaryTable.refoc_pulse_duration;
summaryShort.excite_flip_angle = summaryTable.excite_flip_angle;
summaryShort.refoc_flip_angle = summaryTable.refoc_flip_angle;
summaryShort.excite_sharpness = summaryTable.excite_sharpness;
summaryShort.refoc_sharpness = summaryTable.refoc_sharpness;
summaryShort.excite_bwfac = summaryTable.excite_bwfac;
summaryShort.refoc_bwfac = summaryTable.refoc_bwfac;
summaryShort.coil_setup        = summaryTable.coil_setup;
summaryShort.sequence          = summaryTable.sequence;
summaryShort.sw                = summaryTable.sw;
summaryShort.n_pts             = summaryTable.n_pts;
summaryShort.TE                = summaryTable.TE;
summaryShort.TR                = summaryTable.TR;
summaryShort.shim_method       = summaryTable.shim_method;

% Rename number_of_averages → averages
summaryShort.averages          = summaryTable.number_of_averages;

summaryShort.region            = summaryTable.region;
summaryShort.voxel_size        = summaryTable.voxel_size;

% Add all method-derived metadata to the short DP summary as well.
for j = 1:numel(methodFields)
    summaryShort.(char(methodNames(j))) = summaryTable.(char(methodNames(j)));
end
summaryShort.MethodFileStatus = summaryTable.MethodFileStatus;
summaryShort.MethodFileName = summaryTable.MethodFileName;

%% ------------------------------------------------------------------------
% Export
%% ------------------------------------------------------------------------
% Ensure all newly requested fields are present in the subject-level CSV.
requiredCsvCols = {'MRconsole','MRsoftwareversion','MRSsw','MRSnpts', ...
    'MRSTE','MRSTR','MRSexcitePulseType','MRSexcitePulseDuration', ...
    'MRSrefocPulseType','MRSrefocPulseDuration', ...
    'MRSexciteFlipAngle','MRSrefocFlipAngle', ...
    'MRSexciteSharpness','MRSrefocSharpness', ...
    'MRSexciteBwfac','MRSrefocBwfac'};
missingCsvCols = setdiff(requiredCsvCols, participantTable.Properties.VariableNames);
if ~isempty(missingCsvCols)
    error('Missing columns in CSV output: %s', strjoin(missingCsvCols, ', '));
end

writetable(participantTable, outCsv);
% Matching Excel export for inspection and downstream R workflows.
exportDir = outputDir;
writetable(participantTable, fullfile(exportDir, 'CoMP_MRS_Rstats_input.xlsx'), 'Sheet', 'Rstats_input');
writetable(summaryTable, outSummaryXlsx, 'Sheet','Summary');

writetable(summaryShort, outSummaryXlsx, 'Sheet','Summary_short');
writetable(methodAudit, 'CoMP_MRS_method_discrepancy_report.xlsx', 'Sheet','Comparison');

%% ------------------------------------------------------------------------
% Dedicated one-row-per-DP acquisition table, ready for the manuscript/PPT
% Canonical values prefer method/procpar; Varian-only missing fields
% may use participant spreadsheet values. DP33 Bruker remains NA.
%% ------------------------------------------------------------------------
acquisitionTable = table();
acquisitionTable.DP = string(summaryTable.dataset);
acquisitionTable.Species = string(summaryTable.species);
acquisitionTable.Vendor = string(summaryTable.vendor);
acquisitionTable.MRI_console = string(summaryTable.console);             % col 30
acquisitionTable.Software_version = string(summaryTable.software_version); % col 31
acquisitionTable.Field_strength_T = string(summaryTable.field_strength);
acquisitionTable.Pulse_sequence = string(summaryTable.sequence);
acquisitionTable.TE_ms = string(summaryTable.TE);                          % col 40
acquisitionTable.TR_ms = string(summaryTable.TR);                          % col 41
acquisitionTable.Excitation_RF_pulse = string(summaryTable.excite_pulse_type); % col 55
acquisitionTable.Excitation_duration = string(summaryTable.excite_pulse_duration); % col 56
acquisitionTable.Refocusing_RF_pulse = string(summaryTable.refoc_pulse_type); % col 57
acquisitionTable.Refocusing_duration = string(summaryTable.refoc_pulse_duration); % col 58
acquisitionTable.Excitation_flip_angle_deg = string(summaryTable.excite_flip_angle);
acquisitionTable.Refocusing_flip_angle_deg = string(summaryTable.refoc_flip_angle);
acquisitionTable.Excitation_sharpness = string(summaryTable.excite_sharpness);
acquisitionTable.Refocusing_sharpness = string(summaryTable.refoc_sharpness);
acquisitionTable.Excitation_bwfac_Hz_ms = string(summaryTable.excite_bwfac);
acquisitionTable.Refocusing_bwfac_Hz_ms = string(summaryTable.refoc_bwfac);
acquisitionTable.Spectral_width_Hz = string(summaryTable.sw);             % col 38
acquisitionTable.Spectral_points = string(summaryTable.n_pts);           % col 39
acquisitionTable.Method_file_status = string(summaryTable.MethodFileStatus);
acquisitionTable.Method_file = string(summaryTable.MethodFileName);
% Convert missing or NaN values to explicit NA in the presentation table.
for kk = 1:width(acquisitionTable)
    c = string(acquisitionTable{:,kk});
    c(ismissing(c) | strlength(strtrim(c)) == 0 | strcmpi(c,'NaN') | strcmpi(c,'<missing>')) = "NA";
    acquisitionTable.(acquisitionTable.Properties.VariableNames{kk}) = c;
end
% Round numeric acquisition measurements to at most two decimal places.
% Keep identifiers, software versions, pulse names, missing values and
% integer spectral-point counts unchanged. This affects the DP table only;
% the subject-level Rstats data retain their original precision.
roundColumns = {'Field_strength_T','TE_ms','TR_ms', ...
    'Excitation_duration','Refocusing_duration', ...
    'Excitation_flip_angle_deg','Refocusing_flip_angle_deg', ...
    'Excitation_sharpness','Refocusing_sharpness', ...
    'Excitation_bwfac_Hz_ms','Refocusing_bwfac_Hz_ms', ...
    'Spectral_width_Hz'};
for jj = 1:numel(roundColumns)
    colName = roundColumns{jj};
    values = string(acquisitionTable.(colName));
    numericValues = str2double(values);
    valid = isfinite(numericValues);
    rounded = round(numericValues(valid), 2);
    values(valid) = string(rounded);  % at most 2 decimal places
    acquisitionTable.(colName) = values;
end

acquisitionXlsx = fullfile(exportDir,'CoMP_MRS_acquisition_parameters_by_DP.xlsx');
writetable(acquisitionTable, acquisitionXlsx, 'Sheet','Acquisition_by_DP');
% Also embed the identical table in the main Rstats workbook.
writetable(acquisitionTable, fullfile(exportDir,'CoMP_MRS_Rstats_input.xlsx'), 'Sheet','Acquisition_by_DP');
% PowerPoint preparation: export the exact MATLAB-validated table to CSV.
writetable(acquisitionTable, fullfile(exportDir,'CoMP_MRS_acquisition_parameters_by_DP.csv'));
fprintf('DP acquisition table: %s (%d DPs)\n',acquisitionXlsx,height(acquisitionTable));


fprintf('Updated subject-level CSV: %s (%d rows, %d columns)\n', outCsv, height(participantTable), width(participantTable));
disp('Done.')

%% ------------------------------------------------------------------------
% Helpers
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

col = tbl{:,idx};
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

function val = local_first(x)
x = string(x);
x = x(x ~= "" & ~ismissing(x));
if isempty(x)
    val = "";
else
    val = x(1);
end
end
%% ------------------------------------------------------------------------
% Metadata-only parsers (no spectral FID loading)
%% ------------------------------------------------------------------------
function v = local_read_bruker_metadata(f)
% Use the supplied Bruker parser for ordinary scalar fields; inspect raw
% method text for pulse structs (which may contain nested arrays/angles).
h = compMRS_parseBrukerFormat(f);
v = repmat("NA",1,14);
v(1) = local_field_as_text(h, {'PVM_DigSw','PVM_SpecSWH'});
v(2) = local_field_as_text(h, {'PVM_DigNp','PVM_SpecMatrix'});
v(3) = local_field_as_text(h, {'PVM_EchoTime'});
v(4) = local_field_as_text(h, {'PVM_RepetitionTime'});
t = fileread(f);
% Bruker nomenclature varies by ParaVision generation.
for k = 1:3
    pulseName = local_bruker_scalar(t, {sprintf('VoxPul%dEnum',k),sprintf('VoxPulse%dEnum',k)});
    pulseDuration = local_bruker_duration(t, {sprintf('VoxPul%d',k),sprintf('VoxPulse%d',k)});
    if k == 1
        v(5) = pulseName; v(6) = pulseDuration;
    else
        if pulseName ~= "NA"
            if v(7) == "NA", v(7) = pulseName;
            elseif ~contains(";"+v(7)+";", ";"+pulseName+";")
                v(7) = v(7) + "; " + pulseName;
            end
        end
        if pulseDuration ~= "NA"
            if v(8) == "NA", v(8) = pulseDuration;
            elseif ~contains(";"+v(8)+";", ";"+pulseDuration+";")
                v(8) = v(8) + "; " + pulseDuration;
            end
        end
    end
    % Nominal flip angle is the third entry in VoxPulN/VoxPulseN.
    % Use the angle itself to distinguish 90-degree and 180-degree pulses;
    % the first pulse is not always excitation (e.g., sSPECIAL).
    angle = local_bruker_flip(t, {sprintf('VoxPul%d',k),sprintf('VoxPulse%d',k)});
    if angle ~= "NA"
        a = str2double(angle);
        if isfinite(a)
            if abs(a-90) <= abs(a-180)
                v(9) = local_append_unique(v(9), angle);
            else
                v(10) = local_append_unique(v(10), angle);
            end
        end
    end
    % For PV > 5, VoxPul entries are:
    % (Length_ms, Bandwidth_Hz, FlipAngle_deg, Yes/No, Sharpness, Bwfac_Hz_ms, ...)
    % The pulse's 5th and 6th entries are Sharpness and Bwfac respectively.
    [sharpness,bwfac] = local_bruker_sharpness_bwfac(t, ...
        {sprintf('VoxPul%d',k),sprintf('VoxPulse%d',k)});
    if angle ~= "NA" && (sharpness ~= "NA" || bwfac ~= "NA")
        a = str2double(angle);
        if isfinite(a)
            if abs(a-90) <= abs(a-180)
                v(11) = local_append_unique(v(11),sharpness);
                v(13) = local_append_unique(v(13),bwfac);
            else
                v(12) = local_append_unique(v(12),sharpness);
                v(14) = local_append_unique(v(14),bwfac);
            end
        end
    end
end
end

function v = local_read_varian_metadata(f)
% Extract acquisition metadata directly from Varian procpar, following
% sequence-specific mappings used in the supplied MARSS simulation code.
% The helper local_varian_value reads the same procpar name/value records
% without loading FIDs or requiring readprocpar2 on the MATLAB path.
t = fileread(f);
v = repmat("NA",1,14);
v(1) = local_varian_value(t,'sw');
np = str2double(local_varian_value(t,'np'));
if isfinite(np), v(2) = string(np/2); end % real + imaginary -> complex points
te = str2double(local_varian_value(t,'te'));
tr = str2double(local_varian_value(t,'tr'));
if isfinite(te), v(3) = string(te*1000); end % seconds -> ms
if isfinite(tr), v(4) = string(tr*1000); end

sequence = lower(strtrim(local_varian_value(t,'seqfil')));
% Varian pulse widths pw90/pw180 are in microseconds in these procpar
% files; report both excitation and refocusing durations in milliseconds.
if contains(sequence,'steam')
    v(5) = local_varian_rf_name(t,'p1pat');
    v(6) = local_varian_pulse_ms(t,'pw90');
    v(9) = "90";
    % STEAM uses three 90-degree pulses, not a 180-degree refocusing pulse.
elseif contains(sequence,'laser')
    v(5) = local_varian_rf_name(t,'p1pat');
    v(6) = local_varian_pulse_ms(t,'pw90');
    v(7) = local_varian_rf_name(t,'pat180Y');
    v(8) = local_varian_pulse_ms(t,'pw180');
    v(9) = "90"; v(10) = "180";
elseif contains(sequence,'special') || contains(sequence,'isise')
    v(5) = local_varian_rf_name(t,'p1pat');
    v(6) = local_varian_pulse_ms(t,'pw90');
    v(7) = local_varian_rf_name(t,'p2pat');
    v(8) = local_varian_pulse_ms(t,'pw180');
    v(9) = "90"; v(10) = "180";
elseif contains(sequence,'press')
    % PRESS is not present among the supplied Varian DPs. Do not guess.
end
end

function name = local_varian_rf_name(txt, key)
name = local_varian_value(txt,key);
if name ~= "NA" && strlength(strtrim(name)) > 0
    name = name + ".RF";
else
    name = "NA";
end
end

function ms = local_varian_pulse_ms(txt, key)
ms = "NA";
us = str2double(local_varian_value(txt,key));
if isfinite(us)
    ms = string(us/1000);
end
end

function val = local_field_as_text(h,names)
val = "NA";
for i = 1:numel(names)
    if isfield(h,names{i})
        x = h.(names{i});
        if isnumeric(x) && ~isempty(x), val = string(x(1)); return; end
        if isstring(x) || ischar(x)
            x = string(x);
            if ~isempty(x) && strlength(strtrim(x(1)))>0
                val = x(1); return
            end
        end
    end
end
end

function val = local_bruker_scalar(txt,names)
val = "NA";
for i = 1:numel(names)
    expr = ['(?m)^##\$' regexptranslate('escape',names{i}) '=([^\r\n]*)'];
    tok = regexp(txt,expr,'tokens','once');
    if ~isempty(tok)
        val = strtrim(string(tok{1}));
        val = regexprep(val,'^<|>$','');
        if strlength(val)>0, return; end
    end
end
end

function val = local_bruker_duration(txt,names)
val = "NA";
for i = 1:numel(names)
    expr = ['(?m)^##\$' regexptranslate('escape',names{i}) '=\(\s*([-+0-9.eE]+)\s*,'];
    tok = regexp(txt,expr,'tokens','once');
    if ~isempty(tok), val = string(tok{1}); return; end
end
end

function val = local_varian_value(txt,name)
val = "NA";
% procpar: parameter definition line followed by count and value(s).
expr = ['(?m)^' regexptranslate('escape',name) ' [^\r\n]*\r?\n\s*\d+\s+([^\r\n]+)'];
tok = regexp(txt,expr,'tokens','once');
if isempty(tok), return; end
s = strtrim(string(tok{1}));
if startsWith(s,'"')
    m = regexp(char(s),'^"([^"]*)"','tokens','once');
    if ~isempty(m), val = string(m{1}); end
else
    parts = regexp(char(s),'\s+','split');
    if ~isempty(parts), val = string(parts{1}); end
end
end

function val = local_bruker_flip(txt,names)
% Third scalar in a Bruker VoxPul/VoxPulse structure = nominal flip angle.
val = "NA";
for i = 1:numel(names)
    expr = ['(?m)^##\$' regexptranslate('escape',names{i}) ...
        '=\(\s*[-+0-9.eE]+\s*,\s*[-+0-9.eE]+\s*,\s*([-+0-9.eE]+)\s*,'];
    tok = regexp(txt,expr,'tokens','once');
    if ~isempty(tok)
        val = string(str2double(tok{1}));
        return
    end
end
end

function [sharpness,bwfac] = local_bruker_sharpness_bwfac(txt,names)
% PV6+ VoxPulN = (length, bandwidth, flip angle, Yes/No, sharpness, bwfac, ...)
% The tuple can continue onto multiple lines. Read only the first six fields.
sharpness = "NA"; bwfac = "NA";
for i = 1:numel(names)
    expr = ['(?m)^##\$' regexptranslate('escape',names{i}) ...
        '=\(\s*[-+0-9.eE]+\s*,\s*[-+0-9.eE]+\s*,\s*' ...
        '[-+0-9.eE]+\s*,\s*(?:Yes|No)\s*,\s*' ...
        '([-+0-9.eE]+)\s*,\s*([-+0-9.eE]+)\s*,'];
    tok = regexp(txt,expr,'tokens','once','ignorecase');
    if ~isempty(tok)
        sharpness = string(str2double(tok{1}));
        bwfac = string(str2double(tok{2}));
        return
    end
end
end

function result = local_append_unique(existing,newValue)
result = existing;
if newValue == "NA" || strlength(strtrim(newValue)) == 0, return; end
if existing == "NA" || strlength(existing) == 0
    result = newValue;
elseif ~any(strtrim(split(existing,";")) == strtrim(newValue))
    result = existing + "; " + newValue;
end
end
