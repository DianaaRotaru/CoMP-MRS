%compMRS_DPmakeBasis_allDPs.m
% Diana RFotaru, Medical University of Vienna, 2026
%
% USAGE:[outbasis]=compMRS_DPmakeBasis_allDPs()
%
% DESCRIPTION:  
% Simple script to run compMRS_DPmakeBasis on all Data Packets (DPs).
% To be launched from the data folder containing the DPs, after adding the
% code directory and subfolders to path.
% 
% Input: None
% Output: 
% out:      k x {m x n} nested cell array where
%           k is the number of DPs
%           m is the number of subjects in the DP{k}
%           n is the number of sessions in the DP{k}
%           Each element {k}{m, n} is the basis set as a FID-A data struct.
%
 
function [outbasis]=compMRS_DPmakeBasis_allDPs()
clc
clear

    % Look for all DPs in the current folder
    res = dir('DP*');
    
    % run compMRS_DPproc on all DPs
    outbasis         = cell(1,length(res));
    for ii=1:length(res)
        try
        [outbasis{ii}]=compMRS_makeBasis(res(ii).name);
        catch 
            disp([res(ii).name ' error'])
        end
    end
end