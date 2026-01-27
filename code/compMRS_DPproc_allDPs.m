%compMRS_DPproc_allDPs.m

% Thanh Phong Lê, CIBM Center for Biomedical Imaging and École polytechnique fédérale de Lausanne, 2025
%
% USAGE:[out,outw]=compMRS_DPproc_allDPs()
%
% DESCRIPTION:  
% Simple script to run compMRS_DPproc on all Data Packets (DPs).
% To be launched from the data folder containing the DPs, after adding the
% code directory and subfolders to path.
% 
% Input: None
% Output: 
% out:      k x {m x n} nested cell array where
%           k is the number of DPs
%           m is the number of subjects in the DP{k}
%           n is the number of sessions in the DP{k}
%           Each element {k}{m, n} is a water suppressed FID-A data struct.
%
% out:      k x {m x n} nested cell array where
%           k is the number of DPs
%           m is the number of subjects in the DP{k}
%           n is the number of sessions in the DP{k}
%           Each element {k}{m, n} is a water unsuppressed FID-A data struct.
 
function [out, outw, out_auto, outw_auto]=compMRS_DPproc_allDPs()

    % Look for all DPs in the current folder
    res = dir('DP*');
    
    % run compMRS_DPproc on all DPs
    
    for ii=1:length(res)
        [out{ii}, outw{ii}, out_auto{ii}, outw_auto{ii}]=compMRS_DPproc(res(ii).name);
    end
end