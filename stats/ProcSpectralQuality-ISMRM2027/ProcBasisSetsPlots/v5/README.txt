ProcBasisSetsPlots — CoMP-MRS acquisition-parameter figures

1. Extract the entire ZIP to a folder.
2. Open ProcBasisSetsPlots.Rproj in RStudio.
3. Install missing packages if needed:
   install.packages(c('readr','dplyr','tidyr','ggplot2','stringr','patchwork'))
4. Open Run_AcquisitionPlots.R and click Source.

Input: A:/CoMP-MRS/CoMP-MRS-DGR/stats/ProcSpectralQuality-ISMRM2027/data/input_table/v4_20261025/CoMP_MRS_Rstats_input.csv
Output: BasisSetsPlots/BasisSets_CombinedOverview.png and .pdf, plus individual plots.

The combined figure follows the uploaded purple-pink reference's 4/3/2 panel
layout, with a compact vendor donut, sequence and field-strength donuts,
RF-shape bars, and jittered DP points plus median lines for TE, TR and pulse
durations. LASER/sLASER and SPECIAL/sSPECIAL are grouped only in the overview.
All numeric values are calculated from the CSV, not copied from the mockup.
DPs with missing data are excluded only from the affected panel.

Note: The currently uploaded old CSV does not include the new RF columns;
regenerate it with the latest MATLAB script to populate the RF panels.
