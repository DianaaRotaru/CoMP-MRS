# Run this file from the ProcBasisSetsPlots RStudio project.
# Input table is read from the shared CoMP-MRS input_table folder.
source('BasisSetsPlots.R')
result <- PlotAcquisitionParameters(
  csv_file = 'A:/CoMP-MRS/CoMP-MRS-DGR/stats/ProcSpectralQuality-ISMRM2027/data/input_table/v4_20261025/CoMP_MRS_Rstats_input.csv',
  out_dir = 'BasisSetsPlots'
)
message('Plots saved to: ', normalizePath('BasisSetsPlots'))

# Generate the combined, publication-style 3 x 3 overview (PNG and vector PDF).
combined_plot <- CreateCombinedAcquisitionFigure(result$data, out_dir = 'BasisSetsPlots')
message('Combined figure: BasisSetsPlots/BasisSets_CombinedOverview.png')
