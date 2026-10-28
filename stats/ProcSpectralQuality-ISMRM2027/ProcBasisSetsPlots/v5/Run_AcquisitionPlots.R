# Run from any working directory by locating the script beside this runner.
project_dir <- if (file.exists('BasisSetsPlots.R')) normalizePath('.') else {
  candidates <- c(file.path(getwd(), 'ProcBasisSetsPlots'),
                  'A:/CoMP-MRS/CoMP-MRS-DGR/stats/ProcSpectralQuality-ISMRM2027')
  found <- candidates[file.exists(file.path(candidates, 'BasisSetsPlots.R'))]
  if (length(found)) normalizePath(found[1]) else stop('BasisSetsPlots.R not found. Open ProcBasisSetsPlots.Rproj from the extracted project folder.')
}
source(file.path(project_dir, 'BasisSetsPlots.R'))
input_file <- 'A:/CoMP-MRS/CoMP-MRS-DGR/stats/ProcSpectralQuality-ISMRM2027/data/input_table/v4_20261025/CoMP_MRS_Rstats_input.csv'
if (!file.exists(input_file)) stop('Input CSV not found: ', input_file)
output_dir <- file.path(project_dir, 'BasisSetsPlots')
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
result <- PlotAcquisitionParameters(csv_file=input_file, out_dir=output_dir)
combined_plot <- CreateCombinedAcquisitionFigure(result$data, out_dir=output_dir)
message('Figures saved to: ', normalizePath(output_dir))
