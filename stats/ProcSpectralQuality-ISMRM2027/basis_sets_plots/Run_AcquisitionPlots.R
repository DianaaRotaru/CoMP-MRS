# Run this file from the ProcSpectralQuality RStudio project.
# Place CoMP_MRS_Rstats_input.csv in the project directory, or edit csv_file below.
source('PlotAcquisitionParameters_FromRstats.R')
result <- PlotAcquisitionParameters(
  csv_file = 'CoMP_MRS_Rstats_input.csv',
  out_dir = file.path('Plots', 'AcquisitionParameters')
)
message('Plots saved to: ', normalizePath(file.path('Plots', 'AcquisitionParameters')))
