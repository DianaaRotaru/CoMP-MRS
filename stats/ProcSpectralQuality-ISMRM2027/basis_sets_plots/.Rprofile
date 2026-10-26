# Automatically load the acquisition plotting function when this RStudio project opens.
# No plots are generated until Run_AcquisitionPlots.R is executed.
script <- file.path(getwd(), 'PlotAcquisitionParameters_FromRstats.R')
if (file.exists(script)) {
  source(script, local = .GlobalEnv)
  message('CoMP-MRS acquisition plotting function loaded. Run Run_AcquisitionPlots.R to generate figures.')
} else {
  warning('Cannot find PlotAcquisitionParameters_FromRstats.R in the project directory.')
}
rm(script)
