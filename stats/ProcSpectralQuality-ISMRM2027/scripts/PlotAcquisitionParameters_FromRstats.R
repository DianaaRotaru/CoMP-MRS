# CoMP-MRS acquisition parameter plots directly from the MATLAB-generated
# CoMP_MRS_Rstats_input.csv. No separate spreadsheet join is required.
# One observation per data packet (DP), not per animal.
# Requires: readr, dplyr, tidyr, ggplot2, stringr

PlotAcquisitionParameters <- function(
    csv_file = "CoMP_MRS_Rstats_input.csv",
    out_dir = file.path("Plots", "AcquisitionParameters"),
    exclude_dp = character(),
    show_plots = FALSE) {

  pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr")
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) stop("Install required packages: ", paste(missing_pkgs, collapse = ", "))
  if (!file.exists(csv_file)) stop("Input CSV not found: ", csv_file)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  raw <- readr::read_csv(csv_file, col_types = readr::cols(.default = readr::col_character()),
                         na = c("", "NA", "N/A", "NaN", "null"), show_col_types = FALSE)
  if (!"DP" %in% names(raw)) stop("Input must contain a DP column")
  raw <- dplyr::filter(raw, !is.na(.data$DP), !(.data$DP %in% exclude_dp))

  # Supports the existing Rstats naming convention and the acquisition-table
  # naming convention, should either be present in the CSV.
  aliases <- list(
    Species = c("AnimalSpecies", "Species"),
    Vendor = c("MRvendor", "Vendor"),
    Software = c("MRsoftwareversion", "Software_version"),
    FieldStrength = c("MRfield", "Field_strength_T"),
    Sequence = c("MRsequence", "Pulse_sequence"),
    SpectralWidth = c("MRSsw", "Spectral_width_Hz"),
    SpectralPoints = c("MRSnpts", "Spectral_points"),
    TE = c("MRSTE", "TE_ms"),
    TR = c("MRSTR", "TR_ms"),
    ExcitationShape = c("MRSexcitePulseType", "Excitation_RF_pulse"),
    ExcitationDuration = c("MRSexcitePulseDuration", "Excitation_duration"),
    ExcitationFlipAngle = c("MRSexciteFlipAngle", "Excitation_flip_angle_deg"),
    ExcitationSharpness = c("MRSexciteSharpness", "Excitation_sharpness"),
    ExcitationBwfac = c("MRSexciteBwfac", "Excitation_bwfac_Hz_ms"),
    RefocusingShape = c("MRSrefocPulseType", "Refocusing_RF_pulse"),
    RefocusingDuration = c("MRSrefocPulseDuration", "Refocusing_duration"),
    RefocusingFlipAngle = c("MRSrefocFlipAngle", "Refocusing_flip_angle_deg"),
    RefocusingSharpness = c("MRSrefocSharpness", "Refocusing_sharpness"),
    RefocusingBwfac = c("MRSrefocBwfac", "Refocusing_bwfac_Hz_ms"))

  pick <- function(cols) {
    hit <- intersect(cols, names(raw))
    if (!length(hit)) return(rep(NA_character_, nrow(raw)))
    z <- raw[[hit[1]]]
    if (length(hit) > 1) for (h in hit[-1]) z <- dplyr::coalesce(z, raw[[h]])
    trimws(z)
  }
  dp <- data.frame(DP = raw$DP, stringsAsFactors = FALSE)
  for (nm in names(aliases)) dp[[nm]] <- pick(aliases[[nm]])
  if (!any(names(raw) %in% unlist(aliases)[grepl("MRSexcite|MRSrefoc|Excitation_|Refocusing_", unlist(aliases))])) {
    warning("RF pulse columns are missing. Regenerate Rstats_input using the latest MATLAB script.")
  }

  # Prevent pseudoreplication: one value per DP. Flag disagreements between
  # subjects within a DP rather than arbitrarily selecting one.
  collapse_dp <- function(z) {
    z <- unique(z[!is.na(z) & nzchar(z)])
    if (!length(z)) return(NA_character_)
    if (length(z) > 1) return("CONFLICT")
    z
  }
  conflicts <- dp |>
    tidyr::pivot_longer(-DP, names_to = "Variable", values_to = "Value") |>
    dplyr::filter(!is.na(.data$Value), nzchar(.data$Value)) |>
    dplyr::distinct(.data$DP, .data$Variable, .data$Value) |>
    dplyr::count(.data$DP, .data$Variable, name = "DistinctValues") |>
    dplyr::filter(.data$DistinctValues > 1)
  readr::write_csv(conflicts, file.path(out_dir, "DP_metadata_conflicts.csv"))
  if (nrow(conflicts)) warning("Conflicting metadata found within DPs; see DP_metadata_conflicts.csv")
  dp <- dp |>
    dplyr::group_by(.data$DP) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), collapse_dp), .groups = "drop")
  readr::write_csv(dp, file.path(out_dir, "AcquisitionParameters_ByDP.csv"))

  numeric_vars <- c("FieldStrength", "SpectralWidth", "SpectralPoints", "TE", "TR",
                    "ExcitationDuration", "ExcitationFlipAngle", "ExcitationSharpness",
                    "ExcitationBwfac", "RefocusingDuration", "RefocusingFlipAngle",
                    "RefocusingSharpness", "RefocusingBwfac")
  for (nm in numeric_vars) dp[[nm]] <- suppressWarnings(as.numeric(dp[[nm]]))

  labels <- c(Species="Species", Vendor="Vendor", Software="Software version",
              Sequence="Pulse sequence", ExcitationShape="Excitation RF shape",
              RefocusingShape="Refocusing RF shape")
  num_labels <- c(FieldStrength="Field strength (T)", SpectralWidth="Spectral width (Hz)",
                  SpectralPoints="Spectral points", TE="TE (ms)", TR="TR (ms)",
                  ExcitationDuration="Excitation duration (ms)",
                  ExcitationFlipAngle="Excitation flip angle (degrees)",
                  ExcitationSharpness="Excitation sharpness",
                  ExcitationBwfac="Excitation Bwfac (Hz ms)",
                  RefocusingDuration="Refocusing duration (ms)",
                  RefocusingFlipAngle="Refocusing flip angle (degrees)",
                  RefocusingSharpness="Refocusing sharpness",
                  RefocusingBwfac="Refocusing Bwfac (Hz ms)")

  theme_comp <- function() ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   panel.grid.minor = ggplot2::element_blank())
  plots <- list()
  for (nm in names(labels)) {
    z <- dp
    z$Category <- ifelse(is.na(z[[nm]]) | z[[nm]] == "", "Not reported", z[[nm]])
    counts <- dplyr::count(z, .data$Category, name = "DPs")
    readr::write_csv(counts, file.path(out_dir, paste0(nm, "_counts.csv")))
    p <- ggplot2::ggplot(counts, ggplot2::aes(x = stats::reorder(.data$Category, .data$DPs),
                                               y = .data$DPs)) +
      ggplot2::geom_col(fill = "#8756A3", width = .72) +
      ggplot2::geom_text(ggplot2::aes(label = .data$DPs), hjust = -.15, size = 3.4) +
      ggplot2::coord_flip() + ggplot2::labs(title = labels[[nm]], x = NULL, y = "Number of DPs") +
      ggplot2::expand_limits(y = max(counts$DPs, 1) * 1.16) + theme_comp()
    plots[[nm]] <- p
    ggplot2::ggsave(file.path(out_dir, paste0(nm, "_counts.png")), p,
                    width = 8, height = max(4, .34*nrow(counts)+2), dpi = 300)
  }

  for (nm in names(num_labels)) {
    z <- dplyr::filter(dp, !is.na(.data[[nm]]), !is.na(.data$Sequence),
                       .data$Sequence != "CONFLICT")
    if (!nrow(z)) next
    p <- ggplot2::ggplot(z, ggplot2::aes(x = .data$Sequence, y = .data[[nm]])) +
      ggplot2::geom_boxplot(outlier.shape = NA, fill = "#D6C3E7", width = .58,
                            alpha = .8, na.rm = TRUE) +
      ggplot2::geom_jitter(width = .12, height = 0, color = "#6D328C", size = 2,
                           alpha = .85) +
      ggplot2::labs(title = num_labels[[nm]], x = "Pulse sequence", y = num_labels[[nm]],
                    subtitle = paste(nrow(z), "DPs with reported values")) + theme_comp()
    plots[[nm]] <- p
    ggplot2::ggsave(file.path(out_dir, paste0(nm, "_by_sequence.png")), p,
                    width = 8.5, height = 5.2, dpi = 300)
  }

  # Optional additional comparison by vendor, for all numerical variables.
  for (nm in names(num_labels)) {
    z <- dplyr::filter(dp, !is.na(.data[[nm]]), !is.na(.data$Vendor),
                       .data$Vendor != "CONFLICT")
    if (!nrow(z)) next
    p <- ggplot2::ggplot(z, ggplot2::aes(x = .data$Vendor, y = .data[[nm]])) +
      ggplot2::geom_boxplot(outlier.shape = NA, fill = "#D6C3E7", width = .55,
                            na.rm = TRUE) +
      ggplot2::geom_jitter(width = .12, color = "#6D328C", size = 2) +
      ggplot2::labs(title = num_labels[[nm]], x = "Vendor", y = num_labels[[nm]]) + theme_comp()
    ggplot2::ggsave(file.path(out_dir, paste0(nm, "_by_vendor.png")), p,
                    width = 7, height = 5, dpi = 300)
  }
  invisible(list(data = dp, conflicts = conflicts, plots = plots))
}

# Run from the directory containing CoMP_MRS_Rstats_input.csv:
# result <- PlotAcquisitionParameters()
# To apply the exclusions used in the existing LoadData.R:
# result <- PlotAcquisitionParameters(exclude_dp = c("DP01", "DP32"))
