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


# Nine-panel, poster-styled overview. All values are calculated from the CSV;
# the reference image is a visual guide, NOT a source of numeric results.
CreateCombinedAcquisitionFigure <- function(dp, out_dir = 'BasisSetsPlots',
                                             file_stem = 'BasisSets_CombinedOverview') {
  required <- c('ggplot2', 'patchwork', 'dplyr')
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
  if(length(missing)) stop('Install packages: ', paste(missing, collapse=', '))
  library(ggplot2)
  library(patchwork)
  dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

  plum <- '#751A86'; purple <- '#8E31A7'; violet <- '#AC68C4'
  pink <- '#F12C9B'; rose <- '#D14A9E'; blush <- '#FBE6F5'
  lilac <- '#D7A8E6'; grey <- '#B8B1B9'; ink <- '#4E1559'
  seq_colors <- c(PRESS=plum, STEAM=pink, 'LASER+sLASER'=lilac,
                  'SPECIAL+sSPECIAL'=grey)
  seq_levels <- names(seq_colors)
  clean <- function(x) {
    z <- trimws(as.character(x))
    z[toupper(z) %in% c('', 'NA', 'N/A', 'NAN', 'NULL', 'CONFLICT')] <- NA_character_
    z
  }
  d <- dp
  d$Vendor <- clean(d$Vendor)
  d$Sequence <- clean(d$Sequence)
  s <- toupper(d$Sequence)
  d$SeqCombined <- ifelse(grepl('LASER',s), 'LASER+sLASER',
                          ifelse(grepl('SPECIAL',s), 'SPECIAL+sSPECIAL',
                                 ifelse(grepl('STEAM',s), 'STEAM',
                                        ifelse(grepl('PRESS',s), 'PRESS', NA_character_))))
  d$SeqCombined <- factor(d$SeqCombined, levels=seq_levels)
  total <- nrow(d)
  pct <- function(n, den) sprintf('%.1f%%', 100*n/den)

  base_theme <- theme_minimal(base_size=10) + theme(
    text=element_text(color=ink, family='sans'),
    plot.title=element_text(face='bold',size=12,color=ink,margin=margin(b=8)),
    axis.title=element_text(size=9),axis.text=element_text(size=8.5),
    panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    panel.grid.major.y=element_line(color='#F1E6F0',linewidth=.25),
    plot.background=element_rect(fill='white',color='#E7B6DF',linewidth=.65),
    plot.margin=margin(13,13,13,13),legend.title=element_blank())
  empty_panel <- function(title) ggplot()+annotate('text',x=0,y=0,label='No reported values',
                    size=4,color=grey)+xlim(-1,1)+ylim(-1,1)+labs(title=title)+
                    theme_void()+theme(plot.title=element_text(face='bold',color=ink,size=12),
                                       plot.background=element_rect(fill='white',color='#E7B6DF'))

  donut <- function(x, title, colors=NULL, sequence=FALSE, legend_cols=1) {
    x <- clean(x); x <- x[!is.na(x)]
    if(!length(x)) return(empty_panel(title))
    counts <- as.data.frame(table(x), stringsAsFactors=FALSE)
    names(counts) <- c('category','n'); counts <- counts[counts$n>0,,drop=FALSE]
    if(sequence) counts$category <- factor(counts$category,levels=seq_levels)
    counts$label <- paste0(as.character(counts$category),' (n = ',counts$n,', ',
                           pct(counts$n,sum(counts$n)),')')
    cols <- if(is.null(colors)) setNames(rep(c(plum,pink,violet,rose,grey),length.out=nrow(counts)),
                                        as.character(counts$category)) else colors
    ggplot(counts,aes(x=2,y=n,fill=category))+
      geom_col(color='white',linewidth=.8,width=.75)+coord_polar(theta='y')+
      xlim(.45,2.5)+scale_fill_manual(values=cols,drop=FALSE,
          labels=setNames(counts$label,as.character(counts$category)))+
      labs(title=title)+theme_void(base_size=10)+
      theme(plot.title=element_text(face='bold',size=12,color=ink),
            plot.background=element_rect(fill='white',color='#E7B6DF',linewidth=.65),
            plot.margin=margin(13,10,10,10),legend.position='bottom',
            legend.text=element_text(size=8),legend.key.size=grid::unit(8,'pt'))+
      guides(fill=guide_legend(ncol=legend_cols))
  }

  # Field strength donut with exact reported Tesla categories.
  field <- clean(d$FieldStrength)
  field <- suppressWarnings(as.numeric(field))
  field_chr <- ifelse(is.finite(field), paste0(format(field,trim=TRUE,scientific=FALSE),' T'),NA_character_)
  pA <- donut(d$Vendor,paste0('A   Vendor (n = ',sum(!is.na(d$Vendor)),' DPs)'),
              c(Bruker=purple,Varian=pink))
  pB <- donut(as.character(d$SeqCombined),paste0('B   Pulse sequence (n = ',sum(!is.na(d$SeqCombined)),' DPs)'),
              seq_colors,sequence=TRUE)
  pC <- donut(field_chr,paste0('C   Field strength (n = ',sum(is.finite(field)),' DPs)'),
              setNames(c(pink,violet,grey,plum,rose)[seq_along(unique(na.omit(field_chr)))],
                       unique(na.omit(field_chr))))

  # Strip plot with median and IQR, one dot per DP, log y when appropriate.
  strip <- function(field_name,title,unit='ms',log_y=FALSE) {
    vals <- suppressWarnings(as.numeric(d[[field_name]]))
    data <- data.frame(seq=d$SeqCombined,value=vals)
    data <- data[!is.na(data$seq)&is.finite(data$value),,drop=FALSE]
    if(log_y) data <- data[data$value>0,,drop=FALSE]
    if(!nrow(data)) return(empty_panel(title))
    data$seq <- factor(as.character(data$seq),levels=seq_levels)
    stat <- aggregate(value~seq,data,FUN=function(z) c(median=median(z),
                      lo=unname(quantile(z,.25)),hi=unname(quantile(z,.75))))
    stat <- data.frame(seq=stat$seq,stat$value,check.names=FALSE)
    names(stat)[2:4] <- c('median','lo','hi')
    p <- ggplot(data,aes(x=seq,y=value,color=seq))+
      geom_point(position=position_jitter(width=.13,height=0,seed=2026),size=2.3,alpha=.85)+
      geom_segment(data=stat,aes(x=as.numeric(seq)-.27,xend=as.numeric(seq)+.27,
                  y=median,yend=median,color=seq),inherit.aes=FALSE,linewidth=1.05)+
      scale_color_manual(values=seq_colors,drop=FALSE)+
      labs(title=title,x=NULL,y=unit)+base_theme+
      theme(legend.position='none',axis.text.x=element_text(angle=20,hjust=1,size=8),
            panel.grid.major.y=element_line(color='#EFE2EE'))
    if(log_y) p <- p + scale_y_log10()
    p
  }
  pD <- strip('TE','D   Echo time, TE (ms)',log_y=TRUE)
  pE <- strip('TR','E   Repetition time, TR (ms)')
  pH <- strip('ExcitationDuration','H   Excitation RF pulse duration (ms)',log_y=TRUE)
  pI <- strip('RefocusingDuration','I   Refocusing RF pulse duration (ms)',log_y=TRUE)

  # Horizontal bars are more legible than pies for many RF shape names.
  shape_bar <- function(field_name,title,max_categories=7) {
    x <- clean(d[[field_name]]); x <- x[!is.na(x)]
    if(!length(x)) return(empty_panel(title))
    tab <- sort(table(x),decreasing=TRUE)
    if(length(tab)>max_categories) {
      keep <- seq_len(max_categories-1)
      tab <- c(tab[keep], 'Other'=sum(tab[-keep]))
    }
    dat <- data.frame(shape=names(tab),n=as.integer(tab))
    dat$shape <- factor(dat$shape,levels=rev(dat$shape))
    ggplot(dat,aes(y=shape,x=n))+
      geom_col(fill=purple,width=.66)+
      geom_text(aes(label=paste0(n,' (',pct(n,total),')')),hjust=-.10,size=2.8,color=ink)+
      scale_x_continuous(expand=expansion(mult=c(0,.4)))+
      labs(title=title,x='Number of DPs',y=NULL)+base_theme+
      theme(panel.grid.major.y=element_blank())
  }
  pF <- shape_bar('ExcitationShape','F   Excitation RF pulse shape')
  pG <- shape_bar('RefocusingShape','G   Refocusing RF pulse shape')

  # Match the reference's visual hierarchy: four top, three middle, two bottom.
  top <- (pA + pB + pC + pD) + plot_layout(widths=c(.77,1.23,.95,1.25))
  mid <- (pE + pF + pG) + plot_layout(widths=c(1.45,1.27,1.28))
  bottom <- (pH + pI) + plot_layout(widths=c(1,1))
  combined <- (top / mid / bottom) +
    plot_layout(heights=c(1.02,1,1.08)) +
    plot_annotation(
      title=paste0('CoMP-MRS Acquisition Parameters Across Datasets (n = ',total,' DPs)'),
      subtitle='One observation per DP  |  LASER includes sLASER; SPECIAL includes sSPECIAL',
      caption='Points represent individual DPs; horizontal lines mark medians. Missing values are excluded per panel. Percentages are based on DPs with reported values for each categorical panel.',
      theme=theme(
        plot.title=element_text(face='bold',size=23,color='white',hjust=.5,
                                margin=margin(9,0,9,0)),
        plot.subtitle=element_text(color=ink,size=11,hjust=.5,margin=margin(b=8)),
        plot.caption=element_text(color=ink,size=9,hjust=.5,margin=margin(t=9)),
        plot.title.position='plot',
        plot.background=element_rect(fill='white',color=NA)))
  # A gradient cannot be drawn directly by ggplot's title theme; purple title
  # lettering on a pale pink header keeps the figure vector-safe.
  combined <- combined & theme(plot.background=element_rect(fill='white',color=NA))
  combined <- combined + plot_annotation(
    title=paste0('CoMP-MRS Acquisition Parameters Across Datasets (n = ',total,' DPs)'),
    subtitle='One observation per DP  |  LASER+sLASER and SPECIAL+sSPECIAL grouped',
    caption='Numerical panels: dots = DPs; horizontal line = median. Missing values excluded per panel.',
    theme=theme(plot.title=element_text(face='bold',size=23,color=plum,hjust=.5),
                plot.subtitle=element_text(size=11,color=rose,hjust=.5),
                plot.caption=element_text(size=9,color=ink,hjust=.5),
                plot.background=element_rect(fill='white',color=NA)))
  ggplot2::ggsave(file.path(out_dir,paste0(file_stem,'.png')),combined,
                  width=19,height=13,dpi=300,bg='white',limitsize=FALSE)
  ggplot2::ggsave(file.path(out_dir,paste0(file_stem,'.pdf')),combined,
                  width=19,height=13,bg='white',limitsize=FALSE)
  invisible(combined)
}
