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


# Poster-themed multipanel figure. Counts and distributions are DP-level, not animal-level.
# LASER/sLASER and SPECIAL/sSPECIAL are combined ONLY in this overview.
CreateCombinedAcquisitionFigure <- function(dp, out_dir = 'BasisSetsPlots',
                                             file_stem = 'BasisSets_CombinedOverview') {
  needed <- c('ggplot2','patchwork')
  missing <- needed[!vapply(needed,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing)) stop('Install packages: ',paste(missing,collapse=', '))
  dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
  library(ggplot2)
  library(patchwork)

  # Colors sampled/approximated from the CoMP-MRS magenta/purple poster.
  plum <- '#66215C'; purple <- '#8F45B0'; violet <- '#AF75C7'
  pink <- '#EF3C99'; rose <- '#CF6AB2'; pale <- '#F2D5EB'
  muted <- '#766579'; ink <- '#46213F'; grid <- '#F1E7F0'
  seq_cols <- c(PRESS=plum,STEAM=purple,LASER=rose,SPECIAL=pink)
  clean <- function(x) {
    x <- trimws(as.character(x))
    x[toupper(x) %in% c('','NA','N/A','NAN','NULL','CONFLICT')] <- NA_character_
    x
  }
  dp$SeqCombined <- clean(dp$Sequence)
  dp$SeqCombined[grepl('LASER',dp$SeqCombined,ignore.case=TRUE) & !is.na(dp$SeqCombined)] <- 'LASER'
  dp$SeqCombined[grepl('SPECIAL',dp$SeqCombined,ignore.case=TRUE) & !is.na(dp$SeqCombined)] <- 'SPECIAL'
  dp$SeqCombined <- factor(dp$SeqCombined,levels=c('PRESS','STEAM','LASER','SPECIAL'))

  th <- theme_minimal(base_size=11)+theme(
    plot.title=element_text(face='bold',size=12,color=ink,margin=margin(b=5)),
    plot.subtitle=element_text(size=9,color=muted),
    axis.text=element_text(size=9,color=ink),
    axis.title=element_text(size=9,color=muted),
    panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
    panel.grid.major.x=element_line(color=grid,linewidth=.3),
    plot.margin=margin(9,11,9,11),legend.position='none')
  missing_panel <- function(title) ggplot()+annotate('text',x=0,y=0,
    label='No reported values',size=3.4,color=muted)+xlim(-1,1)+ylim(-1,1)+
    labs(title=title)+theme_void()+theme(plot.title=element_text(face='bold',color=ink))

  # A: small vendor donut rather than a full-width bar chart.
  vendor_panel <- function() {
    x <- clean(dp$Vendor); x <- x[!is.na(x)]
    if(!length(x)) return(missing_panel('A  Vendor'))
    tab <- as.data.frame(table(x),stringsAsFactors=FALSE)
    names(tab) <- c('Category','n'); tab <- tab[tab$n>0,,drop=FALSE]
    tab$pct <- 100*tab$n/sum(tab$n)
    tab$lab <- paste0(tab$Category,'  ',tab$n,' (',round(tab$pct),'%)')
    tab$Category <- factor(tab$Category,levels=tab$Category)
    ggplot(tab,aes(x=2,y=n,fill=Category))+geom_col(width=.65,color='white',linewidth=.8)+
      coord_polar(theta='y')+xlim(.6,2.5)+
      scale_fill_manual(values=c(Bruker=plum,Varian=pink),
                        na.value=violet)+
      labs(title='A  Vendor',subtitle=paste0(sum(tab$n),' DPs'))+
      theme_void()+theme(plot.title=element_text(face='bold',size=12,color=ink),
        plot.subtitle=element_text(color=muted,size=9),
        legend.position='bottom',legend.title=element_blank(),
        legend.text=element_text(size=8,color=ink),
        legend.key.size=grid::unit(8,'pt'),plot.margin=margin(9,4,9,4))+
      guides(fill=guide_legend(nrow=2))
  }
  # Horizontal lollipop chart: easier to read many pulse shape names.
  lollipop <- function(field,title,max_categories=7) {
    x <- clean(dp[[field]]); x <- x[!is.na(x)]
    if(!length(x)) return(missing_panel(title))
    counts <- sort(table(x),decreasing=TRUE)
    if(length(counts)>max_categories) counts <- c(counts[seq_len(max_categories-1)],
      Other=sum(counts[-seq_len(max_categories-1)]))
    d <- data.frame(category=names(counts),n=as.integer(counts))
    d$category <- factor(d$category,levels=rev(d$category))
    ggplot(d,aes(y=category,x=n))+
      geom_segment(aes(x=0,xend=n,yend=category),linewidth=2.7,color=pale)+
      geom_point(size=3.7,color=purple)+
      geom_text(aes(label=n),hjust=-.45,size=3,color=ink)+
      scale_x_continuous(expand=expansion(mult=c(0,.2)))+
      labs(title=title,subtitle=paste0(length(x),' DPs with data'),x='Number of DPs',y=NULL)+th+
      theme(panel.grid.major.y=element_blank())
  }
  field_panel <- function() {
    x <- suppressWarnings(as.numeric(dp$FieldStrength));x <- x[is.finite(x)]
    if(!length(x)) return(missing_panel('C  Field strength'))
    tab <- as.data.frame(table(format(x,trim=TRUE,scientific=FALSE)),stringsAsFactors=FALSE)
    names(tab) <- c('field','n');tab$n <- as.integer(tab$n)
    tab$field <- factor(tab$field,levels=tab$field[order(as.numeric(tab$field))])
    ggplot(tab,aes(x=field,y=n))+
      geom_segment(aes(xend=field,y=0,yend=n),color=pale,linewidth=4)+
      geom_point(color=rose,size=5)+
      geom_text(aes(label=n),vjust=-.9,size=3,color=ink)+
      scale_y_continuous(expand=expansion(mult=c(0,.2)))+
      labs(title='C  Field strength',subtitle=paste0(length(x),' DPs'),x='Tesla',y='Number of DPs')+th+
      theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_line(color=grid))
  }
  # Median-and-dots chart, no boxplots. Each dot is one DP; black line = median.
  strip_panel <- function(field,title,unit='ms') {
    vals <- suppressWarnings(as.numeric(dp[[field]]))
    d <- data.frame(seq=dp$SeqCombined,value=vals)
    d <- d[!is.na(d$seq)&is.finite(d$value),,drop=FALSE]
    if(!nrow(d)) return(missing_panel(title))
    d$seq <- droplevels(d$seq)
    ggplot(d,aes(x=seq,y=value,color=seq))+
      geom_point(position=position_jitter(width=.16,height=0,seed=42),size=2.6,alpha=.78)+
      stat_summary(fun=median,geom='crossbar',width=.55,linewidth=.65,
                   aes(group=seq),color=ink)+
      scale_color_manual(values=seq_cols,drop=FALSE)+
      labs(title=title,subtitle=paste0(nrow(d),' DPs · points = DPs; line = median'),
           x=NULL,y=unit)+th+
      theme(axis.text.x=element_text(size=8,angle=25,hjust=1),
            panel.grid.major.y=element_line(color=grid))
  }
  panels <- list(vendor_panel(),lollipop('SeqCombined','B  Pulse sequence',4),
    field_panel(),strip_panel('TE','D  Echo time · TE'),
    strip_panel('TR','E  Repetition time · TR'),
    lollipop('ExcitationShape','F  Excitation RF shape'),
    lollipop('RefocusingShape','G  Refocusing RF shape'),
    strip_panel('ExcitationDuration','H  Excitation RF duration'),
    strip_panel('RefocusingDuration','I  Refocusing RF duration'))
  combined <- wrap_plots(panels,ncol=3,nrow=3,widths=c(.72,1.18,1.1),
                         heights=c(.95,1,1.05)) +
    plot_annotation(title='CoMP-MRS | Acquisition and RF pulse characteristics',
      subtitle='LASER includes sLASER; SPECIAL includes sSPECIAL · One observation per DP · Missing values excluded',
      theme=theme(plot.title=element_text(face='bold',size=21,color=plum),
        plot.subtitle=element_text(size=10,color=muted),
        plot.background=element_rect(fill='white',color=NA)))
  ggsave(file.path(out_dir,paste0(file_stem,'.png')),combined,
         width=17,height=14,dpi=300,bg='white',limitsize=FALSE)
  ggsave(file.path(out_dir,paste0(file_stem,'.pdf')),combined,
         width=17,height=14,bg='white',limitsize=FALSE)
  invisible(combined)
}
