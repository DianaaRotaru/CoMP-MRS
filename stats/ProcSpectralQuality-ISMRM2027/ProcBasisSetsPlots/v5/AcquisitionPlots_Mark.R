# CoMP-MRS: acquisition-parameter summary plots for the data packets (DPs)
#
# Input : CoMP_MRS_Rstats_input.xlsx (sheet "Rstats_input"; one row per animal)
# Output: editable vector PDFs in BasisSetsPlots_Mark/
#
# One observation per DP: the animal-level rows are collapsed to unique DPs.
# DPs with a missing value are excluded only from the affected plot.
# Multi-pulse entries (e.g. "HS4_R20; Calculated" with "2; 0.994") are split
# so that each pulse is counted/plotted separately.
#
# Author : Mark Mikkelsen, Ph.D. <mark.mikkelsen@gmail.com>
# Created: October 2026
# Note   : Code generated with Claude Opus 5.5; human-inspected by the author


# ---- 0. Initialize ----------------------------------------------------------

rm(list = ls()) # Clear the current environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # If using RStudio, this clears all plots
cat("\014") # CTRL+L (clear console)


# ---- 1. Packages ------------------------------------------------------------

# tidyverse: readr, dplyr, tidyr, stringr, forcats, purrr, tibble, ggplot2
# (readxl is installed with the tidyverse but not attached, so it is loaded too)
pkgs <- c("tidyverse", "readxl", "ggrepel", "scales", "patchwork", "ggtext")
new_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(ggrepel)
  library(scales)
  library(patchwork)
  library(ggtext)
})


# ---- 2. Paths and data ------------------------------------------------------

base_dir   <- "/Users/mmikkel/Documents/GitHub/CoMP-MRS/CoMP-MRS-MM/stats/ProcSpectralQuality-ISMRM2027/ProcBasisSetsPlots/v5"
input_file <- "/Users/mmikkel/Documents/GitHub/CoMP-MRS/CoMP-MRS-MM/stats/ProcSpectralQuality-ISMRM2027/data/input_table/v4_20261025/CoMP_MRS_Rstats_input.xlsx"
out_dir    <- file.path(base_dir, "AcquisitionPlots_Mark")

# Check paths before anything is read or written (edit the paths above if
# running this script on another computer)
input_dir <- dirname(input_file)
if (!dir.exists(base_dir)) {
  stop("Base directory not found: ", base_dir,
       "\nUpdate `base_dir` at the top of section 2 to match your system.", call. = FALSE)
}
if (!dir.exists(input_dir)) {
  stop("Input directory not found: ", input_dir,
       "\nUpdate `input_file` at the top of section 2 to match your system.", call. = FALSE)
}
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file,
       "\nCheck the file name in `input_file` at the top of section 2.", call. = FALSE)
}

# Start from an empty output folder so no outdated plots are left behind
if (dir.exists(out_dir)) {
  old_files <- list.files(out_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  unlink(old_files, recursive = TRUE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

na_strings <- c("", "NA", "N/A", "NaN", "null")
raw <- read_excel(input_file, sheet = "Rstats_input", col_types = "text", na = na_strings)

acq_cols <- c("DP", "MRvendor", "MRfield", "MRsequence", "MRSTE", "MRSTR",
              "MRSexcitePulseType", "MRSexcitePulseDuration",
              "MRSrefocPulseType", "MRSrefocPulseDuration")

# Collapse animal-level rows to one row per DP
dp <- raw %>%
  select(all_of(acq_cols)) %>%
  filter(!is.na(DP)) %>%
  mutate(across(everything(), str_trim)) %>%
  distinct()

# Each DP must have a single set of acquisition parameters
dup_dp <- dp %>% count(DP) %>% filter(n > 1) %>% pull(DP)
if (length(dup_dp)) stop("Conflicting acquisition parameters for DP(s): ",
                         str_flatten(dup_dp, collapse = ", "))

n_dp <- nrow(dp)
message("Number of DPs: ", n_dp)

# Display order of sequences (any unexpected sequence is appended)
# LASER/sLASER and SPECIAL/sSPECIAL are pooled into one group each
seq_levels <- c("PRESS", "STEAM", "LASER + sLASER", "SPECIAL + sSPECIAL")

dp <- dp %>%
  mutate(MRsequence = fct_collapse(factor(MRsequence),
                                   "LASER + sLASER"     = c("LASER", "sLASER"),
                                   "SPECIAL + sSPECIAL" = c("SPECIAL", "sSPECIAL")) %>%
           fct_relevel(intersect(seq_levels, unique(.))),
         across(c(MRfield, MRSTE, MRSTR), parse_double))

seq_levels <- levels(dp$MRsequence)


# ---- 3. Plot styling --------------------------------------------------------

font_family <- "Arial"
text_col    <- "#000000"   # all text is black
# Font scales (multipliers; 1 = the unscaled base size for each element)
title_scale   <- 1.75  # plot titles
legend_scale  <- 1.75  # legend text (doughnut plots)
axis_scale    <- 1.75  # axis titles and tick labels
caption_scale <- 1.75  # captions below the plots
annot_scale   <- 1.5   # text annotated inside the plots (slice %, bar labels)
annot_scale_d <- 1.2   # text annotated inside doughnut plots
# Overall size multiplier for text, points and lines (set automatically when
# the combined figure is shrunk to A4 width; leave at 1)
fig_scale     <- 1

# Palette anchors: dark purple -> purple -> magenta -> light pink. Colors are
# interpolated evenly in CIELAB space, so any number of groups gets the widest
# possible, perceptually even spread (the previous 7-color palette contained
# near-identical colors, e.g. #e172e4 vs #de73e7).
pal_base <- c("#71279c", "#b256bb", "#dda3e2")
col_missing <- "#cbcad0"   # reserved for "Not reported"

# n colors spread evenly along the palette ramp
get_pal <- function(n) {
  colorRampPalette(pal_base, space = "Lab")(n)
}

theme_comp <- function(base_size = 11 * fig_scale) {
  theme_classic(base_size = base_size, base_family = font_family) +
    theme(
      text              = element_text(color = text_col),
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.grid        = element_blank(),
      axis.ticks        = element_line(color = "black"),
      axis.ticks.length = unit(0.15 * fig_scale, "cm"),      # positive = outward ticks
      axis.title        = element_text(color = text_col, face = "bold", size = rel(axis_scale)),
      axis.text         = element_text(color = text_col, face = "bold", size = rel(0.8 * axis_scale)),
      axis.line         = element_line(color = "black"),
      plot.title        = element_text(face = "bold", size = rel(1.2 * title_scale)),
      # plot.title        = element_blank(),
      legend.key        = element_blank()
    )
}

# Doughnut drawing area (data units) and its fixed size on the page. Fixing the
# panel size (instead of letting it fill the space left next to the legend)
# makes all doughnuts the same size, whatever the width of their legends.
donut_xlim    <- c(-1.6, 1.6)   # tighter limits = larger doughnut in the same panel
donut_ylim    <- c(-1.3, 1.3)
donut_panel_w <- 3.8   # panel width (in) at fig_scale = 1; height follows

fix_donut_size <- function(p) {
  # Build the gtable inside a temporary Cairo PDF device so text (Arial) is
  # measured with the same font metrics as the final PDFs
  tmp <- tempfile(fileext = ".pdf")
  if (capabilities("cairo")) cairo_pdf(tmp) else pdf(tmp, useDingbats = FALSE)
  on.exit({ dev.off(); unlink(tmp) }, add = TRUE)
  g   <- ggplotGrob(p)
  pan <- g$layout[str_detect(g$layout$name, "^panel"), ]
  w   <- donut_panel_w * fig_scale
  h   <- w * diff(donut_ylim) / diff(donut_xlim)
  g$widths[pan$l]  <- unit(w, "in")
  g$heights[pan$t] <- unit(h, "in")
  g
}

# Save as vector PDF with live text (editable in Adobe Illustrator).
# cairo_pdf embeds Arial; falls back to pdf() if cairo is unavailable.
save_pdf <- function(p, name, width, height) {
  f <- file.path(out_dir, str_c(name, ".pdf"))
  if (capabilities("cairo")) {
    ggsave(f, p, width = width, height = height, units = "in", device = cairo_pdf)
  } else {
    ggsave(f, p, width = width, height = height, units = "in", device = "pdf",
           useDingbats = FALSE)
  }
  message("Saved: ", f)
  invisible(f)
}

# Label helpers
fmt_pct <- function(x) sprintf("%.1f%%", x)
fmt_num <- function(x) format(x, trim = TRUE, drop0trailing = TRUE)

# "n = ..." labels with an italic n, built as plotmath expressions
# (text stays editable in the PDF)
quote_pm <- function(x) str_c('"', str_replace_all(x, '"', '\\\\"'), '"')
# e.g. "PRESS (n = 13)": group label followed by an italic-n sample size
n_labels <- function(group, n) {
  parse(text = sprintf('paste(%s, " (", italic("n"), " = %d)")', quote_pm(group), n))
}
# Two-line version, written as Markdown/HTML for ggtext::element_markdown()
# (plotmath atop() leaves a fixed gap between the lines that does not scale
# with font size):
#   "PRESS"          -> "PRESS"   / "(n = 13)"
#   "LASER + sLASER" -> "LASER +" / "sLASER" / "(n = 7)"  (text after "+" moves down)
n_labels_2line <- function(group, n) {
  n_txt <- sprintf("(<i>n</i> = %d)", n)
  if_else(str_detect(group, " \\+ "),
          str_c(str_replace(group, " \\+ ", " +<br>"), "<br>", n_txt),
          str_c(group, "<br>", n_txt))
}
# e.g. "n = 16 (44.4%)": string to draw with geom_text(parse = TRUE)
n_label_str <- function(n, suffix = "") {
  sprintf('paste(italic("n"), %s)', quote_pm(str_c(" = ", n, suffix)))
}


# ---- 4. Data frames for each variable ---------------------------------------

## 4a. Categorical counts (doughnuts) -----------------------------------------
# Count DPs per group; "Not reported" (if any) is always placed last
count_df <- function(data, var, levels = NULL) {
  data %>%
    transmute(group = replace_na(as.character({{ var }}), "Not reported")) %>%
    count(group, name = "n") %>%
    mutate(pct = 100 * n / sum(n),
           ord = if (is.null(levels)) rank(-n, ties.method = "first")
           else match(group, levels)) %>%
    arrange(group == "Not reported", ord, group) %>%
    select(-ord) %>%
    mutate(group = fct_inorder(group))
}

df_vendor   <- count_df(dp, MRvendor)
df_sequence <- count_df(dp, MRsequence, levels = seq_levels)
df_field  <- dp %>%
  mutate(field = str_c(fmt_num(MRfield), " T")) %>%
  count_df(field, levels = str_c(fmt_num(sort(unique(dp$MRfield))), " T"))

## 4b. Numeric values by sequence (dot plots) ---------------------------------
value_df <- function(var) {
  dp %>%
    filter(!is.na({{ var }})) %>%
    transmute(DP, sequence = MRsequence, vendor = MRvendor, value = {{ var }})
}

df_TE <- value_df(MRSTE)
df_TR <- value_df(MRSTR)

# One row per pulse; shape and duration lists are paired element-wise
split_pulses <- function(type_col, dur_col) {
  dp %>%
    select(DP, sequence = MRsequence, vendor = MRvendor,
           pulse = {{ type_col }}, value = {{ dur_col }}) %>%
    separate_longer_delim(c(pulse, value), delim = ";") %>%
    mutate(pulse = str_trim(pulse),
           value = parse_double(str_trim(value)))
}

df_exc_pulses <- split_pulses(MRSexcitePulseType, MRSexcitePulseDuration)
df_ref_pulses <- split_pulses(MRSrefocPulseType,  MRSrefocPulseDuration)

df_exc_dur <- df_exc_pulses %>% drop_na(pulse, value) %>% distinct()
df_ref_dur <- df_ref_pulses %>% drop_na(pulse, value) %>% distinct()

## 4c. RF pulse shapes (bar graphs) -------------------------------------------
# n = number of DPs using each pulse shape (a DP with two different shapes
# counts towards both); % is relative to the total number of DPs.
# Factor order: "Not reported" first (bottom of the y-axis), then by
# increasing count, so the most frequent shape is at the top.
shape_counts <- function(pulse_df) {
  pulse_df %>%
    mutate(pulse = replace_na(pulse, "Not reported")) %>%
    distinct(DP, pulse) %>%
    count(pulse, name = "n_total") %>%
    mutate(pct   = 100 * n_total / n_dp,
           label = n_label_str(n_total, sprintf(" (%s)", fmt_pct(pct)))) %>%
    arrange(pulse != "Not reported", n_total, desc(pulse)) %>%
    mutate(pulse = fct_inorder(pulse))
}

df_exc_shape <- shape_counts(df_exc_pulses)
df_ref_shape <- shape_counts(df_ref_pulses)


# ---- 5. Plot functions ------------------------------------------------------

## 5a. Doughnut ---------------------------------------------------------------
# Drawn as polygons in Cartesian space so ggrepel can keep labels apart.
plot_doughnut <- function(df, title,
                          r_in = 0.58, r_out = 1, n_arc = 200) {
  cols <- get_pal(sum(df$group != "Not reported"))
  if ("Not reported" %in% df$group) cols <- c(cols, col_missing)
  names(cols) <- levels(df$group)
  # Legend entries: group name with its sample size, e.g. "PRESS (n = 13)"
  group_n     <- setNames(df$n, df$group)
  legend_labs <- function(x) n_labels(x, group_n[x])
  
  df <- df %>%
    mutate(frac_end = cumsum(n) / sum(n), frac_start = lag(frac_end, default = 0),
           frac_mid = (frac_start + frac_end) / 2,
           theta_mid = pi / 2 - 2 * pi * frac_mid,  # clockwise from 12 o'clock
           label = fmt_pct(pct))                    # slice label: % only
  
  ring <- df %>%
    rowwise() %>%
    reframe(group = group, {
      th <- pi / 2 - 2 * pi * seq(frac_start, frac_end, length.out = n_arc)
      tibble(x = c(r_out * cos(th), r_in * cos(rev(th))),
             y = c(r_out * sin(th), r_in * sin(rev(th))))
    })
  
  lab <- df %>%
    mutate(x = r_out * cos(theta_mid), y = r_out * sin(theta_mid),
           nx = 0.35 * cos(theta_mid), ny = 0.35 * sin(theta_mid),
           hjust = if_else(cos(theta_mid) >= 0, 0, 1))
  
  ggplot() +
    geom_polygon(data = ring, aes(x, y, group = group, fill = group),
                 color = "white", linewidth = 0.6 * fig_scale) +
    geom_text_repel(data = lab, aes(x, y, label = label, hjust = hjust),
                    nudge_x = lab$nx, nudge_y = lab$ny,
                    family = font_family, color = text_col,
                    size = 3.4 * annot_scale * fig_scale, lineheight = 0.95,
                    direction = "y", box.padding = 0.35 * fig_scale, min.segment.length = 0,
                    segment.color = "gray40", segment.size = 0.3 * fig_scale,
                    seed = 1, max.overlaps = Inf) +
    annotate("text", x = 0, y = 0, label = sprintf("%d\nDPs", sum(df$n)),
             family = font_family, color = text_col, fontface = "bold",
             size = 6 * annot_scale_d * fig_scale) +
    scale_fill_manual(values = cols, labels = legend_labs, name = NULL) +   # no legend title
    coord_fixed(xlim = donut_xlim, ylim = donut_ylim, clip = "off") +
    labs(title = title, x = NULL, y = NULL) +
    theme_void(base_family = font_family, base_size = 11 * fig_scale) +
    theme(plot.background  = element_rect(fill = "white", color = NA),
          text       = element_text(color = text_col),
          plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1.2 * title_scale)),
          legend.position = "right",
          legend.text  = element_text(size = rel(0.8 * legend_scale), hjust = 0),   # left-align
          legend.key.size = unit(1.2 * fig_scale, "lines"))
}

## 5b. Horizontal dot plot (value on x, sequence on y) ------------------------
# Each sequence row gets its own palette color (no further breakdown)
plot_dots <- function(df, x_lab, title) {
  df <- df %>% mutate(sequence = droplevels(sequence))
  seqs   <- levels(df$sequence)
  cols   <- setNames(get_pal(length(seqs)), seqs)
  n_lab  <- df %>% distinct(DP, sequence) %>% count(sequence)
  seq_n  <- setNames(n_lab$n, n_lab$sequence)
  y_labs <- function(x) n_labels_2line(x, seq_n[x])   # sample size beneath the name
  
  ggplot(df, aes(x = value, y = sequence, color = sequence)) +
    # separator lines halfway between the sequence rows (drawn under the points)
    geom_hline(yintercept = seq_len(length(seqs) - 1) + 0.5,
               color = "gray85", linewidth = 0.4 * fig_scale) +
    geom_point(size = 2.6 * fig_scale, alpha = 0.85, shape = 16,
               position = position_jitter(width = 0, height = 0.18, seed = 42)) +
    scale_color_manual(values = cols, name = "Pulse sequence", breaks = seqs) +
    scale_y_discrete(labels = y_labs, limits = rev(levels(df$sequence))) +
    labs(title = title, x = x_lab, y = NULL,
         caption = sprintf("%d of %d DPs with data", n_distinct(df$DP), n_dp)) +
    theme_comp() +
    theme(plot.caption = element_text(color = "gray35", size = 8.5 * caption_scale * fig_scale),
          axis.text.y  = element_markdown(halign = 1, lineheight = 1.0),   # multi-line labels, right-aligned
          legend.position = "none")   # colors match the y-axis labels
}

## 5c. Horizontal bar graph (pulse shape; one bar per shape) ------------------
# Each shape gets its own palette color; "Not reported" is always gray
plot_shape_bars <- function(sc, title) {
  x_max  <- max(sc$n_total)
  shapes <- rev(levels(sc$pulse))                     # top-to-bottom order
  shapes_rep <- setdiff(shapes, "Not reported")
  cols <- c(setNames(get_pal(length(shapes_rep)), shapes_rep),
            "Not reported" = col_missing)
  
  ggplot(sc, aes(x = n_total, y = pulse, fill = pulse)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = label), parse = TRUE,
              hjust = 0, nudge_x = 0.25, family = font_family, color = text_col,
              size = 3.3 * annot_scale * fig_scale) +
    scale_fill_manual(values = cols, breaks = shapes) +
    scale_x_continuous(limits = c(0, x_max * 1.45), expand = c(0, 0),
                       breaks = pretty_breaks()) +
    labs(title = title, x = "Number of DPs", y = NULL,
         caption = sprintf("%% of all %d DPs\nDPs using more than one pulse shape count towards each", n_dp)) +
    theme_comp() +
    theme(plot.caption = element_text(color = "gray35", size = 8.5 * caption_scale * fig_scale),
          legend.position = "none")   # colors match the y-axis labels
}


# ---- 6. Make and save plots -------------------------------------------------

# All plots are built by one function so they can be rebuilt at another scale.
# Doughnuts are converted to fixed-size gtables (see fix_donut_size()), so any
# extra plot margin has to be added before that step.
build_plots <- function(plot_margin = NULL) list(
  # Doughnuts
  Vendor_doughnut        = plot_doughnut(df_vendor,   "Vendor"),
  PulSeq_doughnut = plot_doughnut(df_sequence, "Pulse sequence"),
  B0_doughnut = plot_doughnut(df_field,    "Field strength"),
  # Dot plots
  TE_dotplot = plot_dots(df_TE, "TE (ms)", "Echo time (TE)"),
  ExcDur_dotplot = plot_dots(df_exc_dur, "Duration (ms)",
                                         "Excitation RF pulse duration"),
  RefDur_dotplot = plot_dots(df_ref_dur, "Duration (ms)",
                                         "Refocusing RF pulse duration"),
  TR_dotplot = plot_dots(df_TR, "TR (ms)", "Repetition time (TR)"),
  # Bar graphs
  ExcShp_bar = plot_shape_bars(df_exc_shape, "Excitation RF pulse shape"),
  RefShp_bar = plot_shape_bars(df_ref_shape, "Refocusing RF pulse shape")
) %>%
  map(~ if (is.null(plot_margin)) .x else .x + theme(plot.margin = plot_margin)) %>%
  imap(~ if (str_detect(.y, "doughnut")) fix_donut_size(.x) else .x)
plots <- build_plots()

# Individual PDFs (width, height in inches)
sizes <- list(
  Vendor_doughnut = c(6.5, 4.5),
  PulSeq_doughnut = c(7.5, 4.5),
  B0_doughnut = c(6.5, 4.5),
  TE_dotplot = c(6, 5),
  ExcDur_dotplot = c(6, 5),
  RefDur_dotplot = c(6, 5),
  TR_dotplot = c(6, 5),
  ExcShp_bar = c(6, 4.5),
  RefShp_bar = c(6, 5)
)
iwalk(plots, ~ save_pdf(.x, .y, sizes[[.y]][1], sizes[[.y]][2]))

# Combined 3 x 3 overview
#   Row 1: doughnuts (vendor, sequence, field strength)
#   Row 2: dot plots (TE, TR, excitation RF pulse duration)
#   Row 3: refocusing RF pulse duration (dot plot) + RF pulse shapes (bars)
# wrap_elements() keeps each subplot self-contained, so axes are not aligned
# across panels and every subplot keeps the layout of its individual PDF
#
# The figure is laid out at 22 x 14 in and then shrunk to A4 width (210 mm):
# the plots are rebuilt with all text, points and lines scaled by the same
# factor, so the result looks like the large layout reduced to page width.
layout_w <- 22                     # design width (in)
layout_h <- 15.5                   # design height (in); taller for 3-line labels
a4_w     <- 210 / 25.4             # A4 width (in)
fig_scale <- a4_w / layout_w
combined <- build_plots(plot_margin = margin(10, 15, 10, 10) * fig_scale) %>%
  map(~ wrap_elements(full = .x)) %>%
  wrap_plots(ncol = 3, byrow = TRUE)
save_pdf(combined, "CoMP-MRS figure-1a", a4_w, layout_h * fig_scale)
fig_scale <- 1

message("All plots saved to: ", normalizePath(out_dir))
