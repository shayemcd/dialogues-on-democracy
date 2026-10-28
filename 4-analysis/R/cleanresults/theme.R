# Figure theme and palettes ----------------------------------------------------
#
# A clean Nature/Science default with journal variants, the Teal & Terracotta
# palette family, sans-serif type sized for an 85-180 mm column, and vector
# export.

#' Figure palettes
#'
#' `cr_palettes` holds the package palettes (Teal & Terracotta, Forest & Lilac,
#' Nordic Pine & Ochre, Partisan & Bridging) and the default category orders
#' the plotting functions use. The defaults were checked for color-vision
#' deficiency: every pair of colors stays distinguishable for protan, deutan,
#' and tritan readers. Slate and teal are nearly identical for red-blind
#' readers, so the defaults never use them together.
#'
#' @format A named list of character vectors of hex colors.
#' @export
cr_palettes <- list(
  teal_terracotta = c(navy = "#0B1B66", teal = "#20A49C", terracotta = "#C05A3E",
                      sage = "#6A8E7F", slate = "#8A9BA8", sand = "#D9C5B2"),
  forest_lilac = c(forest = "#1E4D3E", lilac = "#9B88B4", mint = "#48A999", plum = "#4A2E4B",
                   clay = "#D87D65", cream = "#EDE7DE"),
  nordic_ochre = c(pine = "#264653", sea_green = "#2A9D8F", ochre = "#E76F51",
                   apricot = "#F4A261", dusty_blue = "#6082B6", mist = "#E9ECEF"),
  partisan_bridge = c(democrat = "#2B5C8F", republican = "#B03A2E", bridging = "#7D5BA6",
                      neutral = "#718096"),
  # default orders by number of groups (validated, all pairs)
  `2` = c("#20A49C", "#C05A3E"),
  `3` = c("#8A9BA8", "#0B1B66", "#C05A3E"),
  `4` = c("#0B1B66", "#20A49C", "#C05A3E", "#7D5BA6"),
  `5` = c("#0B1B66", "#20A49C", "#C05A3E", "#7D5BA6", "#D9C5B2")
)

# Ink and structure colors.
.cr_ink <- c(text = "#1E293B", axis = "#333333", muted = "#555555", grid = "#F0F2F5",
             ref = "#718096", edge = "#64748B")

#' Pick colors for n groups
#'
#' @param n Number of groups.
#' @param palette `NULL` (the validated default for `n`: 2 to 5 groups), a
#'   palette name from [cr_palettes], or a character vector of colors.
#'   Set a project default with `options(cleanresults.palette = "forest_lilac")`.
#' @return A character vector of `n` colors.
#' @export
cr_palette <- function(n, palette = NULL) {
  palette <- palette %||% getOption("cleanresults.palette")
  if (is.character(palette) && length(palette) == 1 && palette %in% names(cr_palettes)) {
    palette <- cr_palettes[[palette]]
  }
  if (is.null(palette)) {
    if (n > 5) {
      stop(paste("More than 5 groups: facet the plot, combine groups, or pass `palette =`",
                 "with enough colors."), call. = FALSE)
    }
    palette <- cr_palettes[[as.character(max(2, n))]]
  }
  if (n > length(palette)) {
    stop(sprintf("%d groups but the palette has %d colors.", n, length(palette)), call. = FALSE)
  }
  unname(palette[seq_len(n)])
}

#' Scales in the cleanresults palettes
#'
#' @param palette Palette name or colors (see [cr_palette()]); `NULL` picks
#'   the validated default for the number of groups.
#' @param ... Passed to [ggplot2::discrete_scale()].
#' @return A ggplot2 scale.
#' @export
scale_fill_cr <- function(palette = NULL, ...) {
  ggplot2::discrete_scale("fill", palette = function(n) cr_palette(n, palette), ...)
}

#' @rdname scale_fill_cr
#' @export
scale_colour_cr <- function(palette = NULL, ...) {
  ggplot2::discrete_scale("colour", palette = function(n) cr_palette(n, palette), ...)
}

#' @rdname scale_fill_cr
#' @export
scale_color_cr <- scale_colour_cr

#' Publication ggplot2 themes
#'
#' `theme_cr()` is the lab default (Nature/Science style): thin dark axes,
#' faint horizontal gridlines, bold axis titles, legend top-left, bold
#' lowercase panel tags. Journal variants: `"psychsci"` (APA 7 /
#' Psychological Science: black axes, no grid, italic note), `"jcr"` (centered
#' titles, legend at the bottom), and `"polsci"` (minimal, dashed vertical
#' grid, for coefficient plots). Sizes follow the figure guide: axis titles
#' about 9.5 pt bold, tick labels about 8.5 pt at the default `base_size`.
#'
#' @param style `"nature"`, `"psychsci"`, `"jcr"`, or `"polsci"`.
#' @param base_size Base font size in points.
#' @param base_family Font family: `"sans"` (Helvetica/Arial), or e.g.
#'   `"Inter"` if installed.
#' @return A ggplot2 theme.
#' @export
theme_cr <- function(style = c("nature", "psychsci", "jcr", "polsci"), base_size = 10,
                     base_family = "sans") {
  style <- match.arg(style)
  el <- ggplot2::element_text
  common <- ggplot2::theme(
    plot.tag = el(size = 12, face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )
  th <- switch(style,
    nature = ggplot2::theme_classic(base_size, base_family) +
      ggplot2::theme(
        axis.line = ggplot2::element_line(colour = .cr_ink[["axis"]], linewidth = 0.4),
        axis.ticks = ggplot2::element_line(colour = .cr_ink[["axis"]], linewidth = 0.35),
        axis.title = el(size = ggplot2::rel(0.95), face = "bold", colour = "#111111"),
        axis.text = el(size = ggplot2::rel(0.85), colour = "#222222"),
        panel.grid.major.y = ggplot2::element_line(colour = .cr_ink[["grid"]], linewidth = 0.35),
        strip.text = el(size = ggplot2::rel(0.95), face = "bold", hjust = 0),
        legend.position = "top", legend.justification = "left",
        legend.title = el(size = ggplot2::rel(0.85), face = "bold"),
        legend.text = el(size = ggplot2::rel(0.8)),
        plot.title = el(size = ggplot2::rel(1.15), face = "bold", hjust = 0,
                        margin = ggplot2::margin(b = 4)),
        plot.subtitle = el(size = ggplot2::rel(0.9), colour = .cr_ink[["muted"]],
                           margin = ggplot2::margin(b = 8)),
        plot.caption = el(size = ggplot2::rel(0.75), colour = .cr_ink[["muted"]], hjust = 0),
        plot.margin = ggplot2::margin(10, 12, 10, 10)
      ),
    psychsci = ggplot2::theme_classic(base_size + 1, base_family) +
      ggplot2::theme(
        axis.line = ggplot2::element_line(colour = "black", linewidth = 0.5),
        axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.4),
        axis.title = el(size = ggplot2::rel(0.95), face = "bold"),
        axis.text = el(size = ggplot2::rel(0.85), colour = "black"),
        plot.title = el(size = ggplot2::rel(1.1), face = "bold", hjust = 0),
        plot.subtitle = el(size = ggplot2::rel(0.9), face = "italic", hjust = 0,
                           margin = ggplot2::margin(b = 8)),
        legend.title = el(size = ggplot2::rel(0.85), face = "bold"),
        plot.caption = el(size = ggplot2::rel(0.8), face = "italic", hjust = 0,
                          margin = ggplot2::margin(t = 8)),
        strip.text = el(face = "bold", size = ggplot2::rel(0.95))
      ),
    jcr = ggplot2::theme_classic(base_size + 0.5, base_family) +
      ggplot2::theme(
        plot.title = el(face = "bold", size = ggplot2::rel(1.05), hjust = 0.5, colour = "#111111"),
        plot.subtitle = el(size = ggplot2::rel(0.88), hjust = 0.5, colour = "#444444",
                           margin = ggplot2::margin(b = 8)),
        axis.title = el(face = "bold", size = ggplot2::rel(0.9)),
        axis.text = el(size = ggplot2::rel(0.85), colour = "#222222"),
        panel.grid.major.y = ggplot2::element_line(colour = "#E9ECEF", linewidth = 0.35),
        strip.text = el(face = "bold", size = ggplot2::rel(0.95), hjust = 0.5),
        strip.background = ggplot2::element_rect(fill = "#F8F9FA", colour = NA),
        legend.position = "bottom",
        legend.title = el(face = "bold", size = ggplot2::rel(0.85))
      ),
    polsci = ggplot2::theme_minimal(base_size, base_family) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(colour = "#E2E8F0", linewidth = 0.4,
                                                   linetype = "dashed"),
        panel.grid.major.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(colour = "#4A5568", linewidth = 0.45),
        axis.ticks.y = ggplot2::element_line(colour = "#4A5568", linewidth = 0.4),
        axis.title = el(face = "bold", size = ggplot2::rel(0.92), colour = "#1A202C"),
        axis.text = el(size = ggplot2::rel(0.85), colour = "#2D3748"),
        plot.title = el(face = "bold", size = ggplot2::rel(1.15), hjust = 0),
        plot.subtitle = el(size = ggplot2::rel(0.88), colour = "#4A5568",
                           margin = ggplot2::margin(b = 8)),
        strip.text = el(face = "bold", size = ggplot2::rel(0.9), colour = "#2D3748"),
        legend.position = "top", legend.justification = "left"
      )
  )
  th + common
}

# Journal column widths in mm (figure guide, section 8).
.cr_widths <- c(`1col` = 88, `1.5col` = 125, `2col` = 178)

# Readable label ink on a fill: near-black on light fills, white on dark ones.
ink_on <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  ifelse(lum > 0.35, .cr_ink[["text"]], "#FFFFFF")
}
