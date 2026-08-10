## 21-style-v2.R — shared style for the v2 figure generation (ggplot2).
## The v1 figures in 09-figures.R are base graphics and stay untouched; new
## figures use ggplot2 for multi-panel and annotation work but keep the same
## palette, white background, and export contract (png 300 dpi + pdf with no
## timestamp + a .md caption sidecar). Chapters embed; they never plot.

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

## same anchors as 09-figures.R ("consistent with the DPMirt package palette")
PAL  <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#666666")
INK  <- "#1A1A1A"
INK2 <- "#4D4D4D"
GRID <- "#E5E5E5"

theory_theme <- function(base_size = 11.5) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
      axis.title = element_text(colour = INK),
      axis.text = element_text(colour = INK2),
      plot.title = element_text(face = "bold", size = base_size + 1, colour = INK),
      plot.subtitle = element_text(colour = INK2, size = base_size - 0.5),
      strip.text = element_text(face = "bold", colour = INK, hjust = 0),
      legend.position = "bottom",
      legend.title = element_text(colour = INK),
      legend.text = element_text(colour = INK2),
      plot.tag = element_text(face = "bold", size = base_size + 1),
      plot.margin = margin(8, 10, 6, 8)
    )
}

theory_theme_void <- function(base_size = 11.5) {
  theme_void(base_family = "Helvetica", base_size = base_size) +
    theme(
      plot.tag = element_text(face = "bold", size = base_size + 1,
                              family = "Helvetica"),
      plot.margin = margin(8, 10, 6, 8),
      legend.position = "none"
    )
}

## Box/arrow constructors for concept diagrams (ggplot on a hand-set 0-100
## coordinate plane; no TikZ dependency, and the labels can carry values read
## from frozen tables when needed).
box_row <- function(x, y, w, h, label, fill = "white", colour = INK2,
                    text_col = INK, size = 3.1, fontface = "plain",
                    linetype = "solid", lwd = 0.45) {
  data.frame(x = x, y = y, w = w, h = h, label = label, fill = fill,
              colour = colour, text_col = text_col, size = size,
              fontface = fontface, linetype = linetype, lwd = lwd,
              stringsAsFactors = FALSE)
}

arrow_row <- function(x0, y0, x1, y1, colour = INK2, lwd = 0.45,
                      linetype = "solid") {
  data.frame(x0 = x0, y0 = y0, x1 = x1, y1 = y1, colour = colour,
             lwd = lwd, linetype = linetype, stringsAsFactors = FALSE)
}

draw_diagram <- function(boxes, arrows = NULL, xlim = c(-1, 101),
                         ylim = c(-1, 101), base_size = 11.5) {
  p <- ggplot()
  if (!is.null(arrows) && nrow(arrows)) {
    p <- p + geom_segment(
      data = arrows,
      aes(x = x0, y = y0, xend = x1, yend = y1),
      colour = arrows$colour, linewidth = arrows$lwd,
      linetype = arrows$linetype,
      arrow = arrow(length = unit(1.7, "mm"), type = "closed"))
  }
  p +
    geom_rect(data = boxes,
              aes(xmin = x - w / 2, xmax = x + w / 2,
                  ymin = y - h / 2, ymax = y + h / 2),
              fill = boxes$fill, colour = boxes$colour,
              linetype = boxes$linetype, linewidth = boxes$lwd) +
    geom_text(data = boxes, aes(x = x, y = y, label = label),
              colour = boxes$text_col, size = boxes$size,
              fontface = boxes$fontface, family = "Helvetica",
              lineheight = 1.02) +
    scale_x_continuous(limits = xlim, expand = c(0, 0)) +
    scale_y_continuous(limits = ylim, expand = c(0, 0)) +
    theory_theme_void(base_size)
}

## Export contract identical in spirit to 09-figures.R::save_fig.
save_fig2 <- function(id, caption, plot, w = 7.2, h = 4.2) {
  ragg::agg_png(file.path(FIG, paste0(id, ".png")), width = w, height = h,
                units = "in", res = 300, background = "white")
  print(plot); dev.off()
  pdf(file.path(FIG, paste0(id, ".pdf")), width = w, height = h,
      timestamp = FALSE, producer = FALSE)
  print(plot); dev.off()
  writeLines(caption, file.path(FIG, paste0(id, ".md")))
  cat(sprintf("  %-22s png + pdf + caption\n", id))
}
