#' Save Plot with Forced Panel Size
#'
#' @description This function ensures every figure panel is exactly the
#' requested size in centimeters, facilitating perfect alignment in Illustrator.
#'
#' @param p A ggplot object. Defaults to \code{last_plot()}, if this parameter is not provided.
#' @param type Choice of "2x2", "2.58x2", "4.9x2", "2x4.9", "4.9x4.9"
#' @param filename Output filename. Must be PDF.
#' @param custom_width Optional numeric. Manual width for the panel in cm.
#' @param custom_height Optional numeric. Manual height for the panel in cm.
#'
#' @details
#' The function is facet-aware; it identifies every panel in the plot
#' (including those created by \code{facet_wrap} or \code{facet_grid})
#' and applies the specified dimensions to each. The final PDF size is
#' calculated automatically to accommodate these panels plus all
#' surrounding labels and margins.
#'
#' Figures are exported using \code{grDevices::cairo_pdf} with a
#' transparent background for seamless editing in Adobe Illustrator or other editors.
#'
#' @section Standard Presets (cm):
  #' \itemize{
  #'   \item \bold{2x2}: Standard square panel.
  #'   \item \bold{2.58x2}: Wide format for multi-group plots.
  #'   \item \bold{4.9x2/2x4.9}: Specialized rectangular panels for
  #'   kinetic data or vertical profiling.
  #'   \item \bold{4.9x4.9}: Big square panel for larger data set.
  #' }
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_wulab()
#' # Save as a standard square figure
#' save_wulab(p, type = "2x2", filename = "Figure_1A.pdf")
#'
#' @export
save_wulab <- function(p = ggplot2::last_plot(), type = "2x2", filename = "plot.pdf",
                       custom_width = NULL, custom_height = NULL) {

  if (is.null(p)) stop("No plot found to save.")

  # Define standard dimensions for each type (x axis * y axis in cm)
  dims <- list(
    "2x2"      = c(2, 2),
    "2.58x2"   = c(2.58, 2),
    "4.9x2"    = c(4.9, 2),
    "2x4.9"    = c(2, 4.9),
    "4.9x4.9"  = c(4.9, 4.9)
  )

  if (!is.null(custom_width) && !is.null(custom_height)) {
    pw <- custom_width; ph <- custom_height
  } else {
    pw <- dims[[type]][1]; ph <- dims[[type]][2]
  }

  if (!grepl("\\.pdf$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".pdf")
  }

  # 1. Convert to gtable
  gt <- ggplot2::ggplotGrob(p)

  # 2. Find ALL panel indices (handles facet_wrap/grid)
  # Instead of looking for "panel", we look for anything starting with "panel"
  panel_indices <- grep("^panel", gt$layout$name)

  if (length(panel_indices) == 0) {
    stop("The provided object does not contain any ggplot panels.")
  }

  # 3. Identify the unique columns and rows that contain panels
  panel_cols <- unique(gt$layout$l[panel_indices])
  panel_rows <- unique(gt$layout$t[panel_indices])

  # 4. Force EVERY panel column/row to the absolute dimensions
  gt$widths[panel_cols] <- grid::unit(pw, "cm")
  gt$heights[panel_rows] <- grid::unit(ph, "cm")

  # 5. Calculate total figure size (Sum of all panels + gutters + labels)
  fw <- grid::convertWidth(sum(gt$widths), "cm", valueOnly = TRUE)
  fh <- grid::convertHeight(sum(gt$heights), "cm", valueOnly = TRUE)

  # 6. Save with Cairo PDF (Transparent background, Arial embedded)
  grDevices::cairo_pdf(
    filename = filename,
    width = fw / 2.54,
    height = fh / 2.54,
    family = "Arial",
    bg = "transparent"
  )
  grid::grid.draw(gt)
  grDevices::dev.off()

  message(sprintf("Successfully saved faceted plot: %s", filename))
  message(sprintf("  - Individual panel size: %s x %s cm", pw, ph))
  message(sprintf("  - Total figure size: %.2f x %.2f cm", fw, fh))
}
