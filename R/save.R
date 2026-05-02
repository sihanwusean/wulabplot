#' Save Plot with Forced Panel Size
#'
#' @description
#' Saves ggplot objects with absolute panel dimensions across various formats.
#' Automatically handles faceting to ensure each individual panel is the
#' requested size (e.g., exactly 2x2 cm).
#' This function ensures every figure panel is exactly the
#' requested size in centimeters, facilitating perfect alignment in Illustrator.
#'
#' @param p A ggplot object. Defaults to \code{last_plot()}, if this parameter is not provided.
#' @param type Character. Choice of standard dimensions: "2x2", "2.58x2", "4.9x2", "2x4.9", "4.9x4.9"
#' @param filename Character. Output filename with extension (.pdf, .png, .tiff, .tif).
#' @param custom_width (Optional) Numeric. Manual width for the panel in cm.
#' @param custom_height (Optional) Numeric. Manual height for the panel in cm.
#' @param dpi (Optional) Numeric. Resolution for raster formats (PNG/TIFF). Default is 300.
#'
#' @details
#' The function is facet-aware; it identifies every panel in the plot
#' (including those created by \code{facet_wrap} or \code{facet_grid})
#' and applies the specified dimensions to each. The final figure size is
#' calculated automatically to accommodate these panels plus all
#' surrounding labels and margins.
#'
#' Supports multiple output formats including PDF, PNG, and TIFF. Vector formats
#' (PDF) are exported via \code{grDevices::cairo_pdf}, while raster formats
#' (PNG, TIFF) utilize Cairo-based devices to ensure consistent font rendering,
#' transparency, and high-resolution output suitable for both digital presentations
#' and high-impact publications.
#'
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
save_wulab <- function(p = ggplot2::last_plot(),
                       type = "2x2",
                       filename = "plot.pdf",
                       custom_width = NULL,
                       custom_height = NULL,
                       dpi = 300) {

  if (is.null(p)) stop("Wu Lab Error: No plot found to save.")

  # 1. Validate Dimensions
  dims <- list(
    "2x2"     = c(2, 2),
    "2.58x2"  = c(2.58, 2),
    "4.9x2"   = c(4.9, 2),
    "2x4.9"   = c(2, 4.9),
    "4.9x4.9" = c(4.9, 4.9)
  )

  # Check custom input symmetry
  if (is.null(custom_width) != is.null(custom_height)) {
    stop("Wu Lab Error: Provide both custom_width and custom_height for custom sizing.")
  }

  if (!is.null(custom_width)) {
    pw <- custom_width
    ph <- custom_height
  } else {
    if (is.null(dims[[type]])) {
      stop("Wu Lab Error: Unknown type. Choose one of: ", paste(names(dims), collapse = ", "))
    }
    pw <- dims[[type]][1]
    ph <- dims[[type]][2]
  }

  # 2. Handle Extensions and File Types
  ext <- tolower(tools::file_ext(filename))
  if (ext == "") {
    ext <- "pdf"
    filename <- paste0(filename, ".pdf")
  }

  supported_exts <- c("pdf", "png", "tiff", "tif")
  if (!ext %in% supported_exts) {
    stop("Wu Lab Error: Unsupported format. Use: ", paste(supported_exts, collapse = ", "))
  }

  # 3. Process Grob Logic (The "Magic Factor" Sizing)
  gt <- ggplot2::ggplotGrob(p)
  panel_indices <- grep("^panel", gt$layout$name)

  if (length(panel_indices) == 0) {
    stop("The provided object does not contain any ggplot panels.")
  }

  panel_cols <- unique(gt$layout$l[panel_indices])
  panel_rows <- unique(gt$layout$t[panel_indices])

  gt$widths[panel_cols]  <- grid::unit(pw, "cm")
  gt$heights[panel_rows] <- grid::unit(ph, "cm")

  fw <- grid::convertWidth(sum(gt$widths), "cm", valueOnly = TRUE)
  fh <- grid::convertHeight(sum(gt$heights), "cm", valueOnly = TRUE)

  # 4. Open Graphics Device with Safeguard
  # Inches conversion for device functions
  fw_in <- fw / 2.54
  fh_in <- fh / 2.54

  if (ext == "pdf") {
    grDevices::cairo_pdf(filename = filename, width = fw_in, height = fh_in,
                         family = "Arial", bg = "transparent")
  } else if (ext == "png") {
    grDevices::png(filename = filename, width = fw_in, height = fh_in,
                   units = "in", res = dpi, bg = "transparent", type = "cairo")
  } else if (ext %in% c("tiff", "tif")) {
    grDevices::tiff(filename = filename, width = fw_in, height = fh_in,
                    units = "in", res = dpi, compression = "lzw", type = "cairo")
  }

  # Ensure device always closes even if drawing fails
  on.exit(if (names(dev.cur()) != "null device") grDevices::dev.off(), add = TRUE)

  grid::grid.draw(gt)

  # 5. Success Messaging
  message(sprintf("Successfully saved plot: %s", filename))
  message(sprintf("  - Individual panel size: %s x %s cm", pw, ph))
  message(sprintf("  - Total figure size: %.2f x %.2f cm (Format: %s)", fw, fh, toupper(ext)))
}
