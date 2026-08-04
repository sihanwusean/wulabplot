#' Save Plot with Forced Panel Size
#'
#' @description
#' Saves ggplot objects with absolute panel dimensions across various formats.
#' Automatically handles faceting to ensure each individual panel is the
#' requested size (e.g., exactly 2x2 cm).
#' This function ensures every figure panel is exactly the
#' requested size in centimeters, facilitating perfect alignment in Illustrator.
#'
#' @param filename Character or ggplot object. Output filename with extension (.pdf, .png, .tiff, .tif).
#' Can be passed as the first positional argument (matching \code{ggsave}), or omitted to default to \code{"plot.pdf"}.
#' @param plot A ggplot object or character filename. Defaults to \code{last_plot()}.
#' @param type (Optional) Character. Choice of standard dimensions: "2x2", "2.58x2", "4.9x2", "2x4.9", "4.9x4.9". If omitted and no custom dimensions are specified, defaults to "2x2" with a warning reminder.
#' @param custom_width (Optional) Numeric. Manual width for the panel in cm.
#' @param custom_height (Optional) Numeric. Manual height for the panel in cm.
#' @param dpi (Optional) Numeric. Resolution for raster formats (PNG/TIFF). Default is 300.
#' @param p Deprecated/Legacy parameter for specifying the plot object. Maintained for backwards compatibility.
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
#'
#' # Save using filename as first positional argument (matching ggsave)
#' save_wulab("Figure_1A.pdf", p, type = "2x2")
#'
#' # Save last_plot() with automatic .pdf extension
#' save_wulab("Figure_1A", type = "2x2")
#'
#' @export
save_wulab <- function(filename = NULL,
                       plot = NULL,
                       type = NULL,
                       custom_width = NULL,
                       custom_height = NULL,
                       dpi = 300,
                       p = NULL) {

  # 1. Flexible Argument Resolution (Supports filename as 1st arg or plot as 1st arg)
  target_p <- NULL
  target_filename <- NULL

  # Check 'p' (legacy parameter)
  if (!is.null(p)) {
    if (inherits(p, "ggplot") || inherits(p, "gtable") || inherits(p, "grob")) {
      target_p <- p
    } else if (is.character(p)) {
      target_filename <- p
    }
  }

  # Check 'plot' argument
  if (!is.null(plot)) {
    if (inherits(plot, "ggplot") || inherits(plot, "gtable") || inherits(plot, "grob")) {
      target_p <- plot
    } else if (is.character(plot)) {
      target_filename <- plot
    }
  }

  # Check 'filename' argument (which can be a string or a ggplot object if passed positionally)
  if (!is.null(filename)) {
    if (inherits(filename, "ggplot") || inherits(filename, "gtable") || inherits(filename, "grob")) {
      target_p <- filename
    } else if (is.character(filename)) {
      target_filename <- filename
    }
  }

  # Assign defaults if still unresolved
  if (is.null(target_p)) {
    target_p <- ggplot2::last_plot()
  }
  if (is.null(target_filename)) {
    target_filename <- "plot.pdf"
  }

  if (is.null(target_p)) {
    stop("Wu Lab Error: No plot found to save.")
  }

  # 2. Check Extension and Smart Default (.pdf)
  ext <- tolower(tools::file_ext(target_filename))
  if (ext == "") {
    ext <- "pdf"
    target_filename <- paste0(target_filename, ".pdf")
  }

  supported_exts <- c("pdf", "png", "tiff", "tif")
  if (!ext %in% supported_exts) {
    stop("Wu Lab Error: Unsupported format. Use: ", paste(supported_exts, collapse = ", "))
  }

  # 3. Create Parent Directory Automatically if missing
  dir_path <- dirname(target_filename)
  if (dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  # 4. Validate Dimensions & Check Missing 'type' Warning
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
    if (is.null(type)) {
      warning("Wu Lab Warning: 'type' parameter was omitted; defaulting to '2x2' cm. Please specify 'type' explicitly (e.g., type = \"2x2\", \"2.58x2\", \"2x4.9\", \"4.9x2\", \"4.9x4.9\") or provide custom dimensions.", call. = FALSE)
      type <- "2x2"
    }
    if (is.null(dims[[type]])) {
      stop("Wu Lab Error: Unknown type. Choose one of: ", paste(names(dims), collapse = ", "))
    }
    pw <- dims[[type]][1]
    ph <- dims[[type]][2]
  }

  # 5. Process Grob Logic (The "Magic Factor" Sizing)
  if (inherits(target_p, "gtable") || inherits(target_p, "grob")) {
    gt <- target_p
    if (!is.null(custom_width) && !is.null(custom_height)) {
      fw <- custom_width
      fh <- custom_height
    } else {
      fw <- grid::convertWidth(sum(gt$widths), "cm", valueOnly = TRUE)
      fh <- grid::convertHeight(sum(gt$heights), "cm", valueOnly = TRUE)
    }
  } else {
    gt <- ggplot2::ggplotGrob(target_p)
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
  }

  # 6. Open Graphics Device
  fw_in <- fw / 2.54
  fh_in <- fh / 2.54

  if (ext == "pdf") {
    grDevices::cairo_pdf(filename = target_filename, width = fw_in, height = fh_in,
                         family = "Arial", bg = "transparent")
  } else if (ext == "png") {
    grDevices::png(filename = target_filename, width = fw_in, height = fh_in,
                   units = "in", res = dpi, bg = "transparent", type = "cairo")
  } else if (ext %in% c("tiff", "tif")) {
    grDevices::tiff(filename = target_filename, width = fw_in, height = fh_in,
                    units = "in", res = dpi, compression = "lzw", type = "cairo")
  }

  # Ensure device always closes even if drawing fails
  on.exit(if (names(dev.cur()) != "null device") grDevices::dev.off(), add = TRUE)

  grid::grid.draw(gt)

  # 7. Success Messaging
  message(sprintf("Successfully saved plot: %s", target_filename))
  message(sprintf("  - Individual panel size: %s x %s cm", pw, ph))
  message(sprintf("  - Total figure size: %.2f x %.2f cm (Format: %s)", fw, fh, toupper(ext)))
}
