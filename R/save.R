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
#' @param match_colorbar (Optional) Logical. If \code{TRUE} (default), automatically scales continuous colorbar legends to match the exact panel dimension (height for vertical colorbars, width for horizontal colorbars) with symmetrical alignment.
#' @param preserve_overflow (Optional) Logical. If \code{TRUE} (default), detects elements exceeding the canvas boundary (such as wide titles or top/bottom legends) and pads outer margins so all elements remain within the artboard for downstream vector editing, while preserving exact data panel dimensions.
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
                       match_colorbar = TRUE,
                       preserve_overflow = TRUE,
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
      warning("Wu Lab Warning: 'type' parameter was omitted; defaulting to '2x2' cm. Please specify 'type' explicitly (e.g., type = \"2x2\", \"2.58x2\", \"2x4.9\", \"4.9x2\", \"4.9x4.9\") or provide custom dimensions (custom_width and custom_height).", call. = FALSE)
      type <- "2x2"
    }
    if (is.null(dims[[type]])) {
      stop(
        "Wu Lab Error: Unknown type '", type, "'. Please choose one of the standard types: ",
        paste(names(dims), collapse = ", "),
        ", or specify 'custom_width' and 'custom_height' (in cm) if standard dimensions are not suitable."
      )
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

    # Match colorbar legend dimensions to panel size
    if (isTRUE(match_colorbar)) {
      gt <- .scale_colorbar_gtable(gt, pw, ph)
    }

    # Preserve overflow elements (legends, wide titles) so they remain editable in Illustrator
    if (isTRUE(preserve_overflow)) {
      gt <- .pad_overflow_gtable(gt)
    }

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

# --- INTERNAL COLORBAR SCALING HELPERS ---

.scale_colorbar_gtable <- function(gt, pw, ph) {
  gbox_indices <- grep("^guide-box", gt$layout$name)
  if (length(gbox_indices) == 0) return(gt)

  for (idx in gbox_indices) {
    gbox_name <- gt$layout$name[idx]
    gbox <- gt$grobs[[idx]]
    if (!inherits(gbox, "gtable")) next

    for (i in seq_along(gbox$grobs)) {
      sub_gt <- gbox$grobs[[i]]
      if (!inherits(sub_gt, "gtable")) next

      sub_gt <- .process_single_guide_gt(sub_gt, gbox_name, pw, ph)
      gbox$grobs[[i]] <- sub_gt
    }

    gt$grobs[[idx]] <- gbox
  }
  return(gt)
}

.process_single_guide_gt <- function(gtbl, gbox_name, pw, ph) {
  bar_idx <- which(gtbl$layout$name == "bar")
  if (length(bar_idx) > 0) {
    bar_row <- gtbl$layout$t[bar_idx]
    bar_col <- gtbl$layout$l[bar_idx]

    is_vertical <- grepl("right|left", gbox_name) || (length(gtbl$heights) > length(gtbl$widths))

    if (is_vertical) {
      gtbl$heights[bar_row] <- grid::unit(ph, "cm")
      top_space <- sum(gtbl$heights[seq_len(bar_row - 1)])
      bot_rows <- (bar_row + 1):length(gtbl$heights)
      if (length(bot_rows) > 0) {
        gtbl$heights[bot_rows[length(bot_rows)]] <- top_space
      } else {
        gtbl <- gtable::gtable_add_rows(gtbl, heights = top_space, pos = -1)
      }
    } else {
      gtbl$widths[bar_col] <- grid::unit(pw, "cm")
      left_space <- sum(gtbl$widths[seq_len(bar_col - 1)])
      right_cols <- (bar_col + 1):length(gtbl$widths)
      if (length(right_cols) > 0) {
        gtbl$widths[right_cols[length(right_cols)]] <- left_space
      } else {
        gtbl <- gtable::gtable_add_cols(gtbl, widths = left_space, pos = -1)
      }
    }
  } else {
    for (j in seq_along(gtbl$grobs)) {
      if (inherits(gtbl$grobs[[j]], "gtable")) {
        gtbl$grobs[[j]] <- .process_single_guide_gt(gtbl$grobs[[j]], gbox_name, pw, ph)
      }
    }
  }
  return(gtbl)
}

# --- INTERNAL OVERFLOW PADDING HELPER ---

.pad_overflow_gtable <- function(gt) {
  col_widths_cm <- vapply(seq_along(gt$widths), function(j) {
    grid::convertWidth(gt$widths[j], "cm", valueOnly = TRUE)
  }, FUN.VALUE = numeric(1))
  row_heights_cm <- vapply(seq_along(gt$heights), function(j) {
    grid::convertHeight(gt$heights[j], "cm", valueOnly = TRUE)
  }, FUN.VALUE = numeric(1))

  col_lefts <- c(0, cumsum(col_widths_cm)[-length(col_widths_cm)])
  col_rights <- cumsum(col_widths_cm)
  total_w <- sum(col_widths_cm)

  row_tops <- c(0, cumsum(row_heights_cm)[-length(row_heights_cm)])
  row_bottoms <- cumsum(row_heights_cm)
  total_h <- sum(row_heights_cm)

  max_overflow_l <- 0
  max_overflow_r <- 0
  max_overflow_t <- 0
  max_overflow_b <- 0

  # Check all grobs except panel, axis, and background
  check_indices <- grep("^panel|^axis|^background", gt$layout$name, invert = TRUE)

  for (i in check_indices) {
    g <- gt$grobs[[i]]
    if (is.null(g) || inherits(g, "zeroGrob")) next

    # Measure width
    gw <- 0
    if (inherits(g, "gtable")) {
      if (!is.null(g$vp$width) && inherits(g$vp$width, "unit")) {
        gw <- grid::convertWidth(g$vp$width, "cm", valueOnly = TRUE)
      } else {
        valid_w <- !grepl("null|npc", grid::unitType(g$widths))
        if (any(valid_w)) gw <- grid::convertWidth(sum(g$widths[valid_w]), "cm", valueOnly = TRUE)
      }
    } else if (inherits(g, "titleGrob") && length(g$children) > 0) {
      gw <- grid::convertWidth(grid::grobWidth(g$children[[1]]), "cm", valueOnly = TRUE)
    } else {
      gw <- tryCatch(grid::convertWidth(grid::grobWidth(g), "cm", valueOnly = TRUE), error = function(e) 0)
    }

    # Measure height
    gh <- 0
    if (inherits(g, "gtable")) {
      if (!is.null(g$vp$height) && inherits(g$vp$height, "unit")) {
        gh <- grid::convertHeight(g$vp$height, "cm", valueOnly = TRUE)
      } else {
        valid_h <- !grepl("null|npc", grid::unitType(g$heights))
        if (any(valid_h)) gh <- grid::convertHeight(sum(g$heights[valid_h]), "cm", valueOnly = TRUE)
      }
    } else if (inherits(g, "titleGrob") && length(g$children) > 0) {
      gh <- grid::convertHeight(grid::grobHeight(g$children[[1]]), "cm", valueOnly = TRUE)
    } else {
      gh <- tryCatch(grid::convertHeight(grid::grobHeight(g), "cm", valueOnly = TRUE), error = function(e) 0)
    }

    l_col <- gt$layout$l[i]
    r_col <- gt$layout$r[i]
    cell_l <- col_lefts[l_col]
    cell_r <- col_rights[r_col]
    cell_mid_x <- (cell_l + cell_r) / 2

    # Check horizontal bounds
    if (gw > (cell_r - cell_l)) {
      hjust <- 0.5
      if (inherits(g, "titleGrob") && length(g$children) > 0 && !is.null(g$children[[1]]$hjust)) {
        hjust <- as.numeric(g$children[[1]]$hjust)
      }
      grob_l <- cell_mid_x - hjust * gw
      grob_r <- cell_mid_x + (1 - hjust) * gw
      if (grob_l < 0) max_overflow_l <- max(max_overflow_l, -grob_l)
      if (grob_r > total_w) max_overflow_r <- max(max_overflow_r, grob_r - total_w)
    }

    t_row <- gt$layout$t[i]
    b_row <- gt$layout$b[i]
    cell_t <- row_tops[t_row]
    cell_b <- row_bottoms[b_row]
    cell_mid_y <- (cell_t + cell_b) / 2

    # Check vertical bounds
    if (gh > (cell_b - cell_t)) {
      vjust <- 0.5
      if (inherits(g, "titleGrob") && length(g$children) > 0 && !is.null(g$children[[1]]$vjust)) {
        vjust <- as.numeric(g$children[[1]]$vjust)
      }
      grob_t <- cell_mid_y - (1 - vjust) * gh
      grob_b <- cell_mid_y + vjust * gh
      if (grob_t < 0) max_overflow_t <- max(max_overflow_t, -grob_t)
      if (grob_b > total_h) max_overflow_b <- max(max_overflow_b, grob_b - total_h)
    }
  }

  # Safety margin of 0.05 cm if overflow occurs
  buffer <- 0.05
  if (max_overflow_l > 0.01) {
    gt <- gtable::gtable_add_cols(gt, widths = grid::unit(max_overflow_l + buffer, "cm"), pos = 0)
  }
  if (max_overflow_r > 0.01) {
    gt <- gtable::gtable_add_cols(gt, widths = grid::unit(max_overflow_r + buffer, "cm"), pos = -1)
  }
  if (max_overflow_t > 0.01) {
    gt <- gtable::gtable_add_rows(gt, heights = grid::unit(max_overflow_t + buffer, "cm"), pos = 0)
  }
  if (max_overflow_b > 0.01) {
    gt <- gtable::gtable_add_rows(gt, heights = grid::unit(max_overflow_b + buffer, "cm"), pos = -1)
  }

  return(gt)
}

