#' Wu Lab Color Palettes and Visualization
#'
#' @description
#' A suite of functions to visualize and select lab-standard color palettes.
#' These palettes are designed for scientific publications,
#' ensuring consistency in experimental group mapping and data gradients.
#'
#' @details
#' The show_color_...() functions provide a visual reference in the RStudio
#' Plots pane while simultaneously printing the exact HEX codes to the
#' console for easy copy-pasting into \code{ggplot2} manual scales.
#'
#' The scale_..._wulab() functions apply pre-defined color palettes to the
#' ggplot2 object.
#'
#' @section Palettes included:
#' \itemize{
#'   \item \bold{qualitative-pair}: 12 paired colors (Deep/Light pairs) inspired
#'   by traditional Chinese aesthetics.
#'   \item \bold{qualitative-deep}: The 6 deeper shades (odd positions) from
#'   the paired palette. Best for points and lines.
#'   \item \bold{qualitative-light}: The 6 lighter shades (even positions) from
#'   the paired palette. Best for bar fills and violin areas.
#'   \item \bold{sequential}: A Creamy Avocado (#d9ed92) to Moroccan Blue (#184e77)
#'   gradient for serial discrete data.
#'   \item \bold{sequential-highcontrast} (or \bold{sequential-hc}): A White (#ffffff)
#'   to Moroccan Blue (#184e77) high-contrast gradient for unidirectional heatmaps and continuous data.
#'   \item \bold{diverging}: An Orange-red (#bb3e03) to Blue-cyan (#0380bb)
#'   transition via a White (#ffffff) midpoint.
#'   \item \bold{umap}: Sasha Trubetskoy's 20-color palette, optimized for
#'   high-contrast UMAP cluster visualization (e.g., Seurat/Scanpy).
#' }
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn scale_fill_gradientn
#'
#' @name wulab_colors
NULL

# --- INTERNAL HELPERS ---

# Internal Helper: Determine text color (black/white) based on background HEX for readability
.get_text_col <- function(hex) {
  rgb <- grDevices::col2rgb(hex)
  lum <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
  if (lum > 0.5) "black" else "white"
}

# Internal Palette Storage
.qual_hex <- c("#c3282b", "#f9b1a7", "#1b7cb0", "#5dc9e0", "#08a34a", "#bdd974",
               "#f47521", "#fec773", "#793b96", "#c7a4cd", "#41555e", "#88aca5")

.wulab_palettes <- list(
  "qualitative-pair"        = .qual_hex,
  "qualitative-deep"        = .qual_hex[c(1, 3, 5, 7, 9, 11)],
  "qualitative-light"       = .qual_hex[c(2, 4, 6, 8, 10, 12)],
  "umap"                    = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4',
                               '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff', '#9a6324', '#fffac8',
                               '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9'),
  "sequential"              = c("#d9ed92", "#52b69a", "#184e77"),
  "sequential-highcontrast" = c("#ffffff", "#d9ed92", "#52b69a", "#184e77"),
  "sequential-hc"           = c("#ffffff", "#d9ed92", "#52b69a", "#184e77"),
  "diverging"               = c("#bb3e03", "#ffffff", "#0380bb")
)

# Internal Helper: Get raw palette vectors
.get_wulab_pal <- function(type = "qualitative-pair", reverse = FALSE) {
  # match.arg ensures 'type' exists in our list names
  type <- match.arg(type, names(.wulab_palettes))

  pal <- .wulab_palettes[[type]]

  if (reverse) pal <- rev(pal)
  return(pal)
}

# Internal Helper: Convert named lab greys to HEX
.get_na_color <- function(na.color) {
  switch(na.color,
         "G1"    = "#f1f0f3",
         "G2"    = "#c2ccd0",
         "G3"    = "#808080",
         "white" = "#ffffff",
         "black" = "#000000",
         na.color # Returns the input if it's already a HEX or standard R color name
  )
}

# Internal Helper: Select and handle the palette function
.get_pal_fn <- function(type, pal_vec) {
  if (type %in% c("sequential", "sequential-highcontrast", "sequential-hc", "diverging")) {
    # For gradients: Always interpolate to the requested 'n'
    function(n) grDevices::colorRampPalette(pal_vec)(n)
  } else {
    # For qualitative/UMAP: Pick exact colors or recycle if n is too large
    function(n) {
      if (n > length(pal_vec)) {
        warning(sprintf(
          "Wu Lab Palette Alert: Requested %d colors, but only %d available in '%s'. Colors will be recycled.",
          n, length(pal_vec), type
        ))
        # Recycle the palette to match the required length
        pal_vec <- rep(pal_vec, length.out = n)
      }
      pal_vec[1:n]
    }
  }
}

# The Plotting Engine
.plot_wulab_ref <- function(hex, palette_name, usage_msg = "", recommend_msg = "", show_greys = TRUE) {

  n <- length(hex)

  # --- CONSOLE PRINTING ---
  cat(sprintf("\n--- Wu Lab %s Palette ---\n", palette_name))
  for(i in 1:n) cat(sprintf("%-3s : %s\n", i, hex[i]))

  if(show_greys) {
    grey_hex <- c("#f1f0f3", "#c2ccd0", "#808080")
    cat("\n--- Background Greys ---\n")
    for(i in 1:3) cat(sprintf("G%-2s : %s\n", i, grey_hex[i]))
  }

  if(usage_msg != "") cat(paste0("Use for: ", usage_msg, "\n"))
  if(recommend_msg != "") cat(paste0("Note: ", recommend_msg, "\n"))

  # --- DATA PREPARATION ---

  # Build core data
  df <- data.frame(
    hex  = hex,
    id   = as.character(1:n),
    type = "Data",
    x    = 1:n,
    stringsAsFactors = FALSE
  )

  # Attach greys if requested
  if (show_greys) {
    df_grey <- data.frame(
      hex  = c("#f1f0f3", "#c2ccd0", "#808080"),
      id   = paste0("G", 1:3),
      type = "Grey",
      x    = 1:3,
      stringsAsFactors = FALSE
    )
    df <- rbind(df, df_grey)
  }

  # Final formatting
  df$type      <- factor(df$type, levels = c("Grey", "Data"))
  # Using vapply for type-safe color determination
  df$label_col <- vapply(df$hex, .get_text_col, FUN.VALUE = character(1))

  # --- GGPLOT ---
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = type, fill = hex)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = id, color = label_col),
                       size = 5, fontface = "bold", family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = paste(palette_name, "Reference"),
                  subtitle = paste0(usage_msg, " ", recommend_msg)) +
    ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 8)
    )
}

#' @rdname wulab_colors
#' @param filename (Optional) Character. Output filename (e.g. \code{"wulab_palettes.pdf"}) to export the Letter-size reference guide to disk.
#' @param dpi (Optional) Numeric. Resolution for raster exports (default = 300).
#' @export
show_wulab_colors <- function(filename = NULL, dpi = 300) {
  
  # Palette definitions
  q_deep    <- .get_wulab_pal("qualitative-deep")
  q_light   <- .get_wulab_pal("qualitative-light")
  umap_hex  <- .get_wulab_pal("umap")
  seq_pal   <- .get_wulab_pal("sequential")
  seq_hc    <- .get_wulab_pal("sequential-highcontrast")
  div_pal   <- .get_wulab_pal("diverging")
  greys_hex <- c("G1" = "#f1f0f3", "G2" = "#c2ccd0", "G3" = "#808080")
  
  # Console Print Summary
  cat("\n=======================================================\n")
  cat("          WU LAB STANDARDIZED COLOR PALETTES          \n")
  cat("=======================================================\n\n")
  
  cat("--- QUALITATIVE - PAIRED (DEEP / LIGHT) ---\n")
  cat(paste(sprintf("[P%d] Deep: %s / Light: %s", 1:6, toupper(q_deep), toupper(q_light)), collapse = "\n"), "\n\n")
  
  cat("--- UMAP 20-COLOR ---\n")
  cat(paste(sprintf("[C%02d] %s", 1:20, toupper(umap_hex)), collapse = "  "), "\n\n")
  
  cat("--- BACKGROUND GREYS ---\n")
  cat(paste(sprintf("[%s] %s", names(greys_hex), toupper(unname(greys_hex))), collapse = "  "), "\n\n")
  
  cat("--- SEQUENTIAL ---\n")
  cat("Key Stops:", paste(toupper(seq_pal), collapse = " -> "), "\n\n")
  
  cat("--- SEQUENTIAL HIGH-CONTRAST ---\n")
  cat("Key Stops:", paste(toupper(seq_hc), collapse = " -> "), "\n\n")
  
  cat("--- DIVERGING ---\n")
  cat("Key Stops:", paste(toupper(div_pal), collapse = " -> "), "\n")
  cat("=======================================================\n\n")
  
  # Standardized uniform subplot theme across all 6 subplots
  sub_theme <- theme_wulab() +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 9.0, face = "bold", hjust = 0, margin = ggplot2::margin(b = 3)),
      plot.subtitle = ggplot2::element_text(size = 7.8, color = "#005f73", face = "italic", hjust = 0, margin = ggplot2::margin(b = 3))
    )

  # Normalized X coordinate system (0 to 100)
  X_START <- 25
  X_END   <- 100
  W_TOTAL <- X_END - X_START # 75

  make_tiles <- function(n, y_val, labels, hex_vec) {
    w_item <- W_TOTAL / n
    x_centers <- X_START + (1:n - 0.5) * w_item
    df <- data.frame(
      id = 1:n,
      label = labels,
      hex = toupper(hex_vec),
      x = x_centers,
      y = y_val,
      w = w_item * 0.92,
      stringsAsFactors = FALSE
    )
    df$text_col <- vapply(df$hex, .get_text_col, FUN.VALUE = character(1))
    return(df)
  }

  # --- Subplot 1: Qualitative Paired (6 items) ---
  df_q1 <- make_tiles(6, 2, paste0("P", 1:6, " Deep"), q_deep)
  df_q2 <- make_tiles(6, 1, paste0("P", 1:6, " Light"), q_light)
  df_qual <- rbind(df_q1, df_q2)

  p1 <- ggplot2::ggplot(df_qual) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex, width = w), color = "black", linewidth = 0.2, height = 0.55) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y + 0.08, label = label, color = text_col), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y - 0.12, label = hex, color = text_col), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0.3, 2.5), expand = c(0,0)) +
    ggplot2::labs(
      title = "Qualitative - Paired Colors (Deep for Points/Lines, Light for Fills)",
      subtitle = "Usage: scale_color_wulab(type = \"qualitative-deep\") | scale_fill_wulab(type = \"qualitative-light\")"
    ) +
    sub_theme

  # --- Subplot 2: UMAP 20-Color (10 items per row) ---
  df_u1 <- make_tiles(10, 2, paste0("C", 1:10), umap_hex[1:10])
  df_u2 <- make_tiles(10, 1, paste0("C", 11:20), umap_hex[11:20])
  df_umap <- rbind(df_u1, df_u2)

  p2 <- ggplot2::ggplot(df_umap) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex, width = w), color = "black", linewidth = 0.2, height = 0.55) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y + 0.08, label = label, color = text_col), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y - 0.12, label = hex, color = text_col), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0.3, 2.5), expand = c(0,0)) +
    ggplot2::labs(
      title = "UMAP 20-Color Cluster Palette",
      subtitle = "Usage: scale_color_wulab(type = \"umap\") | scale_fill_wulab(type = \"umap\")"
    ) +
    sub_theme

  # --- Subplot 3: Background Greys (3 tiles matching Qualitative tile width/height) ---
  # y=1.5 = center of shared y-range c(0.3,2.5); same height=0.55 as all other tiles
  w_q_tile <- W_TOTAL / 6
  df_greys <- data.frame(
    name = names(greys_hex),
    hex = toupper(unname(greys_hex)),
    x = X_START + (1:3 - 0.5) * w_q_tile,
    y = 1.5,
    w = w_q_tile * 0.92,
    stringsAsFactors = FALSE
  )
  df_greys$text_col <- vapply(df_greys$hex, .get_text_col, FUN.VALUE = character(1))

  p3 <- ggplot2::ggplot(df_greys) +
    ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = hex, width = w), color = "black", linewidth = 0.2, height = 0.55) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y + 0.08, label = name, color = text_col), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y - 0.12, label = hex, color = text_col), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0.3, 2.5), expand = c(0,0)) +
    ggplot2::labs(
      title = "Standardized Background Greys (Missing / NA Value Fills)",
      subtitle = "Usage: scale_fill_wulab(..., na.color = \"G1\")  [Options: \"G1\" (#F1F0F3), \"G2\" (#C2CCD0), \"G3\" (#808080)]"
    ) +
    sub_theme

  # --- Subplot 4: Sequential Palette ---
  # Colorbar: n=200 tiles each of width dx; centers offset by +dx/2 from cb_left so
  # left edge of tile-1 = cb_left exactly, right edge of tile-200 = cb_right exactly.
  # cb_left/right = outer edges of the discrete bins (4% half-gap of tile width on each side)
  seq_n_disc <- 9
  seq_w_item  <- W_TOTAL / seq_n_disc
  seq_cb_left  <- X_START + 0.04 * seq_w_item
  seq_cb_right <- X_END   - 0.04 * seq_w_item
  seq_cb_dx    <- (seq_cb_right - seq_cb_left) / 200
  seq_cols     <- grDevices::colorRampPalette(seq_pal)(200)
  df_seq_cont  <- data.frame(
    x   = seq_cb_left + (seq_len(200) - 0.5) * seq_cb_dx,
    y   = 2.0,
    col = seq_cols,
    w   = seq_cb_dx
  )
  df_seq_disc <- make_tiles(seq_n_disc, 1.0, "", grDevices::colorRampPalette(seq_pal)(seq_n_disc))

  p4 <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = df_seq_cont, ggplot2::aes(x = x, y = y, fill = col, width = w), height = 0.55) +
    ggplot2::annotate("rect", xmin = seq_cb_left, xmax = seq_cb_right, ymin = 2.0 - 0.275, ymax = 2.0 + 0.275, fill = NA, color = "black", linewidth = 0.2) +
    ggplot2::geom_tile(data = df_seq_disc, ggplot2::aes(x = x, y = y, fill = hex, width = w), color = "black", linewidth = 0.2, height = 0.55) +
    ggplot2::geom_text(data = df_seq_disc, ggplot2::aes(x = x, y = y - 0.38, label = hex), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::annotate("text", x = X_START - 1.5, y = 2.0, label = "Continuous Gradient:", size = 2.2, fontface = "bold", hjust = 1, family = "Arial") +
    ggplot2::annotate("text", x = X_START - 1.5, y = 1.0, label = "Discrete Bins (n=9, customizable via n):", size = 2.2, fontface = "bold", hjust = 1, family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0.3, 2.5), expand = c(0,0)) +
    ggplot2::labs(
      title = "Sequential Palette (Creamy Avocado to Moroccan Blue)",
      subtitle = "Usage: scale_fill_wulab(type = \"sequential\") | scale_color_wulab(type = \"sequential\")"
    ) +
    sub_theme

  # --- Subplot 5: Sequential High-Contrast Palette ---
  seq_hc_n_disc  <- 9
  seq_hc_w_item  <- W_TOTAL / seq_hc_n_disc
  seq_hc_cb_left  <- X_START + 0.04 * seq_hc_w_item
  seq_hc_cb_right <- X_END   - 0.04 * seq_hc_w_item
  seq_hc_cb_dx    <- (seq_hc_cb_right - seq_hc_cb_left) / 200
  seq_hc_cols     <- grDevices::colorRampPalette(seq_hc)(200)
  df_seq_hc_cont  <- data.frame(
    x   = seq_hc_cb_left + (seq_len(200) - 0.5) * seq_hc_cb_dx,
    y   = 2.0,
    col = seq_hc_cols,
    w   = seq_hc_cb_dx
  )
  df_seq_hc_disc <- make_tiles(seq_hc_n_disc, 1.0, "", grDevices::colorRampPalette(seq_hc)(seq_hc_n_disc))

  p5 <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = df_seq_hc_cont, ggplot2::aes(x = x, y = y, fill = col, width = w), height = 0.55) +
    ggplot2::annotate("rect", xmin = seq_hc_cb_left, xmax = seq_hc_cb_right, ymin = 2.0 - 0.275, ymax = 2.0 + 0.275, fill = NA, color = "black", linewidth = 0.2) +
    ggplot2::geom_tile(data = df_seq_hc_disc, ggplot2::aes(x = x, y = y, fill = hex, width = w), color = "black", linewidth = 0.2, height = 0.55) +
    ggplot2::geom_text(data = df_seq_hc_disc, ggplot2::aes(x = x, y = y - 0.38, label = hex), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::annotate("text", x = X_START - 1.5, y = 2.0, label = "Continuous Gradient:", size = 2.2, fontface = "bold", hjust = 1, family = "Arial") +
    ggplot2::annotate("text", x = X_START - 1.5, y = 1.0, label = "Discrete Bins (n=9, customizable via n):", size = 2.2, fontface = "bold", hjust = 1, family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0.3, 2.5), expand = c(0,0)) +
    ggplot2::labs(
      title = "Sequential High-Contrast Palette (Pure White #FFFFFF to Moroccan Blue)",
      subtitle = "Usage: scale_fill_wulab(type = \"sequential-hc\") [Ideal for unidirectional heatmaps with white baseline]"
    ) +
    sub_theme

  # --- Subplot 6: Diverging Palette ---
  div_n_disc  <- 9
  div_w_item  <- W_TOTAL / div_n_disc
  div_cb_left  <- X_START + 0.04 * div_w_item
  div_cb_right <- X_END   - 0.04 * div_w_item
  div_cb_dx    <- (div_cb_right - div_cb_left) / 200
  div_cols     <- grDevices::colorRampPalette(div_pal)(200)
  df_div_cont  <- data.frame(
    x   = div_cb_left + (seq_len(200) - 0.5) * div_cb_dx,
    y   = 2.0,
    col = div_cols,
    w   = div_cb_dx
  )
  df_div_disc <- make_tiles(div_n_disc, 1.0, "", grDevices::colorRampPalette(div_pal)(div_n_disc))

  p6 <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = df_div_cont, ggplot2::aes(x = x, y = y, fill = col, width = w), height = 0.55) +
    ggplot2::annotate("rect", xmin = div_cb_left, xmax = div_cb_right, ymin = 2.0 - 0.275, ymax = 2.0 + 0.275, fill = NA, color = "black", linewidth = 0.2) +
    ggplot2::geom_tile(data = df_div_disc, ggplot2::aes(x = x, y = y, fill = hex, width = w), color = "black", linewidth = 0.2, height = 0.55) +
    ggplot2::geom_text(data = df_div_disc, ggplot2::aes(x = x, y = y - 0.38, label = hex), size = 2.0, fontface = "bold", family = "Arial") +
    ggplot2::annotate("text", x = X_START - 1.5, y = 2.0, label = "Continuous Gradient:", size = 2.2, fontface = "bold", hjust = 1, family = "Arial") +
    ggplot2::annotate("text", x = X_START - 1.5, y = 1.0, label = "Discrete Bins (n=9, customizable via n):", size = 2.2, fontface = "bold", hjust = 1, family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0.3, 2.5), expand = c(0,0)) +
    ggplot2::labs(
      title = "Diverging Palette (Orange-Red to Blue-Cyan via White #FFFFFF Midpoint)",
      subtitle = "Usage: scale_fill_wulab(type = \"diverging\", midpoint = 0) | scale_color_wulab(type = \"diverging\", midpoint = 0)"
    ) +
    sub_theme

  # Stack all 6 subplots vertically using equal panel heights.
  # All subplots share y-limits c(0.3, 2.5) and height=0.55 so tiles are
  # physically identical in size across every palette row.
  grobs <- list(ggplot2::ggplotGrob(p1), ggplot2::ggplotGrob(p2), ggplot2::ggplotGrob(p3),
                ggplot2::ggplotGrob(p4), ggplot2::ggplotGrob(p5), ggplot2::ggplotGrob(p6))

  max_width <- do.call(grid::unit.pmax, lapply(grobs, function(g) g$widths))
  for (i in seq_along(grobs)) {
    grobs[[i]]$widths <- max_width
  }

  stacked_gt <- do.call(rbind, grobs)

  # Create standalone top title grob for the entire page
  title_grob <- grid::textGrob(
    "Wu Lab Standardized Color Palettes",
    gp = grid::gpar(fontsize = 13, fontface = "bold", fontfamily = "Arial")
  )
  
  # Insert top title into gtable layout
  final_gtable <- gtable::gtable_add_rows(stacked_gt, heights = grid::unit(0.4, "in"), pos = 0)
  final_gtable <- gtable::gtable_add_grob(
    final_gtable,
    title_grob,
    t = 1, l = 1, r = ncol(final_gtable),
    clip = "off", name = "main_page_title"
  )

  # Add 0.5 inch page margins around the entire Letter page
  final_gtable <- gtable::gtable_add_padding(final_gtable, grid::unit(c(0.4, 0.5, 0.5, 0.5), "in"))

  # Draw directly to active graphics device (e.g. RStudio Plots pane)
  tryCatch({
    grid::grid.newpage()
    grid::grid.draw(final_gtable)
  }, error = function(e) invisible(NULL))

  if (!is.null(filename)) {
    save_wulab(filename, final_gtable, custom_width = 21.59, custom_height = 27.94, dpi = dpi)
  }

  invisible(final_gtable)
}

#' @rdname wulab_colors
#' @export
show_color_qualitative <- function() {
  .plot_wulab_ref(
    hex = .get_wulab_pal("qualitative-pair"),
    palette_name = "Paired Qualitative",
    usage_msg = "Categorical groups (e.g., WT vs. Mutant).",
    recommend_msg = "Deeper colors for lines and points. Lighter colors for fills and bars."
  )
}

#' @rdname wulab_colors
#' @param n (Required) Numeric. Number of colors to display (default = 9).
#' @export
show_color_sequential <- function(n = 9) {
  .plot_wulab_ref(
    hex = grDevices::colorRampPalette(.get_wulab_pal("sequential"))(n),
    palette_name = paste0("Sequential (n=", n, ")"),
    usage_msg = "Serial discrete data (e.g., dosage levels or time points).",
    recommend_msg = "For the most accurate sequential gradient, we recommend a strictly monotonic and linear palette, such as:\nscale_fill_gradient2(low = '#ffffff', mid = '#8ba6bb', high = '#184e77')"
  )
}

#' @rdname wulab_colors
#' @param n (Required) Numeric. Number of colors to display (default = 9).
#' @export
show_color_sequential_hc <- function(n = 9) {
  .plot_wulab_ref(
    hex = grDevices::colorRampPalette(.get_wulab_pal("sequential-highcontrast"))(n),
    palette_name = paste0("Sequential High-Contrast (n=", n, ")"),
    usage_msg = "Unidirectional heatmaps and continuous expression gradients with white zero-baseline.",
    recommend_msg = "For the most accurate sequential gradient, we recommend a strictly monotonic and linear palette, such as:\nscale_fill_gradient2(low = '#ffffff', mid = '#8ba6bb', high = '#184e77')"
  )
}

#' @rdname wulab_colors
#' @param n (Required) Numeric. Number of colors to display (default = 9).
#' @export
show_color_diverging <- function(n = 9) {
  .plot_wulab_ref(
    hex = grDevices::colorRampPalette(.get_wulab_pal("diverging"))(n),
    palette_name = paste0("Diverging (n=", n, ")"),
    usage_msg = "Contrasting discrete data (e.g., inhibition vs. activation).",
    recommend_msg = "For bi-directional gradients (e.g., heatmaps), we recommend native gradient functions, such as:\nscale_fill_gradient2(low = '#bb3e03', mid = '#ffffff', high = '#0380bb')."
  )
}

#' @rdname wulab_colors
#' @export
show_color_umap <- function() {
  .plot_wulab_ref(
    hex = .get_wulab_pal("umap"),
    palette_name = "UMAP 20-Color",
    usage_msg = "Discrete clusters (e.g., Seurat/Scanpy output).",
    show_greys = FALSE
  )
}

# Internal Helper: Auto-detecting GGProto Scale
.ScaleWulabAuto <- ggplot2::ggproto(
  "ScaleWulabAuto",
  ggplot2::Scale,
  aesthetics = "colour",
  type = "qualitative-deep",
  na.color = "G2",
  reverse = FALSE,
  midpoint = NULL,
  limits = NULL,
  oob = scales::squish,
  args = list(),
  actual_scale = NULL,

  init_scale = function(self, df) {
    if (!is.null(self$actual_scale)) return()
    if (is.null(df) || nrow(df) == 0) return()
    aesthetics <- intersect(self$aesthetics, names(df))
    if (length(aesthetics) == 0) return()

    x <- df[[aesthetics[1]]]
    is_disc <- !is.numeric(x)

    pal_vec <- .get_wulab_pal(self$type, self$reverse)
    na_val  <- .get_na_color(self$na.color)

    if (is_disc) {
      if (!is.null(self$limits)) {
        warning("limits parameter is intended for continuous scales and is ignored for discrete data.")
      }
      real_sc <- do.call(
        ggplot2::discrete_scale,
        c(list(aesthetics = self$aesthetics,
               palette = .get_pal_fn(self$type, pal_vec),
               na.value = na_val), self$args)
      )
    } else {
      scale_args <- self$args
      if (!is.null(self$limits)) {
        if (!"limits" %in% names(scale_args)) scale_args$limits <- self$limits
        if (!"oob" %in% names(scale_args)) scale_args$oob <- self$oob
      }

      if (self$type == "diverging") {
        mid_val  <- if (is.null(self$midpoint)) 0 else self$midpoint
        low_col  <- pal_vec[1]
        mid_col  <- if (length(pal_vec) >= 3) pal_vec[2] else "#ffffff"
        high_col <- if (length(pal_vec) >= 3) pal_vec[3] else pal_vec[length(pal_vec)]

        if ("fill" %in% self$aesthetics) {
          real_sc <- do.call(
            ggplot2::scale_fill_gradient2,
            c(list(low = low_col, mid = mid_col, high = high_col,
                   midpoint = mid_val, na.value = na_val), scale_args)
          )
        } else {
          real_sc <- do.call(
            ggplot2::scale_color_gradient2,
            c(list(low = low_col, mid = mid_col, high = high_col,
                   midpoint = mid_val, na.value = na_val), scale_args)
          )
        }
      } else {
        if ("fill" %in% self$aesthetics) {
          real_sc <- do.call(
            ggplot2::scale_fill_gradientn,
            c(list(colors = pal_vec, na.value = na_val), scale_args)
          )
        } else {
          real_sc <- do.call(
            ggplot2::scale_color_gradientn,
            c(list(colors = pal_vec, na.value = na_val), scale_args)
          )
        }
      }
    }
    self$actual_scale <- real_sc
    self$guide <- real_sc$guide
  },

  clone = function(self) {
    new <- ggplot2::ggproto(NULL, self)
    if (!is.null(self$actual_scale)) {
      new$actual_scale <- self$actual_scale$clone()
    }
    new
  },

  transform_df = function(self, df, ...) {
    self$init_scale(df)
    if (!is.null(self$actual_scale)) self$actual_scale$transform_df(df, ...) else df
  },

  transform = function(self, x, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$transform(x, ...) else x
  },

  train_df = function(self, df, ...) {
    self$init_scale(df)
    if (!is.null(self$actual_scale)) self$actual_scale$train_df(df, ...)
  },

  train = function(self, x, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$train(x, ...)
  },

  map_df = function(self, df, ...) {
    self$init_scale(df)
    if (!is.null(self$actual_scale)) self$actual_scale$map_df(df, ...) else df
  },

  map = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$map(...) else NULL
  },

  is_discrete = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$is_discrete(...) else TRUE
  },

  is_empty = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$is_empty(...) else FALSE
  },

  dimension = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$dimension(...) else c(0, 1)
  },

  get_breaks = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$get_breaks(...) else NULL
  },

  get_labels = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$get_labels(...) else NULL
  },

  get_limits = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$get_limits(...) else NULL
  },

  break_info = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$break_info(...)
  },

  make_title = function(self, ...) {
    if (!is.null(self$actual_scale)) self$actual_scale$make_title(...) else NULL
  }
)

# --- EXPORTED GGPLOT2 SCALES ---

#' @rdname wulab_colors
#' @param type (Required) Character. The palette to use: \code{"qualitative-deep"} (default for color),
#' \code{"qualitative-light"} (default for fill), \code{"qualitative-pair"},
#' \code{"sequential"}, \code{"sequential-highcontrast"} (or \code{"sequential-hc"}),
#' \code{"diverging"}, or \code{"umap"}.
#' @param discrete (Optional) Logical or NULL. If \code{NULL} (default), automatically detects whether data is discrete or continuous. Use \code{TRUE} for factors/characters or \code{FALSE} for continuous gradients.
#' @param na.color (Optional) Character. Background/missing data color: \code{"G1"} (lightest),
#' \code{"G2"} (medium, default), \code{"G3"} (darkest), \code{"white"}, or \code{"black"}.
#' @param reverse (Optional) Logical. \code{FALSE} by default. If \code{TRUE}, reverses the palette order.
#' @param midpoint (Optional) Numeric or NULL. Midpoint value for continuous diverging scales (defaults to \code{0} when \code{type = "diverging"}). Ignored for non-diverging scales.
#' @param limits (Optional) Numeric vector of length 2. Defines the continuous data range limits. Values outside these limits will automatically be squished to the minimum/maximum palette colors (via \code{oob = scales::squish}). Ignored with a warning for discrete scales.
#' @param oob (Optional) Function. Out-of-bounds handling function for continuous scales. Defaults to \code{scales::squish}.
#' @param ... Other arguments passed to \code{discrete_scale}, \code{scale_fill_gradient2}, or \code{scale_fill_gradientn}.
#' @export
scale_color_wulab <- function(type = "qualitative-deep", discrete = NULL, na.color = "G2", reverse = FALSE, midpoint = NULL, limits = NULL, oob = scales::squish, ...) {
  if (isTRUE(discrete)) {
    if (!is.null(limits)) {
      warning("limits parameter is intended for continuous scales and is ignored for discrete data.")
    }
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    ggplot2::discrete_scale(aesthetics = "colour",
                            palette = .get_pal_fn(type, pal_vec),
                            na.value = na_val, ...)
  } else if (isFALSE(discrete)) {
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    scale_args <- list(...)
    if (!is.null(limits)) {
      if (!"limits" %in% names(scale_args)) scale_args$limits <- limits
      if (!"oob" %in% names(scale_args)) scale_args$oob <- oob
    }

    if (type == "diverging") {
      mid_val  <- if (is.null(midpoint)) 0 else midpoint
      low_col  <- pal_vec[1]
      mid_col  <- if (length(pal_vec) >= 3) pal_vec[2] else "#ffffff"
      high_col <- if (length(pal_vec) >= 3) pal_vec[3] else pal_vec[length(pal_vec)]
      do.call(ggplot2::scale_color_gradient2,
              c(list(low = low_col, mid = mid_col, high = high_col,
                     midpoint = mid_val, na.value = na_val), scale_args))
    } else {
      do.call(ggplot2::scale_color_gradientn,
              c(list(colors = pal_vec, na.value = na_val), scale_args))
    }
  } else {
    ggplot2::ggproto(NULL, .ScaleWulabAuto, aesthetics = "colour", type = type, na.color = na.color, reverse = reverse, midpoint = midpoint, limits = limits, oob = oob, args = list(...))
  }
}

#' @rdname wulab_colors
#' @export
scale_fill_wulab <- function(type = "qualitative-light", discrete = NULL, na.color = "G2", reverse = FALSE, midpoint = NULL, limits = NULL, oob = scales::squish, ...) {
  if (isTRUE(discrete)) {
    if (!is.null(limits)) {
      warning("limits parameter is intended for continuous scales and is ignored for discrete data.")
    }
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    ggplot2::discrete_scale(aesthetics = "fill",
                            palette = .get_pal_fn(type, pal_vec),
                            na.value = na_val, ...)
  } else if (isFALSE(discrete)) {
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    scale_args <- list(...)
    if (!is.null(limits)) {
      if (!"limits" %in% names(scale_args)) scale_args$limits <- limits
      if (!"oob" %in% names(scale_args)) scale_args$oob <- oob
    }

    if (type == "diverging") {
      mid_val  <- if (is.null(midpoint)) 0 else midpoint
      low_col  <- pal_vec[1]
      mid_col  <- if (length(pal_vec) >= 3) pal_vec[2] else "#ffffff"
      high_col <- if (length(pal_vec) >= 3) pal_vec[3] else pal_vec[length(pal_vec)]
      do.call(ggplot2::scale_fill_gradient2,
              c(list(low = low_col, mid = mid_col, high = high_col,
                     midpoint = mid_val, na.value = na_val), scale_args))
    } else {
      do.call(ggplot2::scale_fill_gradientn,
              c(list(colors = pal_vec, na.value = na_val), scale_args))
    }
  } else {
    ggplot2::ggproto(NULL, .ScaleWulabAuto, aesthetics = "fill", type = type, na.color = na.color, reverse = reverse, midpoint = midpoint, limits = limits, oob = oob, args = list(...))
  }
}
