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
  "qualitative-pair"  = .qual_hex,
  "qualitative-deep"  = .qual_hex[c(1, 3, 5, 7, 9, 11)],
  "qualitative-light" = .qual_hex[c(2, 4, 6, 8, 10, 12)],
  "umap" = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0',
             '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8',
             '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080'),
  "sequential" = c("#d9ed92", "#52b69a", "#184e77"),
  "diverging"  = c("#bb3e03", "#ffffff", "#0380bb")
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
  if (type %in% c("sequential", "diverging")) {
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

# --- EXPORTED VISUALIZATION FUNCTIONS ---

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
    recommend_msg = "For uni-directional gradients (e.g., TPM values), we recommend native gradient functions, such as:\nscale_fill_gradient2(low = '#d9ed92', mid = '#52b69a', high = '#184e77')"
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

# --- EXPORTED GGPLOT2 SCALES ---

#' @rdname wulab_colors
#' @param type (Required) Character. The palette to use: \code{"qualitative-deep"} (default for color),
#' \code{"qualitative-light"} (default for fill), \code{"qualitative-pair"},
#' \code{"sequential"}, \code{"diverging"}, or \code{"umap"}.
#' @param discrete (Optional) Logical. Use TRUE (default) for factors/characters, FALSE for continuous gradients.
#' @param na.color (Optional) Character. Background/missing data color: \code{"G1"} (lightest),
#' \code{"G2"} (medium, default), \code{"G3"} (darkest), \code{"white"}, or \code{"black"}.
#' @param reverse (Optional) Logical. \code{FALSE} by default. If \code{TRUE}, reverses the palette order.
#' @param ... Other arguments passed to \code{discrete_scale} or \code{scale_fill_gradientn}.
#' @export
scale_color_wulab <- function(type = "qualitative-deep", discrete = TRUE, na.color = "G2", reverse = FALSE, ...) {
  pal_vec <- .get_wulab_pal(type, reverse)
  na_val  <- .get_na_color(na.color)

  if (discrete) {
    ggplot2::discrete_scale("colour", "wulab",
                            palette = .get_pal_fn(type, pal_vec),
                            na.value = na_val, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = pal_vec, na.value = na_val, ...)
  }
}

#' @rdname wulab_colors
#' @export
scale_fill_wulab <- function(type = "qualitative-light", discrete = TRUE, na.color = "G2", reverse = FALSE, ...) {
  pal_vec <- .get_wulab_pal(type, reverse)
  na_val  <- .get_na_color(na.color)

  if (discrete) {
    ggplot2::discrete_scale("fill", "wulab",
                            palette = .get_pal_fn(type, pal_vec),
                            na.value = na_val, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = pal_vec, na.value = na_val, ...)
  }
}
