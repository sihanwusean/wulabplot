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
  "umap"                    = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0',
                               '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8',
                               '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080'),
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
      real_sc <- do.call(
        ggplot2::discrete_scale,
        c(list(aesthetics = self$aesthetics,
               scale_name = "wulab",
               palette = .get_pal_fn(self$type, pal_vec),
               na.value = na_val), self$args)
      )
    } else {
      if (self$type == "diverging") {
        mid_val  <- if (is.null(self$midpoint)) 0 else self$midpoint
        low_col  <- pal_vec[1]
        mid_col  <- if (length(pal_vec) >= 3) pal_vec[2] else "#ffffff"
        high_col <- if (length(pal_vec) >= 3) pal_vec[3] else pal_vec[length(pal_vec)]

        if ("fill" %in% self$aesthetics) {
          real_sc <- do.call(
            ggplot2::scale_fill_gradient2,
            c(list(low = low_col, mid = mid_col, high = high_col,
                   midpoint = mid_val, na.value = na_val), self$args)
          )
        } else {
          real_sc <- do.call(
            ggplot2::scale_color_gradient2,
            c(list(low = low_col, mid = mid_col, high = high_col,
                   midpoint = mid_val, na.value = na_val), self$args)
          )
        }
      } else {
        if ("fill" %in% self$aesthetics) {
          real_sc <- do.call(
            ggplot2::scale_fill_gradientn,
            c(list(colors = pal_vec, na.value = na_val), self$args)
          )
        } else {
          real_sc <- do.call(
            ggplot2::scale_color_gradientn,
            c(list(colors = pal_vec, na.value = na_val), self$args)
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
#' @param ... Other arguments passed to \code{discrete_scale}, \code{scale_fill_gradient2}, or \code{scale_fill_gradientn}.
#' @export
scale_color_wulab <- function(type = "qualitative-deep", discrete = NULL, na.color = "G2", reverse = FALSE, midpoint = NULL, ...) {
  if (isTRUE(discrete)) {
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    ggplot2::discrete_scale("colour", "wulab",
                            palette = .get_pal_fn(type, pal_vec),
                            na.value = na_val, ...)
  } else if (isFALSE(discrete)) {
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    if (type == "diverging") {
      mid_val  <- if (is.null(midpoint)) 0 else midpoint
      low_col  <- pal_vec[1]
      mid_col  <- if (length(pal_vec) >= 3) pal_vec[2] else "#ffffff"
      high_col <- if (length(pal_vec) >= 3) pal_vec[3] else pal_vec[length(pal_vec)]
      ggplot2::scale_color_gradient2(low = low_col, mid = mid_col, high = high_col,
                                     midpoint = mid_val, na.value = na_val, ...)
    } else {
      ggplot2::scale_color_gradientn(colors = pal_vec, na.value = na_val, ...)
    }
  } else {
    ggplot2::ggproto(NULL, .ScaleWulabAuto, aesthetics = "colour", type = type, na.color = na.color, reverse = reverse, midpoint = midpoint, args = list(...))
  }
}

#' @rdname wulab_colors
#' @export
scale_fill_wulab <- function(type = "qualitative-light", discrete = NULL, na.color = "G2", reverse = FALSE, midpoint = NULL, ...) {
  if (isTRUE(discrete)) {
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    ggplot2::discrete_scale("fill", "wulab",
                            palette = .get_pal_fn(type, pal_vec),
                            na.value = na_val, ...)
  } else if (isFALSE(discrete)) {
    pal_vec <- .get_wulab_pal(type, reverse)
    na_val  <- .get_na_color(na.color)
    if (type == "diverging") {
      mid_val  <- if (is.null(midpoint)) 0 else midpoint
      low_col  <- pal_vec[1]
      mid_col  <- if (length(pal_vec) >= 3) pal_vec[2] else "#ffffff"
      high_col <- if (length(pal_vec) >= 3) pal_vec[3] else pal_vec[length(pal_vec)]
      ggplot2::scale_fill_gradient2(low = low_col, mid = mid_col, high = high_col,
                                    midpoint = mid_val, na.value = na_val, ...)
    } else {
      ggplot2::scale_fill_gradientn(colors = pal_vec, na.value = na_val, ...)
    }
  } else {
    ggplot2::ggproto(NULL, .ScaleWulabAuto, aesthetics = "fill", type = type, na.color = na.color, reverse = reverse, midpoint = midpoint, args = list(...))
  }
}
