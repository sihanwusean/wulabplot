#' Wu Lab Color Palettes and Visualization
#'
#' @description
#' A suite of functions to visualize and select lab-standard color palettes.
#' These palettes are designed for scientific publications,
#' ensuring consistency in experimental group mapping and data gradients.
#'
#' @details
#' The functions in this family provide a visual reference in the RStudio
#' Plots pane while simultaneously printing the exact HEX codes to the
#' console for easy copy-pasting into \code{ggplot2} manual scales.
#'
#' @section Palettes included:
#' \itemize{
#'   \item \bold{Qualitative}: 12 paired colors inspired by traditional Chinese
#'   aesthetics, ideal for categorical groups (e.g., WT vs. Mutant). Includes
#'   3 specialized grey colors for background or missing data.
#'   \item \bold{Sequential}: A multi-hue gradient from Creamy Avocado (#d9ed92)
#'   to Moroccan Blue (#184e77) via a Teal midpoint (#52b69a).
#'   Define the number of colors with the parameter n (Default n = 9).
#'   \item \bold{Diverging}: A transition from Orange-red (#bb3e03) to
#'   Blue-cyan (#0380bb) with a White (#ffffff) midpoint.
#'   Define the number of colors with the parameter n (Default n = 9).
#'   Odd numbers work best for a central midpoint.
#' }
#'
#' @name wulab_colors
NULL

# Internal Helper: Not exported to users
.get_text_col <- function(hex) {
  rgb <- grDevices::col2rgb(hex)
  lum <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
  if (lum > 0.5) "black" else "white"
}

#' @rdname wulab_colors
#' @description \code{show_color_qualitative}: View 12 paired qualitative
#' colors and 3 grey colors.
#' @export
show_color_qualitative <- function() {
  qual_hex <- c("#c3282b", "#f9b1a7", "#1b7cb0", "#5dc9e0", "#08a34a", "#bdd974",
                "#f47521", "#fec773", "#793b96", "#c7a4cd", "#41555e", "#88aca5")
  grey_hex <- c("#f1f0f3", "#c2ccd0", "#808080")

  df <- data.frame(
    hex = c(qual_hex, grey_hex),
    id  = c(as.character(1:12), paste0("G", 1:3)),
    type = factor(c(rep("Data (1-12)", 12), rep("Grey (G1-3)", 3)),
                  levels = c("Grey (G1-3)", "Data (1-12)")),
    x = c(1:12, 1:3)
  )

  df$label_col <- sapply(df$hex, .get_text_col)

  cat("\n--- Wu Lab Qualitative Palette ---\n")
  for(i in 1:12) cat(sprintf("%-3s : %s\n", i, qual_hex[i]))
  cat("\n--- Background Greys ---\n")
  for(i in 1:3) cat(sprintf("G%-2s : %s\n", i, grey_hex[i]))

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = type, fill = hex)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = id, color = label_col),
                       size = 5, fontface = "bold", family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = "Paired Qualitative Palette Reference",
                  subtitle = "Deeper colors for filling and shading. Greys for missing values or background.") +
    ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
}

#' @rdname wulab_colors
#' @description \code{show_color_sequential}: View sequential colors for continuous data.
#' @param n Integer. The number of colors to generate. Defaults to 9.
#' @export
show_color_sequential <- function(n = 9) {
  seq_hex <- grDevices::colorRampPalette(c("#d9ed92", "#52b69a", "#184e77"))(n)
  grey_hex <- c("#f1f0f3", "#c2ccd0", "#808080")

  df <- data.frame(
    hex = c(seq_hex, grey_hex),
    id  = c(as.character(1:n), paste0("G", 1:3)),
    type = factor(c(rep("Sequential (1-n)", n), rep("Background (G1-3)", 3)),
                  levels = c("Background (G1-3)", "Sequential (1-n)")),
    x = c(1:n, 1:3)
  )

  df$label_col <- sapply(df$hex, .get_text_col)

  cat(sprintf("\n--- Wu Lab Sequential Palette (n=%d) ---\n", n))
  for(i in 1:n) cat(sprintf("%-3s : %s\n", i, seq_hex[i]))
  cat("\n--- Background Greys ---\n")
  for(i in 1:3) cat(sprintf("G%-2s : %s\n", i, grey_hex[i]))
  cat("Use for: Serial discrete data (e.g., dosage levels or time points).\n")
  cat("Note: For continuous gradients (e.g., TPM expression), we recommend native gradient functions, such as:\n")
  cat("      scale_fill_gradient2(low = '#d9ed92', mid = '#52b69a', high = '#184e77')\n\n")

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = type, fill = hex)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = id, color = label_col),
                       size = 5, fontface = "bold", family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = paste("Sequential Palette (n =", n, ")"),
                  subtitle = "Change n = [num] for a different range. Greys for missing values or background.") +
    ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
}

#' @rdname wulab_colors
#' @description \code{show_color_diverging}: View diverging colors for centered data.
#' @param n Integer. Number of colors. Odd numbers recommended.
#' @export
show_color_diverging <- function(n = 9) {
  div_hex <- grDevices::colorRampPalette(c("#bb3e03", "#ffffff", "#0380bb"))(n)
  grey_hex <- c("#f1f0f3", "#c2ccd0", "#808080")

  df <- data.frame(
    hex = c(div_hex, grey_hex),
    id  = c(as.character(1:n), paste0("G", 1:3)),
    type = factor(c(rep("Diverging (1-n)", n), rep("Background (G1-3)", 3)),
                  levels = c("Background (G1-3)", "Diverging (1-n)")),
    x = c(1:n, 1:3)
  )

  df$label_col <- sapply(df$hex, .get_text_col)

  cat(sprintf("\n--- Wu Lab Diverging Palette (n=%d) ---\n", n))
  for(i in 1:n) cat(sprintf("%-3s : %s\n", i, div_hex[i]))
  cat("\n--- Background Greys ---\n")
  for(i in 1:3) cat(sprintf("G%-2s : %s\n", i, grey_hex[i]))
  cat("Use for: Contrasting discrete data (e.g., different degrees of inhibition vs. activation).\n")
  cat("Note: For continuous gradients (e.g., gene expression heatmaps), we recommend native gradient functions, such as:\n")
  cat("      scale_fill_gradient2(low = '#bb3e03', mid = '#ffffff', high = '#0380bb')\n\n")

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = type, fill = hex)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = id, color = label_col),
                       size = 5, fontface = "bold", family = "Arial") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = paste("Diverging Palette (n =", n, ")"),
                  subtitle = "Change n = [num] for a different range. Greys for missing values or background.") +
    ggplot2::theme_minimal(base_family = "Arial") +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold"),
      plot.title = ggplot2::element_text(size = 12, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
}
