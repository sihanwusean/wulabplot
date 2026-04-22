#' Wu Lab Standardized ggplot2 Theme
#'
#' @description
#' A high-precision theme designed to meet the publication standards of diverse journals.
#' It enforces a 6-pt Arial base font and perfectly scaled 0.5-pt axis lines.
#'
#' @details
#' The theme is optimized for downstream editing in Adobe Illustrator or
#' Affinity Designer. It utilizes a "Magic Factor" (96/72 DPI scaling) to
#' compensate for R's internal point-to-pixel conversion, ensuring that
#' axis lines appear as exactly 0.5 pt in the final vector PDF.
#'
#' Additionally, all background rectangles (plot, panel, and legend) are set
#' to \code{element_blank()} to provide a transparent background, preventing
#' the creation of redundant white boxes that interfere with object selection
#' in vector software.
#'
#' @section Standardized Typography:
#' \itemize{
#'   \item \bold{Axis Titles/Text}: 6 pt Arial
#'   \item \bold{Plot Titles}: 7 pt Bold Arial (centered)
#'   \item \bold{Legend Text}: 6 pt Arial
#'   \item \bold{Facet Strips}: 7 pt Arial
#' }
#'
#' @return A ggplot2 theme object.
#' @importFrom ggplot2 theme_classic theme element_text element_line element_blank unit margin %+replace%
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Apply the theme to a basic plot
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(title = "Figure 1A", x = "Weight", y = "MPG") +
#'   theme_wulab()
#'
#' @export
theme_wulab <- function() {
  # The "Magic Factor":
  # To get 0.5 pt in Illustrator, we need to account for R's internal
  # scaling (96/72.27). 0.5 * (96 / 72.27) / ggplot2::.pt approx 0.233mm
  # A simpler robust way for theme lines to be 0.5pt in Illustrator:
  target_line_pt <- 0.5 * (96 / 72) # Adjusting for DPI scaling
  line_size_mm <- target_line_pt / 2.845276

  ggplot2::theme_classic(base_size = 6, base_family = "Arial") %+replace%
    ggplot2::theme(
      # Text settings
      text = ggplot2::element_text(family = "Arial", size = 6),
      axis.title = ggplot2::element_text(size = 6, color = "black"),
      axis.text = ggplot2::element_text(size = 6, color = "black"),
      legend.text = ggplot2::element_text(size = 6),
      plot.title = ggplot2::element_text(size = 7, face = "bold", hjust = 0.5),

      # Adjusted line weights for exactly 0.5pt in Illustrator
      axis.line = ggplot2::element_line(linewidth = line_size_mm, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = line_size_mm, colour = "black"),

      # REMOVE ALL BACKGROUND BOXES (For clean Illustrator editing)
      panel.background = ggplot2::element_blank(),
      plot.background  = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key       = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),

      # Spacing and Facets
      legend.key.size = ggplot2::unit(0.3, "cm"),
      legend.margin = ggplot2::margin(2, 2, 2, 2),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 7)
    )
}
