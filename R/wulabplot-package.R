#' wulabplot: Standardized Scientific Plotting for Journal Publications
#'
#' @description
#' The wulabplot package provides specialized themes and saving functions
#' to ensure figures meet the requirements of high-impact journals.
#' It enforces minimal 6 pt Arial fonts, 0.5 pt line weights, and consistent
#' panel sizing across all lab publications.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{theme_wulab}}: Apply the lab-standard visual theme.
#'   \item \code{\link{save_wulab}}: Export figures with forced panel dimensions.
#'   \item \code{\link{show_wulab_colors}}: View the complete Letter-size publication reference guide for all lab palettes with HEX codes, dual continuous/discrete styles, and optional PDF/PNG export.
#'   \item \code{\link{show_color_qualitative}}: View 12 paired qualitative colors (inspired by traditional Chinese colors) and 3 grey colors.
#'   \item \code{\link{show_color_sequential}}: View sequential colors, from the lightest Creamy Avocado (#d9ed92) to the deepest Moroccan Blue (#184e77), with the midpoint of Teal (#52b69a).
#'   \item \code{\link{show_color_sequential_hc}}: View high-contrast sequential colors, from White (#ffffff) to Moroccan Blue (#184e77).
#'   \item \code{\link{show_color_diverging}}: View diverging colors, from Orange-red (#bb3e03) to Blue-cyan (#0380bb), with a White (#ffffff) midpoint.
#' }
#'
#' @docType package
#' @name wulabplot
#' @import ggplot2
#' @import grid
#' @import gtable
#' @import grDevices
"_PACKAGE"
