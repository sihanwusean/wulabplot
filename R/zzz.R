.onLoad <- function(libname, pkgname) {
  # Quietly register Arial in the R font database if running on Windows
  if (.Platform$OS.type == "windows") {
    # Check if Arial is already registered to avoid overwriting user preferences
    if (!"Arial" %in% names(grDevices::windowsFonts())) {
      grDevices::windowsFonts(Arial = grDevices::windowsFont("TT Arial"))
    }
  }
}
