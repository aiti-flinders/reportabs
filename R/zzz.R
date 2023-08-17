.onAttach <- function(libname = find.package("reportabs"), pkgname = "reportabs") {
  if (is.na(Sys.getenv("R_REPORTABS_THEME", unset = NA))) {
    packageStartupMessage(
      "Environment variable 'R_REPORTABS_THEME' is unset. ",
      "Created plots will use the default theme colours (Orange).\n",
      "You can set 'R_REPORTABS_THEME' by typing\n\tSys.setenv(R_REPORTABS_THEME = <palette>).\n",
      "Type palette_names() to see available palettes"
    )
  }
}
