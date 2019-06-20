.onLoad <- function(libname, pkgname){
  op <- options()
  op.mlr3vis <- list(
    mlr_plot_theme = "theme_pubr"
  )
  toset <- !(names(op.mlr3vis) %in% names(op))
  if (any(toset)) {
    message(paste0("mlr3vis global settings: ",names(op.mlr3vis[toset]),"=",1))
    options(op.mlr3vis[toset])
  }

  theme_set(ggpubr::theme_pubr())

  invisible()
}
