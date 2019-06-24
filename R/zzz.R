.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mlr3vis <- list(
    mlr_plot_theme = "theme_pubr"
  )
  toset <- !(names(op.mlr3vis) %in% names(op))
  if (any(toset)) {
    message(paste0("mlr3vis global settings: ", names(op.mlr3vis[toset]), "=", op.mlr3vis[toset]))
    options(op.mlr3vis[toset])
  }

  #  theme_set(ggpubr::theme_pubr())

  invisible()
}


plotWithTheme = function(p) {
  op <- options()
  if ("mlr_plot_theme" %in% names(op)) {
    ggthemeFun = get(op$mlr_plot_theme)
    if (class(ggthemeFun)[[1]] == "function") {
      p <- p + ggthemeFun()
    } else if (class(ggthemeFun)[[1]] == "theme") {
      p <- p + ggthemeFun
    }
  }
  return(p)
}
