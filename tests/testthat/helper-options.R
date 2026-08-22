# `ggdag_options_reset()` nulls every `ggdag.*` option, including the layout
# option that helper-load_dag.R sets for the whole suite. Any test that calls it
# must put the surrounding state back, or every file that runs afterwards in the
# same worker silently gets a different default layout.
local_ggdag_option_state <- function(.env = parent.frame()) {
  all_options <- options()
  old_state <- all_options[grepl("^ggdag\\.", names(all_options))]
  withr::defer(do.call(options, as.list(old_state)), envir = .env)
  invisible(old_state)
}
