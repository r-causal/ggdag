# `ggdag_options_reset()` nulls every `ggdag.*` option, including the layout
# option that helper-load_dag.R sets for the whole suite. Any test that calls it
# must put the surrounding state back, or every file that runs afterwards in the
# same worker silently gets a different default layout.
ggdag_option_names <- function() {
  option_names <- names(options())
  option_names[grepl("^ggdag\\.", option_names)]
}

local_ggdag_option_state <- function(.env = parent.frame()) {
  old_state <- options()[ggdag_option_names()]
  withr::defer(
    {
      # an option the test set but that was absent at capture has to be unset,
      # not merely left alone, or it leaks into every file that follows
      added <- setdiff(ggdag_option_names(), names(old_state))
      restored <- old_state
      restored[added] <- list(NULL)
      do.call(options, restored)
    },
    envir = .env
  )
  invisible(old_state)
}
