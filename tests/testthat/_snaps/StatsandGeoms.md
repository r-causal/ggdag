# We do not need to update `silent_add()`.

    Code
      body
    Output
      {
          if (is.null(scale)) {
              return()
          }
          prev_aes <- self$find(scale$aesthetics)
          if (any(prev_aes)) {
              scalename <- self$scales[prev_aes][[1]]$aesthetics[1]
              cli::cli_inform(c("Scale for {.field {scalename}} is already present.", 
                  "Adding another scale for {.field {scalename}}, which will replace the existing scale."))
          }
          self$scales <- c(self$scales[!prev_aes], list(scale))
      }

