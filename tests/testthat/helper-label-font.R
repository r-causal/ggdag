# A guard for tests that pin geometry measured with Helvetica's text metrics.
#
# Two kinds of pin depend on the default font. Exact label placements do: the
# label geoms draw in the device's default font, and a default font with
# other metrics, such as DejaVu Sans, sizes every box differently and changes
# which labels fit. So does any pin in millimetres of a plot whose panel is
# sized by text drawn outside it, such as a legend, axis text, strip labels,
# or a title: that text takes its room from the device before the panel is
# laid out, so a wider font narrows or shortens the panel and moves every
# millimetre drawn in it. Such pins only hold where the default font measures
# like Helvetica, as Arial does for the text the scenes carry. Width alone is
# not enough: Liberation Sans matches Helvetica's widths but not its heights,
# and that is enough to move a placement.
#
# The reference string is a label from the saturated ten-node scene, measured
# at the label geoms' default 11 pt on the kind of off-screen ragg device the
# placement tests draw on.

reference_label_font_text <- "Blood pressure"
reference_label_font_size <- c(width = 26.08, height = 2.71)
reference_label_font_tolerance <- c(width = 0.1, height = 0.05)

reference_label_font_cache <- new.env(parent = emptyenv())

# The width and height in millimetres of the reference string set in `family`
# on an off-screen ragg device. The device is closed, the device that was
# current before is made current again, and the file is removed before this
# returns.
measure_label_font <- function(family = "") {
  previous <- grDevices::dev.cur()
  file <- tempfile(fileext = ".png")
  open_test_ragg(file, 4, 3, res = 150)
  device <- grDevices::dev.cur()
  on.exit(
    {
      grDevices::dev.off(device)
      if (previous > 1) {
        grDevices::dev.set(previous)
      }
      unlink(file)
    },
    add = TRUE
  )

  grob <- grid::textGrob(
    reference_label_font_text,
    gp = grid::gpar(fontsize = 11, fontfamily = family, lineheight = 1.2)
  )
  c(
    width = grid::convertWidth(grid::grobWidth(grob), "mm", valueOnly = TRUE),
    height = grid::convertHeight(grid::grobHeight(grob), "mm", valueOnly = TRUE)
  )
}

# `measure_label_font()` of `family`, measured once per session.
measured_label_font <- function(family = "") {
  key <- paste0("family:", family)
  if (is.null(reference_label_font_cache[[key]])) {
    reference_label_font_cache[[key]] <- measure_label_font(family)
  }
  reference_label_font_cache[[key]]
}

# Whether `family` measures like Helvetica, to within the tolerance of each
# dimension. Without ragg there is no device to measure on, and no pin that
# depends on the font can be trusted.
is_reference_label_font <- function(family = "") {
  if (!rlang::is_installed("ragg")) {
    return(FALSE)
  }
  size <- measured_label_font(family)
  all(abs(size - reference_label_font_size) <= reference_label_font_tolerance)
}

# Skip the calling test unless `family` measures like Helvetica. The tests call
# this with the default family the plots draw in; the argument exists so the
# check itself can be exercised against other fonts.
skip_unless_reference_label_font <- function(family = "") {
  skip_if_not_installed("ragg")

  if (!is_reference_label_font(family)) {
    size <- measured_label_font(family)
    skip(sprintf(
      paste(
        "%s sets \"%s\" at %.2f x %.2f mm, not Helvetica's %.2f x %.2f mm;",
        "geometry pinned with Helvetica's metrics does not hold"
      ),
      if (identical(family, "")) "The default font" else family,
      reference_label_font_text,
      size[["width"]],
      size[["height"]],
      reference_label_font_size[["width"]],
      reference_label_font_size[["height"]]
    ))
  }
  invisible(TRUE)
}
