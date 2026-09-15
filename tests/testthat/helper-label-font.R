# A guard for tests that pin exact label placements.
#
# Those pins were measured with Helvetica's text metrics, which Arial shares
# for the labels the scenes carry. The label geoms draw in the device's default
# font, and a default font with other metrics, such as DejaVu Sans, sizes every
# box differently and changes which labels fit, so a pinned outcome only holds
# where the default font measures like Helvetica. Width alone is not enough:
# Liberation Sans matches Helvetica's widths but not its heights, and that is
# enough to move a placement.
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

# Skip the calling test unless `family` measures like Helvetica. The tests call
# this with the default family the label geoms draw in; the argument exists so
# the check itself can be exercised against other fonts. Each family is
# measured once per session.
skip_unless_reference_label_font <- function(family = "") {
  skip_if_not_installed("ragg")

  key <- paste0("family:", family)
  if (is.null(reference_label_font_cache[[key]])) {
    reference_label_font_cache[[key]] <- measure_label_font(family)
  }
  size <- reference_label_font_cache[[key]]

  off <- abs(size - reference_label_font_size) > reference_label_font_tolerance
  if (any(off)) {
    skip(sprintf(
      paste(
        "%s sets \"%s\" at %.2f x %.2f mm, not Helvetica's %.2f x %.2f mm;",
        "placements pinned with Helvetica's metrics do not hold"
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
