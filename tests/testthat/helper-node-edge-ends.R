# Helpers for the tests of edges that stop a fixed gap outside the node at each
# of their ends, under both edge engines, and of the automatic labels that cut
# the edges they trace at the same ends.
#
# An edge stops 2 mm, times the plot's `size`, outside the outline of the node
# it meets. A circle node (point shapes 16, 19, and 21) has its outline at its
# radius, so the tip lies `radius + 2` mm from the centre in a straight line. A
# square node (15 and 22) has its outline at its half side, so the tip lies on
# the square `half side + 2` mm out from the centre along both axes: the larger
# of its horizontal and vertical offsets from the centre is `half side + 2`.

# Fixtures ---------------------------------------------------------------------

# A DAG with a confounder, a node that only has a bidirected edge, and so both
# the link and the arc edge layers.
cap_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    z ~ ~w,
    exposure = "x",
    outcome = "y"
  )
}

# A DAG whose controlled node `z` has an edge running into it and two running
# out of it, and is not a collider, so no collider lines are drawn.
controlled_dag <- function() {
  dagify(
    y ~ x + z,
    x ~ z,
    z ~ a,
    exposure = "x",
    outcome = "y"
  )
}

# The DAG of the README figure, whose three adjustment sets put square nodes at
# the start and the end of directed edges and at both ends of a bidirected arc.
readme_dag <- function() {
  dagitty::dagitty(
    "dag {
      y <- x <- z1 <- v -> z2 -> y
      z1 <- w1 <-> w2 -> z2
      x <- w1 -> y
      x <- w2 -> y
      x [exposure]
      y [outcome]
    }"
  ) |>
    tidy_dagitty()
}

# A DAG with one unconditional instrument and one instrument conditional on
# `w`, so the instrumental plot draws `w` as a square in one of its panels.
conditional_iv_dag <- function() {
  dagify(
    y ~ x + u + w,
    x ~ z + iu + u + w,
    z ~ w,
    exposure = "x",
    outcome = "y",
    latent = "u"
  )
}

# Expected geometry ------------------------------------------------------------

# The point shape number a ggplot2 shape value is drawn with. A shape scale can
# name its shapes, and the built data then carries the name.
shape_number <- function(shape) {
  if (!is.character(shape)) {
    return(shape)
  }
  named <- c(circle = 19, square = 15, `square filled` = 22)
  number <- unname(named[shape])
  digits <- is.na(number) & grepl("^[0-9]+$", shape)
  number[digits] <- as.numeric(shape[digits])
  number
}

is_square_shape <- function(shape) {
  shape_number(shape) %in% c(15, 22)
}

# The distance, in millimetres, from the centre of a node drawn at ggplot2 size
# `size` with point shape `shape` to its outline: the radius of a circle, and
# the half side of a square. R draws the circles (16, 19, 21) with radius
# `0.375 * size` mm, the solid square (15) with that radius as its half side,
# and the filled square (22) with the area of the circle, a half side of
# `sqrt(pi / 4)` radii. Any other shape has no outline here, so a scene that
# draws one fails rather than being checked against a guess.
expected_outline_mm <- function(shape, size) {
  number <- shape_number(shape)
  radius <- 0.375 * size
  dplyr::case_when(
    number %in% c(16, 19, 21) ~ radius,
    number == 15 ~ radius,
    number == 22 ~ radius * sqrt(pi / 4),
    .default = NA_real_
  )
}

# The ggraph cap geometry an edge end at a node of shape `shape` stops at.
expected_cap_geometry <- function(shape) {
  ifelse(is_square_shape(shape), "rect", "circle")
}

# How far a point `dx`, `dy` mm from the centre of a node of shape `shape` lies
# out from that centre, measured the way the node's outline is: in a straight
# line for a circle, and along the farther axis for a square.
outline_distance_mm <- function(shape, dx, dy) {
  ifelse(
    is_square_shape(shape),
    pmax(abs(dx), abs(dy)),
    sqrt(dx^2 + dy^2)
  )
}

node_shape_name <- function(shape) {
  number <- shape_number(shape)
  dplyr::case_when(
    number %in% c(16, 19, 21) ~ "circle",
    number %in% c(15, 22) ~ "square",
    .default = paste("shape", shape)
  )
}

# The node whose centre sits at (`x`, `y`) among `nodes`, a data frame with
# `x` and `y` columns, as a row index.
node_row_at <- function(nodes, x, y) {
  distance <- sqrt((nodes$x - x)^2 + (nodes$y - y)^2)
  match_row <- which(distance < 1e-9)
  if (length(match_row) != 1) {
    return(NA_integer_)
  }
  match_row
}

# Reading a drawn plot ---------------------------------------------------------

# Empty ggplot2's cache of text descents. ggplot2 caches the descent of each
# font size under the name of the device it was measured on, not under the
# device's resolution, and ragg rounds a descent to whole pixels, so the first
# ragg device a process drew text on would otherwise fix the height of the
# legend and axis text, and with it the millimetres of every later panel,
# whatever resolution a later draw is made at. Emptied before a draw, the
# cache holds only the descents measured on the device drawn on. A ggplot2
# without the cache has nothing to empty.
reset_text_descent_cache <- function() {
  cache <- get0(
    "descent_cache",
    envir = asNamespace("ggplot2"),
    inherits = FALSE
  )
  if (is.environment(cache)) {
    rm(list = ls(cache, all.names = TRUE), envir = cache)
  }
  invisible()
}

# Empty R's graphics engine cache of text metrics on the current device. The
# engine (`GEMetricInfo()`) keeps the metrics of the last "M" it measured,
# which is what every text height is read from, and keys them by the address
# of the device and its close function rather than by its resolution. Two
# ragg devices share that function, and a device opened at the address of one
# just closed would read the closed device's text height, in its pixels,
# until text of another size is measured. Measuring an "M" at a size no plot
# sets makes the cache hold the current device, so the first text a plot
# measures is measured on it.
reset_engine_metric_cache <- function() {
  grid::convertHeight(
    grid::grobHeight(grid::textGrob("M", gp = grid::gpar(fontsize = 1))),
    "mm"
  )
  invisible()
}

# Open an off-screen ragg device writing to `file`, `width` by `height`
# inches at `res` dots per inch, whose text metrics are measured on it rather
# than read from a device the process closed before (see
# `reset_engine_metric_cache()`). The caller closes the device.
open_test_ragg <- function(file, width, height, res = 96) {
  ragg::agg_png(file, width = width, height = height, units = "in", res = res)
  reset_engine_metric_cache()
  invisible(file)
}

# Draw `plot` off screen on a device of a fixed size, force the grob tree so
# that every `makeContent()` method has run, and evaluate `code` with the
# drawn tree on the display list. `code` is a function of the built plot. The
# text is measured afresh on the device, so the panel is the same size
# whatever the process drew before.
with_forced_plot <- function(plot, code, width = 7, height = 5) {
  file <- tempfile(fileext = ".png")
  open_test_ragg(file, width, height)
  reset_text_descent_cache()
  on.exit(
    {
      grDevices::dev.off()
      unlink(file)
      reset_text_descent_cache()
    },
    add = TRUE
  )

  built <- ggplot2::ggplot_build(plot)
  gtable <- ggplot2::ggplot_gtable(built)
  grid::grid.newpage()
  grid::grid.draw(gtable)
  grid::grid.force()

  code(built)
}

# The forced grobs whose own name matches `pattern`, each with the viewport
# path it was drawn in. The gtable names the grob tree of each panel
# `panel-<i>`, where `i` is the panel's index, and the grobs are returned with
# that index.
forced_grobs <- function(pattern) {
  found <- grid::grid.grep(
    pattern,
    grep = TRUE,
    global = TRUE,
    viewports = TRUE
  )
  if (length(found) == 0) {
    return(list())
  }
  own_name <- vapply(found, \(path) sub(".*::", "", as.character(path)), "")
  found <- found[grepl(pattern, own_name)]

  lapply(found, \(path) {
    name <- as.character(path)
    panel <- regmatches(name, regexpr("panel-[0-9]+\\.", name))
    list(
      grob = grid::grid.get(path),
      vp_path = attr(path, "vpPath"),
      panel = if (length(panel) == 0) {
        NA_integer_
      } else {
        as.integer(sub("panel-([0-9]+)\\.", "\\1", panel))
      }
    )
  })
}

convert_mm_x <- function(value) {
  if (!grid::is.unit(value)) {
    return(as.numeric(value))
  }
  grid::convertX(value, "mm", valueOnly = TRUE)
}

convert_mm_y <- function(value) {
  if (!grid::is.unit(value)) {
    return(as.numeric(value))
  }
  grid::convertY(value, "mm", valueOnly = TRUE)
}

convert_mm_length <- function(value) {
  if (!grid::is.unit(value)) {
    return(as.numeric(value))
  }
  grid::convertWidth(value, "mm", valueOnly = TRUE)
}

# The node glyphs the node layers of `plot` draw in panel `panel` of `built`,
# with their centres in millimetres. Call it with the panel's viewport pushed.
panel_nodes_mm <- function(plot, built, panel) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$geom, c("GeomDagPoint", "GeomDagNode"))
  }))
  nodes <- purrr::map(index, \(i) {
    data <- as.data.frame(built$data[[i]])
    data <- data[as.integer(data$PANEL) == panel, , drop = FALSE]
    if (nrow(data) == 0) {
      return(NULL)
    }
    data.frame(
      x = data$x,
      y = data$y,
      shape = data[["shape"]] %||% 19,
      size = data[["size"]] %||% 16
    )
  }) |>
    purrr::list_rbind()
  if (nrow(nodes) == 0) {
    return(data.frame(
      x = numeric(),
      y = numeric(),
      shape = numeric(),
      size = numeric()
    ))
  }

  panel_params <- built$layout$panel_params[[panel]]
  npc <- built$layout$coord$transform(nodes, panel_params)
  nodes$x <- convert_mm_x(grid::unit(npc$x, "npc"))
  nodes$y <- convert_mm_y(grid::unit(npc$y, "npc"))
  nodes
}

# The node among `nodes` an edge end at (`x`, `y`) mm meets: the nearest node
# whose disc holds the end, the widest where glyphs overlap there. A routed
# edge may end on a port off its node's centre line, still inside the node.
node_at_end <- function(nodes, x, y) {
  distance <- sqrt((nodes$x - x)^2 + (nodes$y - y)^2)
  if (!any(is.finite(distance))) {
    return(NA_integer_)
  }
  nearest <- which(distance <= min(distance, na.rm = TRUE) + 1e-6)
  nearest <- nearest[which.max(nodes$size[nearest])]
  if (distance[[nearest]] > 0.375 * nodes$size[[nearest]]) {
    return(NA_integer_)
  }
  nearest
}

# The point halfway along a path of points `x`, `y`, by arc length. Two edges
# drawn between the same two nodes, such as a directed edge and a bidirected
# arc, are told apart by where their middles lie.
path_midpoint <- function(x, y) {
  arc <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))
  half <- arc[[length(arc)]] / 2
  c(
    stats::approx(arc, x, xout = half, ties = "ordered")$y,
    stats::approx(arc, y, xout = half, ties = "ordered")$y
  )
}

# The point ggarrow cuts a path back to when it resects `resect` mm from the
# path's last point: where the path leaves the disc of that radius around its
# last point, interpolated the way ggarrow interpolates it. `NA` for a path
# shorter than its resection.
resect_cut_point <- function(x, y, resect) {
  n <- length(x)
  if (n < 2 || is.na(resect)) {
    return(c(NA_real_, NA_real_))
  }
  if (resect <= 0) {
    return(c(x[[n]], y[[n]]))
  }
  distance <- sqrt((x - x[[n]])^2 + (y - y[[n]])^2)
  outside <- which(distance >= resect)
  if (length(outside) == 0) {
    return(c(NA_real_, NA_real_))
  }
  before <- max(outside)
  after <- before + 1L
  d <- (resect - distance[[before]]) / (distance[[after]] - distance[[before]])
  c(
    x[[before]] * (1 - d) + x[[after]] * d,
    y[[before]] * (1 - d) + y[[after]] * d
  )
}

# Where ggarrow draws the tip of the ornament at the end of a path it resects
# by `resect` mm, with the arrow justified at its tip as every DAG edge layer
# draws it. ggarrow cuts the path back by the resection plus the ornament's
# own reach (`ornament`, in mm) and draws the ornament straight from the cut
# towards the path's last point, so the tip lies on the chord from the cut to
# the end, `resect` mm from the end, rather than on the path itself: on a
# curve it leaves the path by the sagitta of that chord. A path with no
# ornament at the end stops at the cut. `NA` for a path that lies wholly
# within the cut, which ggarrow does not draw at all.
drawn_tip_point <- function(x, y, resect, ornament) {
  cut <- resect_cut_point(x, y, resect + ornament)
  if (anyNA(cut) || ornament <= 0) {
    return(cut)
  }
  n <- length(x)
  to_end <- c(x[[n]], y[[n]]) - cut
  cut + ornament * to_end / sqrt(sum(to_end^2))
}

# The reach, in millimetres, of the ornament ggarrow draws at the end of a path
# whose shaft is `width` mm wide there, given `length` mm: how far the drawn
# ornament extends from the point it is anchored at, which is how much further
# ggarrow cuts the path back. An ornament is a matrix of a unit shape, drawn
# at the larger of `length` and the width scaled by the shape's height, and
# its reach is that scale unless the shape declares its own. The DAG layers
# draw matrix ornaments only, so an ornament function is not modelled here.
ornament_reach_mm <- function(ornament, length, width) {
  if (is.null(ornament)) {
    return(0)
  }
  if (!is.matrix(ornament)) {
    stop("only matrix arrow ornaments are modelled here")
  }
  scale <- max(length, width / diff(range(ornament[, "y"])))
  scale <- (attr(ornament, "length") %||% max(ornament[, "x"])) * scale
  (attr(ornament, "resect") %||% 1) * scale
}

# The ggarrow edges -------------------------------------------------------------

# The paths a forced ggarrow grob hands to ggarrow, in millimetres, one list of
# `x` and `y` per edge in the order the grob draws them, the resection
# ggarrow cuts from the fins end (`fins`) and the head end (`head`) of each,
# the reach of the ornament drawn at each end (`fins_reach`, `head_reach`),
# and whether the paths are arcs. The routed layer and `geom_dag_arrow()`
# draw an `arrow_path` grob, whose points and resections are its own fields.
# The arc layer draws a `curve_arrow` grob, which builds its paths from the
# curve it holds when it is drawn, so they are built here the same way. Call
# it with the grob's viewport pushed.
arrow_grob_paths <- function(grob) {
  arc <- FALSE
  if (inherits(grob, "arrow_path")) {
    fields <- unclass(grob$id_rle)
    id <- rep(seq_along(fields$length), fields$length)
    x <- convert_mm_x(grob$x)
    y <- convert_mm_y(grob$y)
    paths <- unname(lapply(split(seq_along(id), id), \(i) {
      list(x = x[i], y = y[i])
    }))
    fins <- grob$resect$fins
    head <- grob$resect$head
    width <- convert_mm_length(grob$shaft_width)
    width_fins <- rep_len(width, length(id))[!duplicated(id)]
    width_head <- rep_len(width, length(id))[!duplicated(id, fromLast = TRUE)]
    length_fins <- grob$length_fins
    length_head <- grob$length_head
    arrow_fins <- grob$arrow_fins
    arrow_head <- grob$arrow_head
  } else {
    curve <- grid::makeContent(grob$curve)$children[[1]]
    if (inherits(curve, "xspline")) {
      arc <- TRUE
      points <- grid::xsplinePoints(curve)
      if (all(c("x", "y") %in% names(points))) {
        points <- list(points)
      }
      paths <- lapply(points, \(p) {
        list(x = convert_mm_x(p$x), y = convert_mm_y(p$y))
      })
    } else {
      x0 <- convert_mm_x(curve$x0)
      y0 <- convert_mm_y(curve$y0)
      x1 <- convert_mm_x(curve$x1)
      y1 <- convert_mm_y(curve$y1)
      paths <- lapply(seq_along(x0), \(i) {
        list(x = c(x0[[i]], x1[[i]]), y = c(y0[[i]], y1[[i]]))
      })
    }
    fins <- grob$params$resect_fins
    head <- grob$params$resect_head
    width_fins <- convert_mm_length(grob$params$width_fins %||% 1)
    width_head <- convert_mm_length(grob$params$width_head %||% 1)
    length_fins <- grob$params$length_fins
    length_head <- grob$params$length_head
    arrow_fins <- grob$params$arrow_fins
    arrow_head <- grob$params$arrow_head
  }

  n <- length(paths)
  reach <- function(ornament, length, width) {
    if (is.list(ornament) && !is.matrix(ornament)) {
      stop("per-edge arrow ornaments are not modelled here")
    }
    length <- rep_len(convert_mm_length(length %||% 0), n)
    width <- rep_len(width, n)
    vapply(
      seq_len(n),
      \(i) ornament_reach_mm(ornament, length[[i]], width[[i]]),
      numeric(1)
    )
  }
  list(
    paths = paths,
    arc = arc,
    fins = rep_len(convert_mm_length(fins %||% 0), n),
    head = rep_len(convert_mm_length(head %||% 0), n),
    fins_reach = reach(arrow_fins, length_fins, width_fins),
    head_reach = reach(arrow_head, length_head, width_head)
  )
}

# Every ggarrow edge the forced plot `plot`, built as `built`, draws: one
# element per arrow grob, holding its panel, its paths in millimetres, and the
# resection of each end of each path, which is what ggarrow draws the edge
# with, whether the layer settled it when the plot was built or when it was
# drawn. Call it with the forced tree on the display list.
forced_arrow_drawings <- function(plot, built) {
  grobs <- forced_grobs("curve_arrow|arrow_path")
  drawings <- purrr::map(grobs, \(found) {
    # a legend key draws its arrow outside every panel
    if (
      !inherits(found$grob, c("curve_arrow", "arrow_path")) ||
        is.na(found$panel)
    ) {
      return(NULL)
    }
    grid::upViewport(0)
    grid::downViewport(found$vp_path)
    on.exit(grid::upViewport(0), add = TRUE)

    drawn <- arrow_grob_paths(found$grob)
    drawn$panel <- found$panel
    drawn$nodes <- panel_nodes_mm(plot, built, found$panel)
    drawn
  })
  purrr::compact(drawings)
}

# Every ggarrow edge `plot` draws, as drawn on a device of a fixed size, as
# `forced_arrow_drawings()` reads it.
arrow_grob_drawings <- function(plot, width = 7, height = 5) {
  with_forced_plot(
    plot,
    \(built) forced_arrow_drawings(plot, built),
    width = width,
    height = height
  )
}

# One row per end of every ggarrow edge among `drawings`, from
# `forced_arrow_drawings()`: the panel, which end, whether the edge is an arc,
# the ends of the edge's path, the node the end meets, with the shape and size
# it is drawn with and its centre, the resection at the end, and where the tip
# is drawn, also relative to that node's centre, all in millimetres. The fins
# end is the start of the path and the head end its finish. An edge shorter
# than the resection and ornament at an end is not drawn by ggarrow, and has
# no tip there (`drawn` is `FALSE`).
arrow_drawing_ends <- function(drawings) {
  purrr::map(drawings, \(drawn) {
    nodes <- drawn$nodes
    purrr::map(seq_along(drawn$paths), \(k) {
      path <- drawn$paths[[k]]
      n <- length(path$x)
      middle <- path_midpoint(path$x, path$y)
      end_row <- function(end) {
        at <- if (end == "fins") 1L else n
        node <- node_at_end(nodes, path$x[[at]], path$y[[at]])
        resect <- if (end == "fins") drawn$fins[[k]] else drawn$head[[k]]
        tip <- if (end == "fins") {
          drawn_tip_point(
            rev(path$x),
            rev(path$y),
            resect,
            drawn$fins_reach[[k]]
          )
        } else {
          drawn_tip_point(path$x, path$y, resect, drawn$head_reach[[k]])
        }
        # the direction the path leaves its end in, which is the run the
        # tip lies on
        run <- if (end == "fins") {
          c(path$x[[2]] - path$x[[1]], path$y[[2]] - path$y[[1]])
        } else {
          c(path$x[[n - 1]] - path$x[[n]], path$y[[n - 1]] - path$y[[n]])
        }
        run <- run / sqrt(sum(run^2))
        data.frame(
          panel = drawn$panel,
          end = end,
          arc = drawn$arc,
          drawn = !anyNA(tip),
          run_dx = run[[1]],
          run_dy = run[[2]],
          from_x = path$x[[1]],
          from_y = path$y[[1]],
          to_x = path$x[[n]],
          to_y = path$y[[n]],
          mid_x = middle[[1]],
          mid_y = middle[[2]],
          shape = nodes$shape[node],
          size = nodes$size[node],
          centre_x = nodes$x[node],
          centre_y = nodes$y[node],
          resect = resect,
          tip_x = tip[[1]],
          tip_y = tip[[2]],
          tip_dx = tip[[1]] - nodes$x[node],
          tip_dy = tip[[2]] - nodes$y[node]
        )
      }
      rbind(end_row("fins"), end_row("head"))
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

# One row per end of every ggarrow edge `plot` draws on a device of a fixed
# size, as `arrow_drawing_ends()` describes it.
drawn_arrow_ends <- function(plot, width = 7, height = 5) {
  arrow_drawing_ends(arrow_grob_drawings(plot, width = width, height = height))
}

# The ggraph edges --------------------------------------------------------------

# One row per end of every ggraph edge the forced plot `plot`, built as
# `built`, draws, in the shape `arrow_drawing_ends()` returns, with `start`
# and `end` for the ends and no resection. A capped path grob keeps the uncut
# path in native units, whose ends are the node centres, and draws the cut
# path as a child in millimetres. The arrowhead of a closed grid arrow has its
# tip at the end of the path. Call it with the forced tree on the display
# list.
forced_edge_ends <- function(plot, built) {
  grobs <- forced_grobs("cappedpathgrob")
  purrr::map(grobs, \(found) {
    grob <- found$grob
    if (
      !inherits(grob, "cappedpathgrob") ||
        length(grob$x) == 0 ||
        is.na(found$panel)
    ) {
      return(NULL)
    }
    grid::upViewport(0)
    grid::downViewport(found$vp_path)
    on.exit(grid::upViewport(0), add = TRUE)

    nodes <- panel_nodes_mm(plot, built, found$panel)
    centre_x <- convert_mm_x(grob$x)
    centre_y <- convert_mm_y(grob$y)
    edge_ids <- unique(grob$id)
    drawn <- grob$children[[1]]
    drawn_ids <- if (inherits(drawn, "polyline")) unique(drawn$id)
    if (length(drawn_ids) != length(edge_ids)) {
      return(NULL)
    }
    drawn_x <- convert_mm_x(drawn$x)
    drawn_y <- convert_mm_y(drawn$y)

    purrr::map(seq_along(edge_ids), \(k) {
      uncut <- which(grob$id == edge_ids[[k]])
      cut <- which(drawn$id == drawn_ids[[k]])
      from <- uncut[[1]]
      to <- uncut[[length(uncut)]]
      middle <- path_midpoint(centre_x[uncut], centre_y[uncut])
      end_row <- function(end, centre, tip) {
        node <- node_at_end(nodes, centre_x[[centre]], centre_y[[centre]])
        data.frame(
          panel = found$panel,
          end = end,
          arc = FALSE,
          drawn = TRUE,
          from_x = centre_x[[from]],
          from_y = centre_y[[from]],
          to_x = centre_x[[to]],
          to_y = centre_y[[to]],
          mid_x = middle[[1]],
          mid_y = middle[[2]],
          shape = nodes$shape[node],
          size = nodes$size[node],
          centre_x = nodes$x[node],
          centre_y = nodes$y[node],
          resect = NA_real_,
          tip_x = drawn_x[[tip]],
          tip_y = drawn_y[[tip]],
          tip_dx = drawn_x[[tip]] - nodes$x[node],
          tip_dy = drawn_y[[tip]] - nodes$y[node]
        )
      }
      rbind(
        end_row("start", from, cut[[1]]),
        end_row("end", to, cut[[length(cut)]])
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}

# One row per end of every ggraph edge `plot` draws on a device of a fixed
# size, as `forced_edge_ends()` describes it.
drawn_edge_ends <- function(plot, width = 7, height = 5) {
  with_forced_plot(
    plot,
    \(built) forced_edge_ends(plot, built),
    width = width,
    height = height
  )
}

# The automatic labels ---------------------------------------------------------

# One row per edge among `edges`, the traced edges the automatic label engine
# turns into the ink it keeps its labels off: the ends of the traced path, the
# millimetres cut from its start (`cap_fins`) and its end (`cap_head`), and
# the points it is cut back to. The engine drops the part of a traced edge
# within that many millimetres of either end, in a straight line from the end,
# which is where ggarrow cuts a path it resects.
traced_edge_ends <- function(edges) {
  rows <- split(
    seq_len(nrow(edges)),
    factor(edges$edge_id, levels = unique(edges$edge_id))
  )
  purrr::map(rows, \(i) {
    x <- edges$x[i]
    y <- edges$y[i]
    n <- length(i)
    cap_fins <- edges$cap_fins[i][[1]]
    cap_head <- edges$cap_head[i][[1]]
    start <- resect_cut_point(rev(x), rev(y), cap_fins)
    end <- resect_cut_point(x, y, cap_head)
    middle <- path_midpoint(x, y)
    data.frame(
      from_x = x[[1]],
      from_y = y[[1]],
      to_x = x[[n]],
      to_y = y[[n]],
      mid_x = middle[[1]],
      mid_y = middle[[2]],
      cap_fins = cap_fins,
      cap_head = cap_head,
      start_x = start[[1]],
      start_y = start[[2]],
      end_x = end[[1]],
      end_y = end[[2]]
    )
  }) |>
    purrr::list_rbind()
}

# Draw the one-panel plot `plot` on a device of a fixed size and evaluate
# `code`, a function of the built plot, with the forced tree on the display
# list, recording the edges the automatic label layer traces where the label
# engine turns them into ink. Returns the value of `code` as `value` and the
# traced edges, as `traced_edge_ends()` describes them, as `traced`. Drawing
# the plot and then forcing its grob tree runs the label grob's
# `makeContent()` once for each, on the same panel and the same device, and
# the traced edges are read from the last.
with_traced_labels <- function(plot, code, width = 7, height = 5) {
  label_ink <- get("label_ink", envir = asNamespace("ggdag"))
  traced <- list()
  record <- function(edges, cap, ...) {
    traced[[length(traced) + 1L]] <<- edges
    label_ink(edges, cap, ...)
  }

  drawn <- testthat::with_mocked_bindings(
    with_forced_plot(
      plot,
      \(built) {
        list(panels = nrow(built$layout$layout), value = code(built))
      },
      width = width,
      height = height
    ),
    label_ink = record,
    .package = "ggdag"
  )
  if (drawn$panels != 1 || length(traced) == 0) {
    stop("expected one panel of traced labels")
  }
  list(value = drawn$value, traced = traced_edge_ends(traced[[length(traced)]]))
}

# The edges the one-panel plot `plot` draws and the edges its automatic label
# layer traces, both read from one drawing on a device of a fixed size, so
# the two are measured in the same panel whatever the process drew before:
# `drawn`, one row per end of every ggarrow and ggraph edge, as
# `arrow_drawing_ends()` and `forced_edge_ends()` describe them, and `traced`,
# as `traced_edge_ends()` describes them.
drawn_and_traced_ends <- function(plot, width = 7, height = 5) {
  found <- with_traced_labels(
    plot,
    \(built) {
      purrr::list_rbind(list(
        arrow_drawing_ends(forced_arrow_drawings(plot, built)),
        forced_edge_ends(plot, built)
      ))
    },
    width = width,
    height = height
  )
  list(drawn = found$value, traced = found$traced)
}

# The edges each panel of the faceted plot `plot` draws and the edges its
# automatic label layer traces there, read from one drawing on a device of a
# fixed size: one element per panel that draws an edge, named by the panel's
# index, holding `drawn` and `traced` as `drawn_and_traced_ends()` describes
# them, with `traced` `NULL` where the labels trace nothing in that panel. The
# label engine is found in a panel by the viewport it draws in, which the edge
# grobs of the panel are drawn in too.
drawn_and_traced_panel_ends <- function(plot, width = 7, height = 5) {
  label_ink <- get("label_ink", envir = asNamespace("ggdag"))
  traced <- list()
  record <- function(edges, cap, ...) {
    viewport <- viewport_panel(unclass(grid::current.vpPath())$path)
    traced[[viewport]] <<- edges
    label_ink(edges, cap, ...)
  }

  found <- testthat::with_mocked_bindings(
    with_forced_plot(
      plot,
      \(built) {
        grobs <- forced_grobs("curve_arrow|arrow_path|cappedpathgrob")
        grobs <- purrr::keep(grobs, \(one) !is.na(one$panel))
        list(
          viewports = unique(data.frame(
            panel = purrr::map_int(grobs, "panel"),
            viewport = purrr::map_chr(grobs, \(one) {
              viewport_panel(as.character(one$vp_path))
            })
          )),
          drawn = purrr::list_rbind(list(
            arrow_drawing_ends(forced_arrow_drawings(plot, built)),
            forced_edge_ends(plot, built)
          ))
        )
      },
      width = width,
      height = height
    ),
    label_ink = record,
    .package = "ggdag"
  )

  viewports <- found$viewports
  stopifnot(!anyDuplicated(viewports$panel))
  panels <- lapply(seq_len(nrow(viewports)), \(i) {
    edges <- traced[[viewports$viewport[[i]]]]
    list(
      drawn = found$drawn[found$drawn$panel == viewports$panel[[i]], ],
      traced = if (!is.null(edges)) traced_edge_ends(edges)
    )
  })
  stats::setNames(panels, viewports$panel)
}

# One row per edge the automatic label layer of `plot` traces in panel
# `panel`: where the edge starts and ends, in data units, the index of the
# layer it is traced from (`layer`, `NA` for an edge traced as its chord),
# and the cap the label engine cuts the traced edge back by at each end. The grob carries the traced
# edges in the order the built layer holds them, so the positions are read
# from the built layer and the caps from the grob. An edge the grob carries no
# cap of its own for is cut by the layer's single cap.
label_edge_caps <- function(plot, panel = 1) {
  index <- which(purrr::map_lgl(plot$layers, \(layer) {
    inherits(layer$stat, "StatNodesLabelAuto")
  }))
  grob <- ggplot2::layer_grob(plot, index[[1]])[[panel]]
  rows <- ggplot2::layer_data(plot, index[[1]])
  rows <- rows[
    rows$ggdag_role %in% "edge" & rows$PANEL == panel,
    ,
    drop = FALSE
  ]
  stopifnot(nrow(rows) == nrow(grob$edges))

  ids <- unique(rows$edge_id)
  first <- match(ids, rows$edge_id)
  last <- nrow(rows) - match(ids, rev(rows$edge_id)) + 1L

  cap_at <- function(column, at) {
    caps <- grob$edges[[column]]
    if (is.null(caps)) {
      return(rep(grob$params$edge_cap, length(at)))
    }
    caps[at]
  }

  data.frame(
    x = rows$x[first],
    y = rows$y[first],
    xend = rows$x[last],
    yend = rows$y[last],
    layer = rows$route_layer[first],
    cap_start = cap_at("cap_fins", first),
    cap_end = cap_at("cap_head", last)
  )
}

# The drawn edge each traced edge among `traced` is drawn as, among the edges
# `drawn`, both as `drawn_and_traced_ends()` reads them: `starts` and `ends`,
# the drawn ends of each drawn edge, and `matched`, for each traced edge the
# row of its drawn edge in both, `NA` where no single drawn edge matches it. A
# traced edge is matched to the drawn edge by the nodes it runs from and to,
# each the drawn node nearest its end, rather than by millimetres: the edge
# from `x` to `y` is matched to the edge drawn from `x` to `y`, and never to
# the one drawn from `y` to `x`. Where more than one edge
# is drawn from one node to the other, as a directed edge and a bidirected
# arc are, the traced edge is matched to the one whose middle lies nearest
# its own. A drawn edge two traced edges are matched to matches neither.
match_traced_edges <- function(traced, drawn) {
  # the drawn ends come in pairs, the start of each edge and then its end
  start_end <- drawn$end %in% c("fins", "start")
  starts <- drawn[start_end, , drop = FALSE]
  ends <- drawn[!start_end, , drop = FALSE]
  stopifnot(nrow(starts) == nrow(ends))
  start_x <- ifelse(is.na(starts$centre_x), starts$from_x, starts$centre_x)
  start_y <- ifelse(is.na(starts$centre_y), starts$from_y, starts$centre_y)
  end_x <- ifelse(is.na(ends$centre_x), ends$to_x, ends$centre_x)
  end_y <- ifelse(is.na(ends$centre_y), ends$to_y, ends$centre_y)
  centres <- unique(data.frame(x = c(start_x, end_x), y = c(start_y, end_y)))
  nearest_centre <- function(x, y) {
    vapply(
      seq_along(x),
      \(i) which.min((centres$x - x[[i]])^2 + (centres$y - y[[i]])^2),
      integer(1)
    )
  }
  drawn_from <- nearest_centre(start_x, start_y)
  drawn_to <- nearest_centre(end_x, end_y)
  traced_from <- nearest_centre(traced$from_x, traced$from_y)
  traced_to <- nearest_centre(traced$to_x, traced$to_y)

  matched <- vapply(
    seq_len(nrow(traced)),
    \(i) {
      same <- which(drawn_from == traced_from[[i]] & drawn_to == traced_to[[i]])
      if (length(same) <= 1) {
        return(if (length(same) == 1) same else NA_integer_)
      }
      apart <- sqrt(
        (starts$mid_x[same] - traced$mid_x[[i]])^2 +
          (starts$mid_y[same] - traced$mid_y[[i]])^2
      )
      same[[which.min(apart)]]
    },
    integer(1)
  )
  matched[matched %in% matched[duplicated(matched)]] <- NA_integer_
  list(starts = starts, ends = ends, matched = matched)
}

# The traced edges among `traced` whose cut ends are not where the edges among
# `drawn`, both as `drawn_and_traced_ends()` reads them, draw their tips,
# within `tolerance` mm, each traced edge matched to its drawn edge by
# `match_traced_edges()`.
label_tip_mismatches <- function(traced, drawn, tolerance = 0.05) {
  if (nrow(traced) == 0) {
    return("the labels trace no edges")
  }
  found <- match_traced_edges(traced, drawn)
  starts <- found$starts
  ends <- found$ends
  matched <- found$matched

  purrr::map_chr(seq_len(nrow(traced)), \(i) {
    edge <- traced[i, ]
    where <- sprintf(
      "the traced edge from (%.2f, %.2f) to (%.2f, %.2f) mm",
      edge$from_x,
      edge$from_y,
      edge$to_x,
      edge$to_y
    )
    if (is.na(matched[[i]])) {
      return(paste(where, "matches no single drawn edge"))
    }
    start <- starts[matched[[i]], ]
    end <- ends[matched[[i]], ]
    start_off <- sqrt(
      (edge$start_x - start$tip_x)^2 + (edge$start_y - start$tip_y)^2
    )
    end_off <- sqrt((edge$end_x - end$tip_x)^2 + (edge$end_y - end$tip_y)^2)
    problems <- c(
      if (is.na(start_off) || start_off > tolerance) {
        sprintf(
          "is cut at its start %.3f mm from the drawn tip (cap %.3f mm)",
          start_off,
          edge$cap_fins
        )
      },
      if (is.na(end_off) || end_off > tolerance) {
        sprintf(
          "is cut at its end %.3f mm from the drawn tip (cap %.3f mm)",
          end_off,
          edge$cap_head
        )
      }
    )
    if (length(problems) == 0) {
      return(NA_character_)
    }
    paste(where, paste(problems, collapse = " and "))
  }) |>
    purrr::discard(is.na)
}

# The traced edges among `traced` cut back by other millimetres at an end than
# the ggarrow edge drawn there, among `drawn`, both as
# `drawn_and_traced_ends()` reads them, is resected by, each traced edge
# matched to its drawn edge by `match_traced_edges()`. An end at a circle is cut by the resection
# itself; an end at a square is cut where the path crosses the square, and
# its tip is what `label_tip_mismatches()` checks instead.
label_cap_mismatches <- function(traced, drawn, tolerance = 1e-6) {
  if (nrow(traced) == 0) {
    return("the labels trace no edges")
  }
  found <- match_traced_edges(traced, drawn)
  purrr::map_chr(seq_len(nrow(traced)), \(i) {
    edge <- traced[i, ]
    where <- sprintf(
      "the traced edge from (%.2f, %.2f) to (%.2f, %.2f) mm",
      edge$from_x,
      edge$from_y,
      edge$to_x,
      edge$to_y
    )
    at <- found$matched[[i]]
    if (is.na(at)) {
      return(paste(where, "matches no single drawn edge"))
    }
    start <- found$starts[at, ]
    end <- found$ends[at, ]
    problems <- c(
      if (abs(edge$cap_fins - start$resect) > tolerance) {
        sprintf(
          "is cut by %.3f mm at its start where the drawn edge is resected by %.3f mm",
          edge$cap_fins,
          start$resect
        )
      },
      if (abs(edge$cap_head - end$resect) > tolerance) {
        sprintf(
          "is cut by %.3f mm at its end where the drawn edge is resected by %.3f mm",
          edge$cap_head,
          end$resect
        )
      }
    )
    if (length(problems) == 0) {
      return(NA_character_)
    }
    paste(where, paste(problems, collapse = " and "))
  }) |>
    purrr::discard(is.na)
}

# Mismatches -------------------------------------------------------------------

# The edge ends among `ends` whose drawn tip is not `gap` mm outside the
# outline of the node there, within `tolerance` mm, described by panel, end,
# path, and node. The end of a ggarrow arc at a square node is allowed
# `arc_tolerance` instead: the layer resects the arc where the curve crosses
# the square, but ggarrow draws the head straight from its cut towards the
# end of the path, so the tip leaves the curve by the sagitta of that chord,
# a third of a millimetre at most on a small device (see `drawn_tip_point()`).
# A circle is met at the same distance from any direction, so its arc ends
# are held to `tolerance`. An end ggarrow does not draw, because the edge is
# shorter than its resection, has no tip to check and is passed over.
tip_gap_mismatches <- function(
  ends,
  gap = 2,
  tolerance = 0.05,
  arc_tolerance = 0.35
) {
  if (nrow(ends) == 0) {
    return("the plot draws no edge ends")
  }
  ends <- ends[ends$drawn %||% TRUE, , drop = FALSE]
  expected <- expected_outline_mm(ends$shape, ends$size) + gap
  actual <- outline_distance_mm(ends$shape, ends$tip_dx, ends$tip_dy)
  allowed <- ifelse(
    (ends$arc %||% FALSE) & is_square_shape(ends$shape),
    arc_tolerance,
    tolerance
  )
  bad <- is.na(expected) | is.na(actual) | abs(actual - expected) > allowed

  sprintf(
    "panel %s, the %s end of the edge from (%.2f, %.2f) to (%.2f, %.2f) mm at a %s drawn at size %s: the tip is drawn %.3f mm out; expected %.3f mm",
    ends$panel[bad],
    ends$end[bad],
    ends$from_x[bad],
    ends$from_y[bad],
    ends$to_x[bad],
    ends$to_y[bad],
    node_shape_name(ends$shape[bad]),
    ends$size[bad],
    actual[bad],
    expected[bad]
  )
}

# The edge ends among `ends` whose drawn tip does not sit `gap` mm past the
# face of the node on the run the path leaves its end along, within
# `tolerance` mm, for the ends of orthogonal routes. An orthogonal path ends
# on its port's own line at the node's coordinate, which is the node's centre
# for a centre port and a point offset from it across the run otherwise, and
# the run into the port is axis-aligned, so the tip lies on that run. The
# face of a circle is `sqrt(r^2 - o^2)` along the run from the path's end,
# for a port offset `o` from the centre line, and the face of a square is its
# half side at any port within it.
port_gap_mismatches <- function(ends, gap = 2, tolerance = 0.05) {
  if (nrow(ends) == 0) {
    return("the plot draws no edge ends")
  }
  end_x <- ifelse(ends$end == "fins", ends$from_x, ends$to_x)
  end_y <- ifelse(ends$end == "fins", ends$from_y, ends$to_y)
  # the offset of the path's end from the node's centre, across the run
  offset <- abs(
    (end_x - ends$centre_x) *
      ends$run_dy -
      (end_y - ends$centre_y) * ends$run_dx
  )
  outline <- expected_outline_mm(ends$shape, ends$size)
  face <- ifelse(
    is_square_shape(ends$shape),
    outline,
    sqrt(pmax(outline^2 - offset^2, 0))
  )
  expected <- face + gap
  actual <- (ends$tip_x - end_x) *
    ends$run_dx +
    (ends$tip_y - end_y) * ends$run_dy
  bad <- is.na(expected) | is.na(actual) | abs(actual - expected) > tolerance

  sprintf(
    "panel %s, the %s end of the edge from (%.2f, %.2f) to (%.2f, %.2f) mm at a %s drawn at size %s, on a port offset %.2f mm: the tip is drawn %.3f mm along the run; expected %.3f mm",
    ends$panel[bad],
    ends$end[bad],
    ends$from_x[bad],
    ends$from_y[bad],
    ends$to_x[bad],
    ends$to_y[bad],
    node_shape_name(ends$shape[bad]),
    ends$size[bad],
    offset[bad],
    actual[bad],
    expected[bad]
  )
}

# The circle edge ends among `ends` that ggarrow does not resect by `gap` mm
# beyond the circle's radius. A resection is the straight-line distance from
# the end of the path, so at a circle it is the whole cap; at a square it
# depends on the angle the edge meets the square at, and only the drawn tip
# says whether it is right.
circle_resect_mismatches <- function(ends, gap = 2) {
  circle <- !is.na(ends$shape) & !is_square_shape(ends$shape)
  expected <- expected_outline_mm(ends$shape, ends$size) + gap
  bad <- circle & (is.na(ends$resect) | abs(ends$resect - expected) > 1e-6)

  sprintf(
    "panel %s, the %s end of the edge from (%.2f, %.2f) to (%.2f, %.2f) mm at a circle drawn at size %s is resected by %.4f mm; expected %.4f mm",
    ends$panel[bad],
    ends$end[bad],
    ends$from_x[bad],
    ends$from_y[bad],
    ends$to_x[bad],
    ends$to_y[bad],
    ends$size[bad],
    ends$resect[bad],
    expected[bad]
  )
}

# The edge ends among `ends` that ggarrow does not resect by `resect` mm.
fixed_resect_mismatches <- function(ends, resect) {
  if (nrow(ends) == 0) {
    return("the plot draws no edge ends")
  }
  bad <- is.na(ends$resect) | abs(ends$resect - resect) > 1e-6

  sprintf(
    "panel %s, the %s end of the edge from (%.2f, %.2f) to (%.2f, %.2f) mm is resected by %.4f mm; expected %.4f mm",
    ends$panel[bad],
    ends$end[bad],
    ends$from_x[bad],
    ends$from_y[bad],
    ends$to_x[bad],
    ends$to_y[bad],
    ends$resect[bad],
    resect
  )
}

# The edge ends among `ends` that meet no node, or a node not drawn at size
# `node_size`.
node_size_mismatches <- function(ends, node_size) {
  bad <- is.na(ends$size) | abs(ends$size - node_size) > 1e-9

  sprintf(
    "panel %s, the %s end of the edge from (%.2f, %.2f) to (%.2f, %.2f) mm meets %s",
    ends$panel[bad],
    ends$end[bad],
    ends$from_x[bad],
    ends$from_y[bad],
    ends$to_x[bad],
    ends$to_y[bad],
    ifelse(
      is.na(ends$size[bad]),
      "no node",
      paste("a node drawn at size", ends$size[bad])
    )
  )
}

# The default size -------------------------------------------------------------

# The canonical DAGs whose ggarrow edges at the default node size are pinned
# in fixtures/ggarrow-default-resects.rds, and the edge routes each is drawn
# with.
default_resect_scenes <- c("mediation", "smoking", "complex_chain")
default_resect_routes <- c("straight", "spline", "orthogonal")

# The canonical DAG `name` from `canonical_dag_specs`, laid out as every plot
# lays it out.
canonical_tidy_dag <- function(name) {
  spec <- canonical_dag_specs[[name]]
  dag <- dagitty::dagitty(paste0("dag { ", paste(spec, collapse = "; "), " }"))
  withr::with_seed(1234, tidy_dagitty(dag))
}

# The ggarrow edges `plot` draws on a 7 by 5 inch device: one row per edge
# with its panel, the ends of its path, and the resection of each end, and
# the path itself, in millimetres, sorted by where the edge runs so that the
# order the layers draw their edges in does not matter.
arrow_drawing_record <- function(plot) {
  drawings <- arrow_grob_drawings(plot)
  edges <- purrr::map(drawings, \(drawn) {
    purrr::map(seq_along(drawn$paths), \(k) {
      path <- drawn$paths[[k]]
      n <- length(path$x)
      list(
        row = data.frame(
          panel = drawn$panel,
          from_x = path$x[[1]],
          from_y = path$y[[1]],
          to_x = path$x[[n]],
          to_y = path$y[[n]],
          fins = drawn$fins[[k]],
          head = drawn$head[[k]]
        ),
        path = data.frame(x = path$x, y = path$y)
      )
    })
  }) |>
    purrr::list_flatten()
  rows <- purrr::list_rbind(purrr::map(edges, "row"))
  order <- order(
    rows$panel,
    round(rows$from_x, 6),
    round(rows$from_y, 6),
    round(rows$to_x, 6),
    round(rows$to_y, 6)
  )
  rows <- rows[order, , drop = FALSE]
  rownames(rows) <- NULL
  list(edges = rows, paths = purrr::map(edges, "path")[order])
}

# The record of the ggarrow edges `ggdag()` draws for each default scene and
# route, at the default node size.
default_resect_drawings <- function() {
  scenes <- lapply(default_resect_scenes, \(name) {
    tidy_dag <- canonical_tidy_dag(name)
    routes <- lapply(default_resect_routes, \(route) {
      arrow_drawing_record(
        ggdag(tidy_dag, edge_engine = "ggarrow", edge_route = route) +
          theme_dag()
      )
    })
    stats::setNames(routes, default_resect_routes)
  })
  stats::setNames(scenes, default_resect_scenes)
}

# The scenes whose routed ggarrow edges under an explicit cap are pinned in
# fixtures/ggarrow-explicit-cap-routes.rds: `ggdag()` of the epidemiology
# DAG at node size 30 and `ggdag_adjustment_set()` of the README DAG at the
# default size, each with a fixed `edge_cap`, routed in spline and orthogonal
# mode. An explicit cap fixes every end, so the router is handed that cap
# for every node, whatever size and shape the node is drawn at, and routes
# exactly as it did before the caps followed the nodes. Each scene is drawn
# with `theme_dag()`, so that the panel, and with it the millimetres the
# router works in, does not depend on the default theme of the session.
explicit_cap_scenes <- list(
  ten_spline_6 = \() {
    ggdag(
      epidemiology_dag(),
      node_size = 30,
      edge_engine = "ggarrow",
      edge_route = "spline",
      edge_cap = 6
    ) +
      theme_dag()
  },
  ten_orthogonal_18 = \() {
    ggdag(
      epidemiology_dag(),
      node_size = 30,
      edge_engine = "ggarrow",
      edge_route = "orthogonal",
      edge_cap = 18
    ) +
      theme_dag()
  },
  readme_spline_5 = \() {
    withr::with_options(
      list(ggdag.edge_route = "spline"),
      ggdag_adjustment_set(
        readme_dag(),
        edge_engine = "ggarrow",
        edge_cap = 5
      ) +
        theme_dag()
    )
  }
)

# The record of the ggarrow edges each explicit-cap scene draws.
explicit_cap_drawings <- function() {
  lapply(explicit_cap_scenes, \(scene) arrow_drawing_record(scene()))
}
