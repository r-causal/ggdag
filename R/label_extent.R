# How much room a labelled node asks of the panel it is drawn in.
#
# A label is a box of millimetres on the page: its size is a property of the
# drawn figure and not of the data, and it is settled long after the scales
# have trained. The panel therefore trains on the nodes alone, and a node near
# the edge of it has nowhere to put its label. The label is pushed far away on
# a long leader, or placement gives up and leaves it on the ink.
#
# These helpers give the labels the standing the nodes already have, by asking
# the position scales for a strip of panel beyond the outermost node wide
# enough to hold one. The strip can only be an approximation, since the device
# is unknown while the scales train, so it is deliberately a modest one: every
# millimetre reserved is white space on a figure that did not need it, and a
# figure drawn larger than the one assumed here has room to spare anyway.
#
# Only the automatic label geoms ask for the strip. They place each label in
# the panel they are given, measuring every candidate spot against the panel's
# edge, so room at the edge is room they can use. The ggrepel geoms do not
# reason about the panel at all: they push labels apart until the forces
# settle, and an emptier panel is one they push further into, which carries
# their labels away from the nodes they name rather than towards them.

# The room a label asks for beyond the node it names, in millimetres: the
# clearance the placement engine leaves at the node's edge plus one line of
# label text. One line, not a whole box: a box is wider than a panel could
# spare on both sides, and a label beside an edge node is drawn reaching back
# across the panel rather than out of it.
label_room_mm <- 8

# The panel assumed while the scales train, in millimetres. A figure drawn at
# the package's documented size, 7 by 5 inches, has a panel about 174 by 123 mm
# once the plot margins are taken off, and that is the size at which labels
# already fit. A reservation measured against it would therefore be nothing at
# all, so the assumed panel is the smaller figure that labels do not fit on,
# 4.5 by 3.5 inches. A figure drawn larger than this keeps a little white space
# it did not need; one drawn smaller still runs short, but by less.
nominal_label_panel_width_mm <- 110
nominal_label_panel_height_mm <- 85

# The expansion the reservation is credited against: `ggplot.tidy_dagitty()`
# adds a tenth of the trained span at each end of both axes, which is already
# room a label can sit in. Only what that expansion leaves short is reserved.
# A plotter that expands further than this reserves a little more than it
# needs, which is the safe direction.
nominal_label_expansion <- 0.1

# The largest share of the trained span the reservation may take at each end.
# Beyond a half the two ends meet and the fixed point below has no solution,
# so a demand that large is met with what this allows and no more.
max_label_reservation <- 0.4

# The share of the assumed panel the drawn node discs cover at which no room is
# reserved at all. A panel is a fixed size on the page, so a strip given to the
# margin is a strip taken from the drawing: the nodes crowd together by exactly
# what the edge gains. A DAG that leaves most of its panel empty can afford
# that, and a crowded one cannot, because the labels that no longer fit between
# its nodes are thrown out to the border on long leaders, which is the failure
# the room is reserved to avoid. The reservation is therefore scaled down as
# the discs fill the panel, and switched off entirely at this coverage.
#
# A tenth is where the measurements put the edge of what a drawing can spare.
# The saturated ten-node scene of the test suite covers an eighth of the
# assumed panel, and reserving any room on it lengthens its longest leader from
# 6 mm to over 40: the labels it had been fitting between its nodes no longer
# fit. Scenes of five or six nodes cover a sixteenth and lose nothing.
crowded_node_coverage <- 0.1

#' Train the position scales on the room a label needs
#'
#' Adds `xmin`, `xmax`, `ymin`, and `ymax` to a label layer's data, each a
#' strip beyond the node the row sits on, so the position scales make room for
#' the label the layer will draw there.
#'
#' The strip is the width that leaves `label_room_mm` clear beyond the node on
#' the assumed panel, and reserving it widens the panel, which in turn moves
#' what a millimetre is worth in data units. Writing `u` for the strip as a
#' share of the trained span, the drawn span is the node span plus two strips
#' and the millimetres beyond the outermost node are
#' `(u + expansion) / (1 + 2 * expansion)` of the panel, so the share that
#' meets the demand is read off directly rather than iterated to.
#'
#' @param data A label layer's data, carrying `x` and `y`.
#' @param node_size The size of the drawn node discs.
#' @return `data`, with the four extent columns added, or unchanged where
#'   neither axis has a span to measure a label against.
#' @noRd
reserve_label_room <- function(data, node_size, n_nodes) {
  if (nrow(data) == 0) {
    return(data)
  }

  radius <- node_radius_mm(node_size)
  affordable <- affordable_share(radius, n_nodes)
  if (affordable <= 0) {
    return(data)
  }

  room <- radius + label_room_mm
  x_reserve <- affordable *
    label_reservation(data$x, room, nominal_label_panel_width_mm)
  y_reserve <- affordable *
    label_reservation(data$y, room, nominal_label_panel_height_mm)
  if (x_reserve == 0 && y_reserve == 0) {
    return(data)
  }

  data$xmin <- data$x - x_reserve
  data$xmax <- data$x + x_reserve
  data$ymin <- data$y - y_reserve
  data$ymax <- data$y + y_reserve
  data
}

#' How much of the room a label asks for the drawing can afford to give
#'
#' @param radius The drawn radius of one node disc, in millimetres.
#' @param n_nodes The number of nodes the panel draws.
#' @return A multiplier between 0 and 1.
#' @noRd
affordable_share <- function(radius, n_nodes) {
  coverage <- n_nodes *
    pi *
    radius^2 /
    (nominal_label_panel_width_mm * nominal_label_panel_height_mm)

  max(0, 1 - coverage / crowded_node_coverage)
}

#' The strip of one axis a label needs, in data units
#'
#' @param values The coordinates the layer holds on that axis.
#' @param room The millimetres a label asks for beyond the outermost node.
#' @param panel The assumed length of that axis of the panel, in millimetres.
#' @return The strip to reserve at each end, in data units, or `0` where the
#'   axis has no span to measure against or the expansion already covers the
#'   room asked for.
#' @noRd
label_reservation <- function(values, room, panel) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(0)
  }

  span <- diff(range(values))
  # A flat axis is given its room by `expand_dag_plot()`, which has the other
  # axis to measure it against; there is nothing here to take a share of.
  if (span <= 0) {
    return(0)
  }

  share <- room *
    (1 + 2 * nominal_label_expansion) /
    panel -
    nominal_label_expansion
  if (share <= 0) {
    return(0)
  }
  share <- min(share, max_label_reservation)

  span * share / (1 - 2 * share)
}
