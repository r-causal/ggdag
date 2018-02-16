StatNodes <- ggplot2::ggproto("StatNodes", ggplot2::Stat,
                              compute_layer = function(data, scales, params) {
                                if (all(c("xend", "yend") %in% names(data))) {
                                  unique(dplyr::select(data, -xend, -yend))
                                } else {
                                  unique(data)
                                }
                              }
)

GeomDagNode <- ggplot2::ggproto("GeomDagNode", ggplot2::Geom,
                                required_aes = c("x", "y"),
                                non_missing_aes = c("size", "shape", "colour", "internal_colour"),
                                default_aes = ggplot2::aes(
                                  shape = 19, colour = "black", size = 16, fill = NA,
                                  alpha = NA, stroke = 0.5, internal_colour = "white"
                                ),

                                draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                                  coords <- coord$transform(data, panel_params)
                                  grid::gList(
                                    ggplot2:::ggname("geom_dag_node",
                                                     grid::pointsGrob(
                                                       coords$x, coords$y,
                                                       pch = coords$shape,
                                                       gp = grid::gpar(
                                                         col = alpha(coords$colour, coords$alpha),
                                                         fill = alpha(coords$fill, coords$alpha),
                                                         fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                         lwd = coords$stroke * .stroke / 2
                                                       )
                                                     )
                                    ),
                                    ggplot2:::ggname("geom_dag_node",
                                                     grid::pointsGrob(
                                                       coords$x, coords$y,
                                                       pch = coords$shape,
                                                       gp = grid::gpar(
                                                         col = alpha(coords$internal_colour, coords$alpha),
                                                         fill = alpha(coords$fill, coords$alpha),
                                                         fontsize = (coords$size - 1) * .pt + coords$stroke * .stroke / 2,
                                                         lwd = coords$stroke * .stroke / 2
                                                       )
                                                     )
                                    ),
                                    ggplot2:::ggname("geom_dag_node",
                                                     grid::pointsGrob(
                                                       coords$x, coords$y,
                                                       pch = coords$shape,
                                                       gp = grid::gpar(
                                                         col = alpha(coords$colour, coords$alpha),
                                                         fill = alpha(coords$fill, coords$alpha),
                                                         fontsize = (coords$size - 2) * .pt + coords$stroke * .stroke / 2,
                                                         lwd = coords$stroke * .stroke / 2
                                                       )
                                                     )
                                    )
                                  )
                                },

                                draw_key = ggplot2::draw_key_point
)

GeomDagText <- ggplot2::ggproto("GeomDagText", ggplot2::GeomText, default_aes = ggplot2::aes(
  colour = "white", size = 4, angle = 0, hjust = 0.5,
  vjust = 0.5, alpha = NA, family = "", fontface = "bold", lineheight = 1.2
))

StatEdgeLink <- ggplot2::ggproto('StatEdgeLink', ggraph::StatEdgeLink,
                                 setup_data = function(data, params) {
                                   data <- data[!is.na(data$direction) &
                                                  data$direction == "->", ]

                                   if (nrow(data) > 0) {
                                     data <- ggraph::StatEdgeLink$setup_data(data, params)
                                   } else {
                                     data <- NULL
                                   }
                                   data
                                 },
                                 compute_panel = function(data, scales, n = 100) {
                                   if (is.null(data)) return(data)
                                   ggraph::StatEdgeLink$compute_panel(data, scales, n = n)
                                 },
                                 default_aes = ggplot2::aes(filter = TRUE)

)

StatEdgeArc <- ggplot2::ggproto('StatEdgeArc', ggraph::StatEdgeArc,
                                setup_data = function(data, params) {

                                  data <- data[!is.na(data$direction) &
                                                 data$direction == "<->" &
                                                 !is.na(data$circular), ]

                                  if (nrow(data) > 0) {
                                    data <- ggraph::StatEdgeArc$setup_data(data, params)
                                  } else {
                                    data <- NULL
                                  }
                                  data
                                },
                                default_aes = ggplot2::aes(filter = TRUE)

)

GeomDAGEdgePath <- ggplot2::ggproto('GeomDAGEdgePath', ggraph::GeomEdgePath,
                                    use_defaults = function(self, data, params = list()) {
                                      # Fill in missing aesthetics with their defaults

                                      missing_aes <- setdiff(names(self$default_aes), names(data))
                                      if (purrr::is_empty(data)) {

                                        data <- plyr::quickdf(self$default_aes[missing_aes])
                                      } else {
                                        if ("start_cap" %in% missing_aes) data$start_cap <- ggraph::circle(8, "mm")
                                        if ("end_cap" %in% missing_aes) data$end_cap <- ggraph::circle(8, "mm")
                                        missing_aes <- missing_aes[!(missing_aes %in% c("start_cap", "end_cap"))]
                                        data[missing_aes] <- self$default_aes[missing_aes]
                                      }

                                      # Override mappings with params
                                      aes_params <- intersect(self$aesthetics(), names(params))
                                      ggplot2:::check_aesthetics(params[aes_params], nrow(data))
                                      data[aes_params] <- params[aes_params]
                                      data
                                    },
                                    non_missing_aes = c("direction", "direction_type"),
                                    default_aes = ggplot2::aes(edge_colour = 'black', edge_width = 0.6, edge_linetype = 'solid',
                                                               edge_alpha = NA, start_cap = NA, end_cap = NA, label = NA,
                                                               label_pos = 0.5, label_size = 3.88, angle = 0,
                                                               hjust = 0.5, vjust = 0.5, family = '', fontface = 1,
                                                               lineheight = 1.2, direction = "->", direction_type = "->")
)
