# ggdag 0.2.10
* Fixed bug where ggdag was using wrong `left_join()` interface with dplyr 1.1.0 (#110)

# ggdag 0.2.9
* Fixed bugs in instrumental functions for complex and non-existent IVs (#99)
* Updated URLs to new r-causal GitHub repository (#94)

# ggdag 0.2.8
* Added new function `time_ordered_coords()` for calculating time-ordered coordinates (#88)
* Addressed changes requested by CRAN, other changes in tidyverse (#87, #86, #85)

# ggdag 0.2.7
* Fixed various bugs in calculating and visualizing paths (#75)
* Fixed breaking changes from the upcoming release of ggplot2 3.4.0 (#79)
* `size` is now deprecated for `geom_dag_collider_edges()` in favor of `linewidth`, related to #79.

# ggdag 0.2.6
* Fixed CRAN errors

# ggdag 0.2.5
* Fixed issue with `dag_adjustment_sets()` when `width` option was low by extracting adjustment sets directly (#70)
* Fixed issue with `tidy_dagitty()` that missed nodes with no edges attached. May cause minor breaking changes in randomized layouts. (#68)
* Added the `limit` argument to `dag_paths`, `ggdag_paths`, and `ggdag_paths_fan` (see `dagitty::paths`) (#65).

# ggdag 0.2.4
* `tidy_dagitty()` no longer allows the dendogram layout type (#62)
* `scale_adjusted()` now correctly aligns legend types (#61)
* ggdag no longer loads ggplot2 via `Depends`, instead importing it internally (#57).

# ggdag 0.2.3
* Update vdiffr to be explicitly conditional (#51)
* Fixed bug in `activate_collider_paths()` that causes an error where a collider had a high number of ancestors (#49)
* Fixed bug in `node_equivalent_dags()` where extra columns did not get joined to new tidy DAG (#40)

# ggdag 0.2.2
* Added vdiffr tests, as well as a basic test file for every `.R` file. (#27)
* Updated roxygen2 version (#29)
* Fixed bugs in `dag_paths()` and `geom_dag_edges_fan()` (#31)
* Removed use of `use_defaults()` `ggproto` method in `GeomDAGEdge` (#33)
* Use `ggplot2::expansion()` instead of `expand_scale()` when ggplot2 version is at least `3.3.0` and removed local version of `expand_scale()` (#34)

# ggdag 0.2.1
* Fixed bug in `ggdag_collider()` where the aesthetics were flipped for the colors but not the labels (issue #15, b72e34b)
* Used `as.character.default()` directly for working with characters to avoid dispatch error when the formula.tools package is attached (issue #17, 0f32bb4)

# ggdag 0.2.0
* Fixed compatibility issue with ggraph 2.0.0 by changing to `strength` parameter in curved geoms
* Fixed join bug in `node_equivalent_class()` that didn't account for the way dagitty returns DAGs with no direction
* Fixed join bug in `node_equivalent_class()` that didn't check `to` node
* Implemented `is_false()` to avoid dependency on R 3.5.0
* improved edge lengths
* add `{}` to adjustment set names to reflect convention
* Set nodes to be unstyled by default
* Changed default themes and scales to be more like base ggplot2
* Added a `NEWS.md` file to track changes to the package.


# ggdag 0.1.0
* Initial release
