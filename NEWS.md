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
