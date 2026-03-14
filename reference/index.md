# Package index

## Create and Tidy Dags

- [`tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tidy_dagitty.md)
  :

  Tidy a `dagitty` object

- [`as_tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/as_tidy_dagitty.md)
  :

  Convert objects into `tidy_dagitty` objects

- [`dagify()`](https://r-causal.github.io/ggdag/reference/dagify.md) :
  Create a dagitty DAG using R-like syntax

- [`dag()`](https://r-causal.github.io/ggdag/reference/dag.md) : Create
  a dagitty DAG

- [`coords2df()`](https://r-causal.github.io/ggdag/reference/coordinates.md)
  [`coords2list()`](https://r-causal.github.io/ggdag/reference/coordinates.md)
  : Manipulate DAG coordinates

- [`time_ordered_coords()`](https://r-causal.github.io/ggdag/reference/time_ordered_coords.md)
  : Create a time-ordered coordinate data frame

- [`` `label<-`() ``](https://r-causal.github.io/ggdag/reference/label.md)
  [`dag_label()`](https://r-causal.github.io/ggdag/reference/label.md)
  [`label()`](https://r-causal.github.io/ggdag/reference/label.md)
  [`has_labels()`](https://r-causal.github.io/ggdag/reference/label.md)
  : DAG labels

- [`simulate_data()`](https://r-causal.github.io/ggdag/reference/simulate_data.md)
  : Simulate Data from Structural Equation Model

- [`pull_dag()`](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  [`pull_dag_data()`](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  [`` `update_dag_data<-`() ``](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  [`update_dag()`](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  [`` `update_dag<-`() ``](https://r-causal.github.io/ggdag/reference/pull_dag.md)
  : Pull components from DAG objects

## Analyzing and Plotting DAGs

- [`node_canonical()`](https://r-causal.github.io/ggdag/reference/canonicalize.md)
  [`ggdag_canonical()`](https://r-causal.github.io/ggdag/reference/canonicalize.md)
  : Canonicalize a DAG
- [`node_collider()`](https://r-causal.github.io/ggdag/reference/colliders.md)
  [`ggdag_collider()`](https://r-causal.github.io/ggdag/reference/colliders.md)
  : Find colliders
- [`node_dconnected()`](https://r-causal.github.io/ggdag/reference/d_relationship.md)
  [`node_dseparated()`](https://r-causal.github.io/ggdag/reference/d_relationship.md)
  [`node_drelationship()`](https://r-causal.github.io/ggdag/reference/d_relationship.md)
  [`ggdag_drelationship()`](https://r-causal.github.io/ggdag/reference/d_relationship.md)
  [`ggdag_dseparated()`](https://r-causal.github.io/ggdag/reference/d_relationship.md)
  [`ggdag_dconnected()`](https://r-causal.github.io/ggdag/reference/d_relationship.md)
  : D-relationship between variables
- [`node_equivalent_dags()`](https://r-causal.github.io/ggdag/reference/equivalent.md)
  [`ggdag_equivalent_dags()`](https://r-causal.github.io/ggdag/reference/equivalent.md)
  [`node_equivalent_class()`](https://r-causal.github.io/ggdag/reference/equivalent.md)
  [`ggdag_equivalent_class()`](https://r-causal.github.io/ggdag/reference/equivalent.md)
  : Generating Equivalent Models
- [`node_exogenous()`](https://r-causal.github.io/ggdag/reference/exogenous.md)
  [`ggdag_exogenous()`](https://r-causal.github.io/ggdag/reference/exogenous.md)
  : Find Exogenous Variables
- [`node_instrumental()`](https://r-causal.github.io/ggdag/reference/instrumental.md)
  [`ggdag_instrumental()`](https://r-causal.github.io/ggdag/reference/instrumental.md)
  : Find Instrumental Variables
- [`node_status()`](https://r-causal.github.io/ggdag/reference/status.md)
  [`ggdag_status()`](https://r-causal.github.io/ggdag/reference/status.md)
  : Find variable status
- [`node_children()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`node_parents()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`node_ancestors()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`node_descendants()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`node_markov_blanket()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`node_adjacent()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`ggdag_children()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`ggdag_parents()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`ggdag_ancestors()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`ggdag_descendants()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`ggdag_markov_blanket()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  [`ggdag_adjacent()`](https://r-causal.github.io/ggdag/reference/variable_family.md)
  : Familial relationships between variables
- [`dag_adjustment_sets()`](https://r-causal.github.io/ggdag/reference/adjustment_sets.md)
  [`ggdag_adjustment_set()`](https://r-causal.github.io/ggdag/reference/adjustment_sets.md)
  : Covariate Adjustment Sets
- [`dag_saturate()`](https://r-causal.github.io/ggdag/reference/dag_saturate.md)
  [`dag_prune()`](https://r-causal.github.io/ggdag/reference/dag_saturate.md)
  : Saturate or prune an existing DAG
- [`` `label<-`() ``](https://r-causal.github.io/ggdag/reference/label.md)
  [`dag_label()`](https://r-causal.github.io/ggdag/reference/label.md)
  [`label()`](https://r-causal.github.io/ggdag/reference/label.md)
  [`has_labels()`](https://r-causal.github.io/ggdag/reference/label.md)
  : DAG labels
- [`dag_paths()`](https://r-causal.github.io/ggdag/reference/paths.md)
  [`ggdag_paths()`](https://r-causal.github.io/ggdag/reference/paths.md)
  [`ggdag_paths_fan()`](https://r-causal.github.io/ggdag/reference/paths.md)
  : Find Open Paths Between Variables
- [`edge_backdoor()`](https://r-causal.github.io/ggdag/reference/edge_backdoor.md)
  : Classify DAG edges as backdoor or direct
- [`control_for()`](https://r-causal.github.io/ggdag/reference/control_for.md)
  [`adjust_for()`](https://r-causal.github.io/ggdag/reference/control_for.md)
  [`ggdag_adjust()`](https://r-causal.github.io/ggdag/reference/control_for.md)
  : Adjust for variables and activate any biasing paths that result
- [`ggdag()`](https://r-causal.github.io/ggdag/reference/ggdag.md) :
  Quickly plot a DAG in ggplot2
- [`ggdag_classic()`](https://r-causal.github.io/ggdag/reference/ggdag_classic.md)
  : Quickly plot a DAG in ggplot2
- [`ggdag_defaults`](https://r-causal.github.io/ggdag/reference/ggdag_options.md)
  [`ggdag_options_set()`](https://r-causal.github.io/ggdag/reference/ggdag_options.md)
  [`ggdag_options_get()`](https://r-causal.github.io/ggdag/reference/ggdag_options.md)
  [`ggdag_options_reset()`](https://r-causal.github.io/ggdag/reference/ggdag_options.md)
  [`ggdag_option()`](https://r-causal.github.io/ggdag/reference/ggdag_options.md)
  [`ggdag_option_proportional()`](https://r-causal.github.io/ggdag/reference/ggdag_options.md)
  : Global DAG Options
- [`query_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  [`test_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  [`ggdag_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  : Query and Test Conditional Independence in a DAG
- [`m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`butterfly_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`confounder_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`collider_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`mediation_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_confounder()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_mediator()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_time_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_butterfly_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_confounder_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_collider_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_mediation_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_confounder()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_mediator()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_time_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  : Quickly create a DAGs with common structures of bias
- [`activate_collider_paths()`](https://r-causal.github.io/ggdag/reference/activate_collider_paths.md)
  : Activate paths opened by stratifying on a collider
- [`is_collider()`](https://r-causal.github.io/ggdag/reference/is_collider.md)
  [`is_downstream_collider()`](https://r-causal.github.io/ggdag/reference/is_collider.md)
  : Detecting colliders in DAGs
- [`is_confounder()`](https://r-causal.github.io/ggdag/reference/is_confounder.md)
  : Assess if a variable confounds a relationship
- [`is_acyclic()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md)
  [`is_adjustment_set()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md)
  [`is_d_separated()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md)
  [`is_d_connected()`](https://r-causal.github.io/ggdag/reference/is_dag_properties.md)
  : Test DAG properties
- [`is_exogenous()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md)
  [`is_instrumental()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md)
  [`is_exposure()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md)
  [`is_outcome()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md)
  [`is_latent()`](https://r-causal.github.io/ggdag/reference/is_node_properties.md)
  : Test node properties
- [`is_parent()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md)
  [`is_child()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md)
  [`is_ancestor()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md)
  [`is_descendant()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md)
  [`is_adjacent()`](https://r-causal.github.io/ggdag/reference/is_node_relationships.md)
  : Test node relationships
- [`is.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/is.tidy_dagitty.md)
  : Test for object class for tidy_dagitty

## Query Functions

Direct analytical queries that return tibbles

- [`query_adjustment_sets()`](https://r-causal.github.io/ggdag/reference/query_adjustment_sets.md)
  : Query Adjustment Sets
- [`query_ancestors()`](https://r-causal.github.io/ggdag/reference/query_ancestors.md)
  : Query Node Ancestors
- [`query_children()`](https://r-causal.github.io/ggdag/reference/query_children.md)
  : Query Node Children
- [`query_colliders()`](https://r-causal.github.io/ggdag/reference/query_colliders.md)
  : Query Collider Nodes
- [`query_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  [`test_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  [`ggdag_conditional_independence()`](https://r-causal.github.io/ggdag/reference/query_conditional_independence.md)
  : Query and Test Conditional Independence in a DAG
- [`query_dconnected()`](https://r-causal.github.io/ggdag/reference/query_dconnected.md)
  : Query D-connection
- [`query_descendants()`](https://r-causal.github.io/ggdag/reference/query_descendants.md)
  : Query Node Descendants
- [`query_dseparated()`](https://r-causal.github.io/ggdag/reference/query_dseparated.md)
  : Query D-separation
- [`query_exogenous()`](https://r-causal.github.io/ggdag/reference/query_exogenous.md)
  : Query Exogenous Variables
- [`query_instrumental()`](https://r-causal.github.io/ggdag/reference/query_instrumental.md)
  : Query Instrumental Variables
- [`query_markov_blanket()`](https://r-causal.github.io/ggdag/reference/query_markov_blanket.md)
  : Query Markov Blanket
- [`query_parents()`](https://r-causal.github.io/ggdag/reference/query_parents.md)
  : Query Node Parents
- [`query_paths()`](https://r-causal.github.io/ggdag/reference/query_paths.md)
  : Query Paths in a DAG
- [`query_status()`](https://r-causal.github.io/ggdag/reference/query_status.md)
  : Query Variable Status

## ggplot2 geoms, themes, and scales

- [`geom_dag()`](https://r-causal.github.io/ggdag/reference/geom_dag.md)
  : Add common DAG layers to a ggplot
- [`geom_dag_arrow()`](https://r-causal.github.io/ggdag/reference/geom_dag_arrow.md)
  [`geom_dag_arrow_arc()`](https://r-causal.github.io/ggdag/reference/geom_dag_arrow.md)
  [`geom_dag_arrows()`](https://r-causal.github.io/ggdag/reference/geom_dag_arrow.md)
  : Directed DAG edges using ggarrow
- [`geom_dag_collider_edges()`](https://r-causal.github.io/ggdag/reference/geom_dag_collider_edges.md)
  : Edges for paths activated by stratification on colliders
- [`geom_dag_edges_link()`](https://r-causal.github.io/ggdag/reference/geom_dag_edge_functions.md)
  [`geom_dag_edges_arc()`](https://r-causal.github.io/ggdag/reference/geom_dag_edge_functions.md)
  [`geom_dag_edges_diagonal()`](https://r-causal.github.io/ggdag/reference/geom_dag_edge_functions.md)
  [`geom_dag_edges_fan()`](https://r-causal.github.io/ggdag/reference/geom_dag_edge_functions.md)
  : Directed DAG edges
- [`geom_dag_edges()`](https://r-causal.github.io/ggdag/reference/geom_dag_edges.md)
  : Directed and bidirected DAG edges
- [`geom_dag_label()`](https://r-causal.github.io/ggdag/reference/geom_dag_label.md)
  : Node text labels
- [`geom_dag_text()`](https://r-causal.github.io/ggdag/reference/geom_dag_text.md)
  : Node text
- [`geom_dag_node()`](https://r-causal.github.io/ggdag/reference/node_point.md)
  [`geom_dag_point()`](https://r-causal.github.io/ggdag/reference/node_point.md)
  : DAG Nodes
- [`geom_dag_text_repel()`](https://r-causal.github.io/ggdag/reference/repel.md)
  [`geom_dag_label_repel()`](https://r-causal.github.io/ggdag/reference/repel.md)
  [`geom_dag_label_repel2()`](https://r-causal.github.io/ggdag/reference/repel.md)
  [`geom_dag_text_repel2()`](https://r-causal.github.io/ggdag/reference/repel.md)
  : Repulsive textual annotations
- [`theme_dag_blank()`](https://r-causal.github.io/ggdag/reference/theme_dag_blank.md)
  [`theme_dag()`](https://r-causal.github.io/ggdag/reference/theme_dag_blank.md)
  [`theme_dag_grid()`](https://r-causal.github.io/ggdag/reference/theme_dag_blank.md)
  : Minimalist DAG themes
- [`theme_dag_grey()`](https://r-causal.github.io/ggdag/reference/theme_dag_grey.md)
  [`theme_dag_gray()`](https://r-causal.github.io/ggdag/reference/theme_dag_grey.md)
  [`theme_dag_grey_grid()`](https://r-causal.github.io/ggdag/reference/theme_dag_grey.md)
  [`theme_dag_gray_grid()`](https://r-causal.github.io/ggdag/reference/theme_dag_grey.md)
  : Simple grey themes for DAGs
- [`scale_adjusted()`](https://r-causal.github.io/ggdag/reference/scale_adjusted.md)
  [`scale_dag()`](https://r-causal.github.io/ggdag/reference/scale_adjusted.md)
  : Common scale adjustments for DAGs
- [`aes_dag()`](https://r-causal.github.io/ggdag/reference/aes_dag.md) :
  Define Aesthetics for Directed Acyclic Graphs (DAGs)
- [`draw_key_dag_collider()`](https://r-causal.github.io/ggdag/reference/draw_key_dag_collider.md)
  : Collider pattern legend key (many-to-one)
- [`draw_key_dag_combined()`](https://r-causal.github.io/ggdag/reference/draw_key_dag_combined.md)
  : Combined DAG legend key (horizontal node-edge-node)
- [`draw_key_dag_edge()`](https://r-causal.github.io/ggdag/reference/draw_key_dag_edge.md)
  : DAG edge legend key (arrow only)
- [`draw_key_dag_point()`](https://r-causal.github.io/ggdag/reference/draw_key_dag_point.md)
  : DAG point legend key (25% size)

## Common Structures of Bias

- [`m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`butterfly_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`confounder_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`collider_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`mediation_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_confounder()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_mediator()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`quartet_time_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_butterfly_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_confounder_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_collider_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_mediation_triangle()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_confounder()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_mediator()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_m_bias()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  [`ggdag_quartet_time_collider()`](https://r-causal.github.io/ggdag/reference/quick_plot.md)
  : Quickly create a DAGs with common structures of bias

## Misc

- [`as.data.frame(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/as.data.frame.tidy_dagitty.md)
  :

  Convert a `tidy_dagitty` object to data.frame

- [`as_tbl_graph(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/as_tbl_graph.md)
  [`as_tbl_graph(`*`<dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/as_tbl_graph.md)
  : Convert DAGS to tidygraph

- [`select(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`filter(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`mutate(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`summarise(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`arrange(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`group_by(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`ungroup(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`transmute(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`distinct(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`full_join(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`inner_join(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`left_join(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`right_join(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`anti_join(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`semi_join(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`slice(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`select_.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`filter_.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`mutate_.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`summarise_.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`arrange_.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  [`slice_.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/dplyr.md)
  :

  Dplyr verb methods for `tidy_dagitty` objects

- [`expand_plot()`](https://r-causal.github.io/ggdag/reference/expand_plot.md)
  : Quickly scale the size of a ggplot

- [`fortify(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/fortify.md)
  [`fortify(`*`<dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/fortify.md)
  :

  Fortify a `tidy_dagitty` object for `ggplot2`

- [`ggplot(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/ggplot.tidy_dagitty.md)
  [`ggplot(`*`<dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/ggplot.tidy_dagitty.md)
  : Create a new ggplot

- [`print(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/print.tidy_dagitty.md)
  :

  Print a `tidy_dagitty`

- [`remove_axes()`](https://r-causal.github.io/ggdag/reference/remove_axes.md)
  [`remove_grid()`](https://r-causal.github.io/ggdag/reference/remove_axes.md)
  : Quickly remove plot axes and grids

- [`as.tbl.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/as.tbl.tidy_dagitty.md)
  [`as_tibble(`*`<tidy_dagitty>`*`)`](https://r-causal.github.io/ggdag/reference/as.tbl.tidy_dagitty.md)
  :

  Convert a `tidy_dagitty` object to tbl

- [`tbl_df.tidy_dagitty()`](https://r-causal.github.io/ggdag/reference/tbl_df.tidy_dagitty.md)
  :

  Convert a `tidy_dagitty` object to tbl_df
