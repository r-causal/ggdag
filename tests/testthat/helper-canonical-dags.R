# The 22 canonical DAGs from the layout design spec, one edge-string vector
# per DAG. Shared by the layout invariance and performance tests and by the
# fixture generator at tests/testthat/fixtures/make-layout-fixtures.R.
canonical_dag_specs <- list(
  confounding = c("z->x", "z->y", "x->y"),
  mediation = c("x->m", "m->y", "x->y"),
  collider = c("x->c", "y->c"),
  iv = c("z->x", "x->y", "u->x", "u->y"),
  front_door = c("u->x", "u->y", "x->m", "m->y"),
  m_bias = c("u1->a", "u1->m", "u2->m", "u2->y", "a->y"),
  smoking = c(
    "genetics->smoking",
    "genetics->cancer",
    "smoking->tar",
    "tar->cancer",
    "smoking->cancer"
  ),
  epidemiology = c(
    "ses->edu",
    "ses->health",
    "edu->income",
    "edu->health",
    "income->health",
    "age->ses",
    "age->health",
    "gene->health",
    "gene->ses"
  ),
  selection_bias = c("a->y", "a->s", "u->s", "u->y", "l->a", "l->u"),
  overcontrol = c("x->z", "z->y", "x->y", "w->x", "w->z"),
  napkin = c(
    "u1->z",
    "u1->a",
    "u2->a",
    "u2->y",
    "z->a",
    "a->m",
    "m->y",
    "a->y"
  ),
  butterfly = c("x1->m", "x2->m", "m->y1", "m->y2", "x1->y1", "x2->y2"),
  complex_chain = c(
    "a->b",
    "b->c",
    "c->d",
    "d->e",
    "a->c",
    "b->d",
    "c->e",
    "a->e"
  ),
  wide_dag = c(
    "x1->m1",
    "x2->m1",
    "x3->m2",
    "x1->m2",
    "m1->y",
    "m2->y",
    "x2->y",
    "x3->y"
  ),
  deep_confound = c(
    "u->a",
    "u->b",
    "u->c",
    "a->b",
    "b->c",
    "a->d",
    "c->d",
    "b->d"
  ),
  large_epi = c(
    "age->ses",
    "age->smoking",
    "age->bmi",
    "age->health",
    "ses->smoking",
    "ses->diet",
    "ses->health",
    "smoking->cancer",
    "smoking->health",
    "diet->bmi",
    "diet->health",
    "bmi->cancer",
    "bmi->health",
    "cancer->health",
    "gene->cancer",
    "gene->bmi",
    "gene->smoking"
  ),
  treatment = c(
    "c1->x",
    "c2->x",
    "c1->y",
    "c2->y",
    "x->m1",
    "x->m2",
    "m1->y",
    "m2->y",
    "u->m1",
    "u->y"
  ),
  double_iv = c(
    "z1->x",
    "z2->x",
    "x->m",
    "m->y",
    "u1->x",
    "u1->m",
    "u2->m",
    "u2->y"
  ),
  cascade = c(
    "a->b",
    "a->d",
    "b->c",
    "c->d",
    "b->e",
    "d->e",
    "c->f",
    "e->f",
    "a->f"
  ),
  triple_confound = c(
    "u->x",
    "u->y",
    "v->x",
    "v->m",
    "w->m",
    "w->y",
    "x->m",
    "m->y",
    "x->y"
  ),
  regression_disc = c(
    "z->x",
    "x->y",
    "x->w",
    "w->y",
    "z->w",
    "u->w",
    "u->y"
  ),
  multi_mediator = c(
    "x->m1",
    "x->m2",
    "x->m3",
    "m1->m2",
    "m2->m3",
    "m1->y",
    "m2->y",
    "m3->y",
    "x->y",
    "u->m2",
    "u->y"
  )
)

# Parse "a->b" strings into an edge data frame shaped like the engine's real
# input: one row per directed edge plus a to = NA row per terminal node.
canonical_dag_edges <- function(edge_strings) {
  parts <- strsplit(edge_strings, "->", fixed = TRUE)
  edges <- data.frame(
    name = vapply(parts, function(p) p[[1]], character(1)),
    to = vapply(parts, function(p) p[[2]], character(1)),
    stringsAsFactors = FALSE
  )
  terminals <- setdiff(unique(edges$to), edges$name)
  if (length(terminals) > 0) {
    edges <- rbind(
      edges,
      data.frame(name = terminals, to = NA_character_, stringsAsFactors = FALSE)
    )
  }
  edges
}

# The exact inputs compute_time_ordered_layout() hands to order_layers():
# layers from the longest-path assignment plus the split edge types.
canonical_ordering_inputs <- function(edges) {
  parts <- split_edge_types(edges)
  layer_assign <- longest_path_layers(edges)
  list(
    layer_nodes = lapply(seq(0L, max(layer_assign)), function(l) {
      names(layer_assign[layer_assign == l])
    }),
    directed = parts$directed,
    layer_assign = layer_assign,
    bidirected = parts$bidirected
  )
}

# order_layers() exactly as the wired layout path calls it.
canonical_order_layers <- function(inputs) {
  order_layers(
    inputs$layer_nodes,
    inputs$directed,
    inputs$layer_assign,
    sweeps = 8L,
    bidirected_pairs = inputs$bidirected
  )
}
