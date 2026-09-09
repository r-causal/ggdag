# geom_dag() errors when the plot maps no DAG aesthetics

    Code
      expr
    Condition <rlang_error>
      Error in `geom_dag_point()`:
      ! Problem while setting up layer.
      i Error occurred in the 1st layer.
      Caused by error in `geom_dag()`:
      ! `geom_dag()` needs the DAG aesthetics on the plot.
      x The plot mapping does not set x and y.
      i Build the plot with `ggplot(dag, aes_dag()) + geom_dag()`.

# geom_dag() errors when the plot maps only some DAG aesthetics

    Code
      expr
    Condition <rlang_error>
      Error in `geom_dag_edges_link()`:
      ! Problem while setting up layer.
      i Error occurred in the 2nd layer.
      Caused by error in `geom_dag()`:
      ! `geom_dag()` needs the DAG aesthetics on the plot.
      x The plot mapping does not set xend and yend.
      i Build the plot with `ggplot(dag, aes_dag()) + geom_dag()`.

