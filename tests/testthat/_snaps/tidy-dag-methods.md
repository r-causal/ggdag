# tidy_dagitty print output snapshots

    Code
      tidy_dag1
    Output
      # DAG:
      # A `dagitty` DAG with: 2 nodes and 1 edge
      #
      # Data:
      # A tibble: 2 x 7
        name      x     y direction to     xend  yend
        <chr> <dbl> <dbl> <fct>     <chr> <dbl> <dbl>
      1 x         0     1 ->        y         0     0
      2 y         0     0 <NA>      <NA>     NA    NA
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      tidy_dag2
    Output
      # DAG:
      # A `dagitty` DAG with: 3 nodes and 3 edges
      # Exposure: x
      # Outcome: y
      #
      # Data:
      # A tibble: 4 x 7
        name          x      y direction to      xend   yend
        <chr>     <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl>
      1 x     -4.99e- 1  0.288 ->        y      0.499  0.288
      2 y      4.99e- 1  0.288 <NA>      <NA>  NA     NA    
      3 z     -3.46e-10 -0.576 ->        x     -0.499  0.288
      4 z     -3.46e-10 -0.576 ->        y      0.499  0.288
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      tidy_dag3
    Output
      # DAG:
      # A `dagitty` DAG with: 3 nodes and 3 edges
      # Latent Variable: u
      #
      # Data:
      # A tibble: 4 x 7
        name          x      y direction to         xend   yend
        <chr>     <dbl>  <dbl> <fct>     <chr>     <dbl>  <dbl>
      1 u     -4.99e- 1  0.288 ->        x      4.99e- 1  0.288
      2 u     -4.99e- 1  0.288 ->        y     -3.46e-10 -0.576
      3 x      4.99e- 1  0.288 ->        y     -3.46e-10 -0.576
      4 y     -3.46e-10 -0.576 <NA>      <NA>  NA        NA    
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      tidy_dag4
    Output
      # DAG:
      # A `dagitty` DAG with: 3 nodes and 3 edges
      # Paths opened by conditioning on a collider: x <-> y, x <-> y
      #
      # Data:
      # A tibble: 6 x 8
        name          x      y direction to         xend   yend collider_line
        <chr>     <dbl>  <dbl> <fct>     <chr>     <dbl>  <dbl> <lgl>        
      1 m     -4.98e- 1 -0.288 <NA>      <NA>  NA        NA     FALSE        
      2 x      4.98e- 1 -0.288 ->        m     -4.98e- 1 -0.288 FALSE        
      3 x      4.98e- 1 -0.288 ->        y     -3.43e-10  0.576 FALSE        
      4 y     -3.43e-10  0.576 ->        m     -4.98e- 1 -0.288 FALSE        
      5 x      4.98e- 1 -0.288 <->       y     -3.43e-10  0.576 TRUE         
      6 x      4.98e- 1 -0.288 <->       y     -3.43e-10  0.576 TRUE         
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      tidy_complex
    Output
      # DAG:
      # A `dagitty` DAG with: 6 nodes and 8 edges
      # Exposure: x
      # Outcome: y
      # Latent Variable: u
      #
      # Data:
      # A tibble: 9 x 8
        name       x      y direction to      xend   yend label
        <chr>  <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>
      1 m     -0.410  1.08  <NA>      <NA>  NA     NA     M    
      2 u     -1.93  -0.650 ->        y     -0.590 -0.117 U    
      3 w      1.70  -0.110 ->        x      0.538  0.374 W    
      4 w      1.70  -0.110 ->        z      0.684 -0.577 W    
      5 x      0.538  0.374 ->        m     -0.410  1.08  X    
      6 x      0.538  0.374 ->        y     -0.590 -0.117 X    
      7 y     -0.590 -0.117 ->        m     -0.410  1.08  Y    
      8 z      0.684 -0.577 ->        x      0.538  0.374 Z    
      9 z      0.684 -0.577 ->        y     -0.590 -0.117 Z    
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

# dag_adjustment_sets print output snapshots

    Code
      adj_sets1
    Output
      # DAG:
      # A `dagitty` DAG with: 3 nodes and 3 edges
      # Exposure: x
      # Outcome: y
      # Adjustment sets: 1 set: {z}
      #
      # Data:
      # A tibble: 4 x 9
        name          x      y direction to      xend   yend adjusted   set  
        <chr>     <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>      <chr>
      1 x      5.02e- 1  0.290 ->        y     -0.502  0.290 unadjusted {z}  
      2 y     -5.02e- 1  0.290 <NA>      <NA>  NA     NA     unadjusted {z}  
      3 z     -2.29e-11 -0.579 ->        x      0.502  0.290 adjusted   {z}  
      4 z     -2.29e-11 -0.579 ->        y     -0.502  0.290 adjusted   {z}  
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      adj_sets2
    Output
      # DAG:
      # A `dagitty` DAG with: 7 nodes and 11 edges
      # Exposure: x
      # Outcome: y
      # Adjustment sets: 3 sets: {w1, w2, z2}, {v, w1}, {w1, z1}
      #
      # Data:
      # A tibble: 36 x 9
         name       x      y direction to      xend   yend adjusted   set         
         <chr>  <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>      <chr>       
       1 v     -0.517  1.39  ->        z1     0.734  0.750 unadjusted {w1, w2, z2}
       2 v     -0.517  1.39  ->        z2    -1.16   0.187 unadjusted {w1, w2, z2}
       3 w1     0.579 -0.690 ->        x      1.28  -0.173 adjusted   {w1, w2, z2}
       4 w1     0.579 -0.690 ->        y     -0.128 -0.437 adjusted   {w1, w2, z2}
       5 w1     0.579 -0.690 ->        z1     0.734  0.750 adjusted   {w1, w2, z2}
       6 w1     0.579 -0.690 <->       w2    -0.785 -1.03  adjusted   {w1, w2, z2}
       7 w2    -0.785 -1.03  ->        y     -0.128 -0.437 adjusted   {w1, w2, z2}
       8 w2    -0.785 -1.03  ->        z2    -1.16   0.187 adjusted   {w1, w2, z2}
       9 x      1.28  -0.173 ->        y     -0.128 -0.437 unadjusted {w1, w2, z2}
      10 y     -0.128 -0.437 <NA>      <NA>  NA     NA     unadjusted {w1, w2, z2}
      # i 26 more rows
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      adj_sets3
    Output
      # DAG:
      # A `dagitty` DAG with: 2 nodes and 1 edge
      # Exposure: x
      # Outcome: y
      # Adjustment sets: 0 (Backdoor paths unconditionally closed)
      #
      # Data:
      # A tibble: 2 x 9
        name      x     y direction to     xend  yend adjusted   set                  
        <chr> <dbl> <dbl> <fct>     <chr> <dbl> <dbl> <chr>      <chr>                
      1 x         0     1 ->        y         0     0 unadjusted {(Backdoor Paths Unc~
      2 y         0     0 <NA>      <NA>     NA    NA unadjusted {(Backdoor Paths Unc~
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

# dag_paths print output snapshots

    Code
      paths1
    Output
      # DAG:
      # A `dagitty` DAG with: 3 nodes and 3 edges
      # Exposure: x
      # Outcome: y
      # Paths: 2 open paths: {x -> y}, {x <- z -> y}
      #
      # Data:
      # A tibble: 9 x 10
        set   name          x      y direction to      xend   yend path      path_type
        <chr> <chr>     <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>     <chr>    
      1 1     x     -4.99e- 1  0.288 ->        y      0.499  0.288 open path direct   
      2 1     y      4.99e- 1  0.288 <NA>      <NA>  NA     NA     open path direct   
      3 1     z     -3.46e-10 -0.576 ->        x     -0.499  0.288 <NA>      <NA>     
      4 1     z     -3.46e-10 -0.576 ->        y      0.499  0.288 <NA>      <NA>     
      5 2     x     -4.99e- 1  0.288 ->        y      0.499  0.288 <NA>      <NA>     
      6 2     y      4.99e- 1  0.288 <NA>      <NA>  NA     NA     open path backdoor 
      7 2     z     -3.46e-10 -0.576 ->        x     -0.499  0.288 open path backdoor 
      8 2     z     -3.46e-10 -0.576 ->        y      0.499  0.288 open path backdoor 
      9 2     x     -4.99e- 1  0.288 <NA>      <NA>  NA     NA     open path backdoor 
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      paths2
    Output
      # DAG:
      # A `dagitty` DAG with: 4 nodes and 5 edges
      # Exposure: x
      # Outcome: y
      # Paths: 3 open paths: {x -> y}, {x <- z -> y}, {x <- z <- w -> y}
      #
      # Data:
      # A tibble: 20 x 10
         set   name         x         y direction to        xend      yend path     
         <chr> <chr>    <dbl>     <dbl> <fct>     <chr>    <dbl>     <dbl> <chr>    
       1 1     w      0.988   -6.81e-10 ->        y      0.00157 -4.62e- 1 <NA>     
       2 1     w      0.988   -6.81e-10 ->        z      0.00157  4.62e- 1 <NA>     
       3 1     x     -0.991    8.98e-10 ->        y      0.00157 -4.62e- 1 open path
       4 1     y      0.00157 -4.62e- 1 <NA>      <NA>  NA       NA        open path
       5 1     z      0.00157  4.62e- 1 ->        x     -0.991    8.98e-10 <NA>     
       6 1     z      0.00157  4.62e- 1 ->        y      0.00157 -4.62e- 1 <NA>     
       7 2     w      0.988   -6.81e-10 ->        y      0.00157 -4.62e- 1 <NA>     
       8 2     w      0.988   -6.81e-10 ->        z      0.00157  4.62e- 1 <NA>     
       9 2     x     -0.991    8.98e-10 ->        y      0.00157 -4.62e- 1 <NA>     
      10 2     y      0.00157 -4.62e- 1 <NA>      <NA>  NA       NA        open path
      11 2     z      0.00157  4.62e- 1 ->        x     -0.991    8.98e-10 open path
      12 2     z      0.00157  4.62e- 1 ->        y      0.00157 -4.62e- 1 open path
      13 2     x     -0.991    8.98e-10 <NA>      <NA>  NA       NA        open path
      14 3     w      0.988   -6.81e-10 ->        y      0.00157 -4.62e- 1 open path
      15 3     w      0.988   -6.81e-10 ->        z      0.00157  4.62e- 1 open path
      16 3     x     -0.991    8.98e-10 ->        y      0.00157 -4.62e- 1 <NA>     
      17 3     y      0.00157 -4.62e- 1 <NA>      <NA>  NA       NA        open path
      18 3     z      0.00157  4.62e- 1 ->        x     -0.991    8.98e-10 open path
      19 3     z      0.00157  4.62e- 1 ->        y      0.00157 -4.62e- 1 <NA>     
      20 3     x     -0.991    8.98e-10 <NA>      <NA>  NA       NA        open path
      # i 1 more variable: path_type <chr>
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      paths3
    Output
      # DAG:
      # A `dagitty` DAG with: 2 nodes and 1 edge
      # Exposure: x
      # Outcome: y
      # Paths: 1 open path: {x -> y}
      #
      # Data:
      # A tibble: 2 x 10
        set   name      x     y direction to     xend  yend path      path_type
        <chr> <chr> <dbl> <dbl> <fct>     <chr> <dbl> <dbl> <chr>     <chr>    
      1 1     x         0     1 ->        y         0     0 open path direct   
      2 1     y         0     0 <NA>      <NA>     NA    NA open path direct   
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

