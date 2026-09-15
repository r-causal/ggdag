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
        <chr> <int> <int> <fct>     <chr> <int> <int>
      1 x         1     0 ->        y         2     0
      2 y         2     0 <NA>      <NA>     NA    NA
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
        name      x      y direction to     xend   yend
        <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl>
      1 x         2  0.242 ->        y         3 -0.121
      2 y         3 -0.121 <NA>      <NA>     NA NA    
      3 z         1 -0.121 ->        x         2  0.242
      4 z         1 -0.121 ->        y         3 -0.121
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
        name      x      y direction to     xend   yend
        <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl>
      1 u         1 -0.121 ->        x         2  0.242
      2 u         1 -0.121 ->        y         3 -0.121
      3 x         2  0.242 ->        y         3 -0.121
      4 y         3 -0.121 <NA>      <NA>     NA NA    
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

---

    Code
      tidy_dag4
    Output
      # DAG:
      # A `dagitty` DAG with: 3 nodes and 3 edges
      # Paths opened by conditioning on a collider: x <-> y
      #
      # Data:
      # A tibble: 5 x 8
        name      x      y direction to     xend   yend collider_line
        <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl> <lgl>        
      1 m         3 -0.121 <NA>      <NA>     NA NA     FALSE        
      2 x         1 -0.121 ->        m         3 -0.121 FALSE        
      3 x         1 -0.121 ->        y         2  0.242 FALSE        
      4 y         2  0.242 ->        m         3 -0.121 FALSE        
      5 x         1 -0.121 <->       y         2  0.242 TRUE         
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
        name      x      y direction to     xend   yend label
        <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl> <chr>
      1 m         5  0.198 <NA>      <NA>     NA NA     M    
      2 u         3 -0.492 ->        y         4 -0.143 U    
      3 w         1  0.244 ->        x         3  0.296 W    
      4 w         1  0.244 ->        z         2 -0.103 W    
      5 x         3  0.296 ->        m         5  0.198 X    
      6 x         3  0.296 ->        y         4 -0.143 X    
      7 y         4 -0.143 ->        m         5  0.198 Y    
      8 z         2 -0.103 ->        x         3  0.296 Z    
      9 z         2 -0.103 ->        y         4 -0.143 Z    
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
        name      x      y direction to     xend   yend adjusted   set  
        <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl> <chr>      <chr>
      1 x         2  0.242 ->        y         3 -0.121 unadjusted {z}  
      2 y         3 -0.121 <NA>      <NA>     NA NA     unadjusted {z}  
      3 z         1 -0.121 ->        x         2  0.242 adjusted   {z}  
      4 z         1 -0.121 ->        y         3 -0.121 adjusted   {z}  
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
         name      x      y direction to     xend   yend adjusted   set         
         <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl> <chr>      <chr>       
       1 v         1 -0.27  ->        z1        2 -0.536 unadjusted {w1, w2, z2}
       2 v         1 -0.27  ->        z2        3  0.075 unadjusted {w1, w2, z2}
       3 w1        1  0.13  ->        x         3 -0.325 adjusted   {w1, w2, z2}
       4 w1        1  0.13  ->        y         4  0.396 adjusted   {w1, w2, z2}
       5 w1        1  0.13  ->        z1        2 -0.536 adjusted   {w1, w2, z2}
       6 w1        1  0.13  <->       w2        1  0.53  adjusted   {w1, w2, z2}
       7 w2        1  0.53  ->        y         4  0.396 adjusted   {w1, w2, z2}
       8 w2        1  0.53  ->        z2        3  0.075 adjusted   {w1, w2, z2}
       9 x         3 -0.325 ->        y         4  0.396 unadjusted {w1, w2, z2}
      10 y         4  0.396 <NA>      <NA>     NA NA     unadjusted {w1, w2, z2}
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
        <chr> <int> <int> <fct>     <chr> <int> <int> <chr>      <chr>                
      1 x         1     0 ->        y         2     0 unadjusted {(Backdoor Paths Unc~
      2 y         2     0 <NA>      <NA>     NA    NA unadjusted {(Backdoor Paths Unc~
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
        set   name      x      y direction to     xend   yend path      path_type
        <chr> <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl> <chr>     <chr>    
      1 1     x         2  0.242 ->        y         3 -0.121 open path direct   
      2 1     y         3 -0.121 <NA>      <NA>     NA NA     open path direct   
      3 1     z         1 -0.121 ->        x         2  0.242 <NA>      <NA>     
      4 1     z         1 -0.121 ->        y         3 -0.121 <NA>      <NA>     
      5 2     x         2  0.242 ->        y         3 -0.121 <NA>      <NA>     
      6 2     y         3 -0.121 <NA>      <NA>     NA NA     open path backdoor 
      7 2     z         1 -0.121 ->        x         2  0.242 open path backdoor 
      8 2     z         1 -0.121 ->        y         3 -0.121 open path backdoor 
      9 2     x         2  0.242 <NA>      <NA>     NA NA     open path backdoor 
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
         set   name      x      y direction to     xend   yend path      path_type
         <chr> <chr> <int>  <dbl> <fct>     <chr> <int>  <dbl> <chr>     <chr>    
       1 1     w         1  0.263 ->        y         4  0.2   <NA>      <NA>     
       2 1     w         1  0.263 ->        z         2 -0.129 <NA>      <NA>     
       3 1     x         3 -0.334 ->        y         4  0.2   open path direct   
       4 1     y         4  0.2   <NA>      <NA>     NA NA     open path direct   
       5 1     z         2 -0.129 ->        x         3 -0.334 <NA>      <NA>     
       6 1     z         2 -0.129 ->        y         4  0.2   <NA>      <NA>     
       7 2     w         1  0.263 ->        y         4  0.2   <NA>      <NA>     
       8 2     w         1  0.263 ->        z         2 -0.129 <NA>      <NA>     
       9 2     x         3 -0.334 ->        y         4  0.2   <NA>      <NA>     
      10 2     y         4  0.2   <NA>      <NA>     NA NA     open path backdoor 
      11 2     z         2 -0.129 ->        x         3 -0.334 open path backdoor 
      12 2     z         2 -0.129 ->        y         4  0.2   open path backdoor 
      13 2     x         3 -0.334 <NA>      <NA>     NA NA     open path backdoor 
      14 3     w         1  0.263 ->        y         4  0.2   open path backdoor 
      15 3     w         1  0.263 ->        z         2 -0.129 open path backdoor 
      16 3     x         3 -0.334 ->        y         4  0.2   <NA>      <NA>     
      17 3     y         4  0.2   <NA>      <NA>     NA NA     open path backdoor 
      18 3     z         2 -0.129 ->        x         3 -0.334 open path backdoor 
      19 3     z         2 -0.129 ->        y         4  0.2   <NA>      <NA>     
      20 3     x         3 -0.334 <NA>      <NA>     NA NA     open path backdoor 
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
        <chr> <chr> <int> <int> <fct>     <chr> <int> <int> <chr>     <chr>    
      1 1     x         1     0 ->        y         2     0 open path direct   
      2 1     y         2     0 <NA>      <NA>     NA    NA open path direct   
      #
      # i Use `pull_dag() (`?pull_dag`)` to retrieve the DAG object and `pull_dag_data() (`?pull_dag_data`)` for the data frame

