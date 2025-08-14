# tidy_dagitty print output snapshots

    Code
      tidy_dag1
    Output
      # A DAG with: 2 nodes and 1 edges
      #
      # A tibble: 2 x 7
        name       x     y direction to      xend   yend
        <chr>  <dbl> <dbl> <fct>     <chr>  <dbl>  <dbl>
      1 x     -0.611 0.166 ->        y      0.181  0.784
      2 y      0.181 0.784 <NA>      <NA>  NA     NA    

---

    Code
      tidy_dag2
    Output
      # A DAG with: 3 nodes and 3 edges
      # Exposure: x
      # Outcome: y
      #
      # A tibble: 4 x 7
        name       x       y direction to      xend    yend
        <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>   <dbl>
      1 x      0.335  0.635  ->        y     -0.469  0.0367
      2 y     -0.469  0.0367 <NA>      <NA>  NA     NA     
      3 z      0.451 -0.361  ->        x      0.335  0.635 
      4 z      0.451 -0.361  ->        y     -0.469  0.0367

---

    Code
      tidy_dag3
    Output
      # A DAG with: 3 nodes and 3 edges
      # Latent Variable: u
      #
      # A tibble: 4 x 7
        name       x       y direction to      xend    yend
        <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>   <dbl>
      1 u      0.335  0.635  ->        x     -0.469  0.0367
      2 u      0.335  0.635  ->        y      0.451 -0.361 
      3 x     -0.469  0.0367 ->        y      0.451 -0.361 
      4 y      0.451 -0.361  <NA>      <NA>  NA     NA     

---

    Code
      tidy_dag4
    Output
      # A DAG with: 3 nodes and 3 edges
      # Paths opened by conditioning on a collider: x <-> y, x <-> y
      #
      # A tibble: 6 x 8
        name       x       y direction to      xend   yend collider_line
        <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>  <dbl> <lgl>        
      1 m      0.335  0.635  <NA>      <NA>  NA     NA     FALSE        
      2 x     -0.469  0.0368 ->        m      0.335  0.635 FALSE        
      3 x     -0.469  0.0368 ->        y      0.451 -0.360 FALSE        
      4 y      0.451 -0.360  ->        m      0.335  0.635 FALSE        
      5 x     -0.469  0.0368 <->       y      0.451 -0.360 TRUE         
      6 x     -0.469  0.0368 <->       y      0.451 -0.360 TRUE         

---

    Code
      tidy_complex
    Output
      # A DAG with: 6 nodes and 8 edges
      # Exposure: x
      # Outcome: y
      # Latent Variable: u
      #
      # A tibble: 9 x 8
        name      x      y direction to     xend   yend label
        <chr> <dbl>  <dbl> <fct>     <chr> <dbl>  <dbl> <chr>
      1 m     -1.27 -0.235 <NA>      <NA>  NA    NA     M    
      2 u     -2.42 -2.24  ->        y     -2.35 -0.796 U    
      3 w     -3.11  1.36  ->        x     -2.26  0.426 W    
      4 w     -3.11  1.36  ->        z     -3.21  0.242 W    
      5 x     -2.26  0.426 ->        m     -1.27 -0.235 X    
      6 x     -2.26  0.426 ->        y     -2.35 -0.796 X    
      7 y     -2.35 -0.796 ->        m     -1.27 -0.235 Y    
      8 z     -3.21  0.242 ->        x     -2.26  0.426 Z    
      9 z     -3.21  0.242 ->        y     -2.35 -0.796 Z    

# dag_adjustment_sets print output snapshots

    Code
      adj_sets1
    Output
      # A DAG with: 3 nodes and 3 edges
      # Exposure: x
      # Outcome: y
      # Adjustment sets: 1 set: {z}
      #
      # A tibble: 4 x 9
        name      x      y direction to      xend   yend adjusted   set  
        <chr> <dbl>  <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>      <chr>
      1 x     0.114 -0.149 ->        y      0.377  0.820 unadjusted {z}  
      2 y     0.377  0.820 <NA>      <NA>  NA     NA     unadjusted {z}  
      3 z     1.08   0.107 ->        x      0.114 -0.149 adjusted   {z}  
      4 z     1.08   0.107 ->        y      0.377  0.820 adjusted   {z}  

---

    Code
      adj_sets2
    Output
      # A DAG with: 7 nodes and 11 edges
      # Exposure: x
      # Outcome: y
      # Adjustment sets: 3 sets: {w1, w2, z2}, {v, w1}, {w1, z1}
      #
      # A tibble: 36 x 9
         name      x      y direction to     xend   yend adjusted   set         
         <chr> <dbl>  <dbl> <fct>     <chr> <dbl>  <dbl> <chr>      <chr>       
       1 v      2.32  2.02  ->        z1     3.72  1.93  unadjusted {w1, w2, z2}
       2 v      2.32  2.02  ->        z2     2.23  0.663 unadjusted {w1, w2, z2}
       3 w1     4.18  0.539 ->        x      4.61  1.32  adjusted   {w1, w2, z2}
       4 w1     4.18  0.539 ->        y      3.43  0.512 adjusted   {w1, w2, z2}
       5 w1     4.18  0.539 ->        z1     3.72  1.93  adjusted   {w1, w2, z2}
       6 w1     4.18  0.539 <->       w2     3.07 -0.306 adjusted   {w1, w2, z2}
       7 w2     3.07 -0.306 ->        y      3.43  0.512 adjusted   {w1, w2, z2}
       8 w2     3.07 -0.306 ->        z2     2.23  0.663 adjusted   {w1, w2, z2}
       9 x      4.61  1.32  ->        y      3.43  0.512 unadjusted {w1, w2, z2}
      10 y      3.43  0.512 <NA>      <NA>  NA    NA     unadjusted {w1, w2, z2}
      # i 26 more rows

---

    Code
      adj_sets3
    Output
      # A DAG with: 2 nodes and 1 edges
      # Exposure: x
      # Outcome: y
      # Adjustment sets: 0 (Backdoor paths unconditionally closed)
      #
      # A tibble: 2 x 9
        name       x       y direction to      xend    yend adjusted   set            
        <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>   <dbl> <chr>      <chr>          
      1 x      0.672 -0.0740 ->        y     -0.325 -0.0608 unadjusted {(Backdoor Pat~
      2 y     -0.325 -0.0608 <NA>      <NA>  NA     NA      unadjusted {(Backdoor Pat~

# dag_paths print output snapshots

    Code
      paths1
    Output
      # A DAG with: 3 nodes and 3 edges
      # Exposure: x
      # Outcome: y
      # Paths: 2 open paths: {x -> y}, {x <- z -> y}
      #
      # A tibble: 9 x 9
        set   name       x       y direction to      xend    yend path     
        <chr> <chr>  <dbl>   <dbl> <fct>     <chr>  <dbl>   <dbl> <chr>    
      1 1     x      0.335  0.635  ->        y     -0.469  0.0367 open path
      2 1     y     -0.469  0.0367 <NA>      <NA>  NA     NA      open path
      3 1     z      0.451 -0.361  ->        x      0.335  0.635  <NA>     
      4 1     z      0.451 -0.361  ->        y     -0.469  0.0367 <NA>     
      5 2     x      0.335  0.635  ->        y     -0.469  0.0367 <NA>     
      6 2     y     -0.469  0.0367 <NA>      <NA>  NA     NA      open path
      7 2     z      0.451 -0.361  ->        x      0.335  0.635  open path
      8 2     z      0.451 -0.361  ->        y     -0.469  0.0367 open path
      9 2     x      0.335  0.635  <NA>      <NA>  NA     NA      open path

---

    Code
      paths2
    Output
      # A DAG with: 4 nodes and 5 edges
      # Exposure: x
      # Outcome: y
      # Paths: 3 open paths: {x -> y}, {x <- z -> y}, {x <- z <- w -> y}
      #
      # A tibble: 20 x 9
         set   name      x     y direction to     xend   yend path     
         <chr> <chr> <dbl> <dbl> <fct>     <chr> <dbl>  <dbl> <chr>    
       1 1     w     0.417 0.337 ->        y      1.50  0.202 <NA>     
       2 1     w     0.417 0.337 ->        z      1.21  1.08  <NA>     
       3 1     x     2.30  0.948 ->        y      1.50  0.202 open path
       4 1     y     1.50  0.202 <NA>      <NA>  NA    NA     open path
       5 1     z     1.21  1.08  ->        x      2.30  0.948 <NA>     
       6 1     z     1.21  1.08  ->        y      1.50  0.202 <NA>     
       7 2     w     0.417 0.337 ->        y      1.50  0.202 <NA>     
       8 2     w     0.417 0.337 ->        z      1.21  1.08  <NA>     
       9 2     x     2.30  0.948 ->        y      1.50  0.202 <NA>     
      10 2     y     1.50  0.202 <NA>      <NA>  NA    NA     open path
      11 2     z     1.21  1.08  ->        x      2.30  0.948 open path
      12 2     z     1.21  1.08  ->        y      1.50  0.202 open path
      13 2     x     2.30  0.948 <NA>      <NA>  NA    NA     open path
      14 3     w     0.417 0.337 ->        y      1.50  0.202 open path
      15 3     w     0.417 0.337 ->        z      1.21  1.08  open path
      16 3     x     2.30  0.948 ->        y      1.50  0.202 <NA>     
      17 3     y     1.50  0.202 <NA>      <NA>  NA    NA     open path
      18 3     z     1.21  1.08  ->        x      2.30  0.948 open path
      19 3     z     1.21  1.08  ->        y      1.50  0.202 <NA>     
      20 3     x     2.30  0.948 <NA>      <NA>  NA    NA     open path

---

    Code
      paths3
    Output
      # A DAG with: 2 nodes and 1 edges
      # Exposure: x
      # Outcome: y
      # Paths: 1 open path: {x -> y}
      #
      # A tibble: 2 x 9
        set   name       x     y direction to      xend   yend path     
        <chr> <chr>  <dbl> <dbl> <fct>     <chr>  <dbl>  <dbl> <chr>    
      1 1     x     -0.611 0.166 ->        y      0.181  0.784 open path
      2 1     y      0.181 0.784 <NA>      <NA>  NA     NA     open path

