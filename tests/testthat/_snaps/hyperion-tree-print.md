# hyperion_nonmem_tree print works

    Code
      print(tree)
    Message
      
      
      -- Hyperion Model Tree ---------------------------------------------------------
      i Models: 3
      
    Output
      base Base population PK model
      \-run001 Run 1
        \-run002 Run 2 with covariate effects

# hyperion_nonmem_tree print honors verbose attr

    Code
      print(tree)
    Message
      
      
      -- Hyperion Model Tree ---------------------------------------------------------
      i Models: 2
      
    Output
      Model   Parent  Description               Tags  Model Hash   Dataset Hash
      ─────────────────────────────────────────────────────────────────────────
      base            Base population PK model  base  f873a13c...  8d8189cf... 
      run001  base    Adding COV step                                          

# hyperion_nonmem_tree verbose print renders 6-column table

    Code
      print(tree, verbose = TRUE)
    Message
      
      
      -- Hyperion Model Tree ---------------------------------------------------------
      i Models: 3
      
    Output
      Model   Parent  Description                   Tags                 Model Hash   Dataset Hash
      ────────────────────────────────────────────────────────────────────────────────────────────
      base            Base population PK model      base                 f873a13c...  8d8189cf... 
      run001  base    Adding COV step, unfixing CL  covariates, unfixed  1a0f07a1...  8d8189cf... 
      run002  run001  Not yet run                                                                 

