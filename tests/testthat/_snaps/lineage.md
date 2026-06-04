# get_model_lineage() returns the whole project tree

    Code
      get_model_lineage()
    Message
      
      
      -- Hyperion Model Tree ---------------------------------------------------------
      i Models: 10
      
    Output
      extdata/models/onecmt/run001 Base model
      +-extdata/models/onecmt/run002 Adding COV step, unfixing eps(2)
      | +-extdata/models/onecmt/run002a Some description about what makes run002a ...
      | +-extdata/models/onecmt/run002b001 Jittering initial sigma estimates, usin...
      | \-extdata/models/onecmt/run003 Jittering initial estimates
      |   +-extdata/models/onecmt/run003b1 Updating run003 to 003b1 with jittered ...
      |   +-extdata/models/onecmt/run003b2 Updating run003 with mod object
      |   \-extdata/models/twocmt/run103 Two compartment model
      +-extdata/models/onecmt/run004 Updating run001 to run004 with jittered param...
      \-extdata/models/onecmt/run005 Updating run001 to run004 with jittered param...

# get_model_lineage(model) returns the model's full lineage

    Code
      get_model_lineage("extdata/models/onecmt/run003.mod")
    Message
      
      
      -- Hyperion Model Tree ---------------------------------------------------------
      i Models: 6
      
    Output
      extdata/models/onecmt/run001 Base model
      \-extdata/models/onecmt/run002 Adding COV step, unfixing eps(2)
        \-extdata/models/onecmt/run003 Jittering initial estimates
          +-extdata/models/onecmt/run003b1 Updating run003 to 003b1 with jittered ...
          +-extdata/models/onecmt/run003b2 Updating run003 with mod object
          \-extdata/models/twocmt/run103 Two compartment model

# get_model_lineage(from, to) slices between two models

    Code
      get_model_lineage(from = "extdata/models/onecmt/run001.mod", to = "extdata/models/onecmt/run003b1.mod")
    Message
      
      
      -- Hyperion Model Tree ---------------------------------------------------------
      i Models: 4
      
    Output
      extdata/models/onecmt/run001 Base model
      \-extdata/models/onecmt/run002 Adding COV step, unfixing eps(2)
        \-extdata/models/onecmt/run003 Jittering initial estimates
          \-extdata/models/onecmt/run003b1 Updating run003 to 003b1 with jittered ...

# lineage helpers return project-relative paths

    Code
      get_model_ancestors("extdata/models/onecmt/run003b1.mod")
    Output
      [1] "extdata/models/onecmt/run001.mod"   "extdata/models/onecmt/run002.mod"  
      [3] "extdata/models/onecmt/run003.mod"   "extdata/models/onecmt/run003b1.mod"

---

    Code
      get_model_descendants("extdata/models/onecmt/run001.mod")
    Output
      [1] "extdata/models/onecmt/run002.mod"    
      [2] "extdata/models/onecmt/run002a.mod"   
      [3] "extdata/models/onecmt/run002b001.mod"
      [4] "extdata/models/onecmt/run003.mod"    
      [5] "extdata/models/onecmt/run003b1.mod"  
      [6] "extdata/models/onecmt/run003b2.mod"  
      [7] "extdata/models/onecmt/run004.mod"    
      [8] "extdata/models/onecmt/run005.mod"    
      [9] "extdata/models/twocmt/run103.mod"    

---

    Code
      are_models_in_lineage("extdata/models/onecmt/run001.mod",
        "extdata/models/onecmt/run003b1.mod")
    Output
      [1] TRUE

