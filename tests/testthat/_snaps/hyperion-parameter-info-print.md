# hyperion_nonmem_parameter_info print works

    Code
      print(info)
    Message
      
      
      -- Model Parameter Info --------------------------------------------------------
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      parameter  name  display  description  unit  parameterization
      ─────────  ────  ───────  ───────────  ────  ────────────────
      THETA1     TVCL  NA       NA           L/hr  NA              
      THETA2     TVV   NA       NA           L     NA              
      THETA3     TVKA  NA       NA           1/hr  NA              
       
    Message
      -- Omega Parameters --
      
    Output
       
      parameter   name        display  description  parameterization  associated_theta
      ──────────  ──────────  ───────  ───────────  ────────────────  ────────────────
      OMEGA(1,1)  OM1 (TVCL)  NA       NA           LogNormal         TVCL            
      OMEGA(2,2)  OM2 (TVV)   NA       NA           LogNormal         TVV             
      OMEGA(3,3)  OM3 (TVKA)  NA       NA           LogNormal         TVKA            
       
    Message
      -- Sigma Parameters --
      
    Output
       
      parameter   name  display  description  unit  parameterization
      ──────────  ────  ───────  ───────────  ────  ────────────────
      SIGMA(1,1)  NA    NA       NA           NA    NA              
      SIGMA(2,2)  NA    NA       NA           NA    NA              

---

    Code
      print(info)
    Message
      
      
      -- Model Parameter Info --------------------------------------------------------
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      parameter  name  display  description  unit  parameterization
      ─────────  ────  ───────  ───────────  ────  ────────────────
      THETA1     TVCL  NA       NA           L/hr  NA              
      THETA2     TVV   NA       NA           L     NA              
      THETA3     TVKA  NA       NA           1/hr  NA              
       
    Message
      -- Omega Parameters --
      
    Output
       
      parameter   name        display  description  parameterization  associated_theta
      ──────────  ──────────  ───────  ───────────  ────────────────  ────────────────
      OMEGA(1,1)  OM1 (TVCL)  NA       NA           LogNormal         TVCL            
      OMEGA(2,2)  OM2 (TVV)   NA       NA           LogNormal         TVV             
      OMEGA(3,3)  OM3 (TVKA)  NA       NA           LogNormal         TVKA            
       
    Message
      -- Sigma Parameters --
      
    Output
       
      parameter   name  display  description  unit  parameterization
      ──────────  ────  ───────  ───────────  ────  ────────────────
      SIGMA(1,1)  NA    NA       NA           NA    NA              
      SIGMA(2,2)  NA    NA       NA           NA    NA              

---

    Code
      print(info)
    Message
      
      
      -- Model Parameter Info --------------------------------------------------------
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      parameter  name  display  description  unit  parameterization
      ─────────  ────  ───────  ───────────  ────  ────────────────
      THETA1     TVCL  NA       NA           L/hr  NA              
      THETA2     TVV   NA       NA           L     NA              
      THETA3     TVKA  NA       NA           1/hr  NA              
       
    Message
      -- Omega Parameters --
      
    Output
       
      parameter   name              display  description  parameterization  associated_theta
      ──────────  ────────────────  ───────  ───────────  ────────────────  ────────────────
      OMEGA(1,1)  OM1 (TVCL)        NA       NA           LogNormal         TVCL            
      OMEGA(2,1)  OM1,2 (TVCL:TVV)  NA       NA           LogNormal         TVCL, TVV       
      OMEGA(2,2)  OM2 (TVV)         NA       NA           LogNormal         TVV             
      OMEGA(3,3)  OM3 (TVKA)        NA       NA           LogNormal         TVKA            
       
    Message
      -- Sigma Parameters --
      
    Output
       
      parameter   name  display  description  unit  parameterization
      ──────────  ────  ───────  ───────────  ────  ────────────────
      SIGMA(1,1)  NA    NA       NA           NA    NA              
      SIGMA(2,2)  NA    NA       NA           NA    NA              

---

    Code
      print(info)
    Message
      
      
      -- Model Parameter Info --------------------------------------------------------
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      parameter  name  display  description  unit  parameterization
      ─────────  ────  ───────  ───────────  ────  ────────────────
      THETA1     TVCL  NA       NA           L/hr  NA              
      THETA2     NA    NA       NA           NA    NA              
      THETA3     TVV   NA       NA           L     NA              
      THETA4     TVKA  NA       NA           1/hr  NA              
       
    Message
      -- Omega Parameters --
      
    Output
       
      parameter   name              display  description  parameterization  associated_theta
      ──────────  ────────────────  ───────  ───────────  ────────────────  ────────────────
      OMEGA(1,1)  OM1 (TVCL)        NA       NA           LogNormal         TVCL            
      OMEGA(2,1)  OM1,2 (TVCL:TVV)  NA       NA           LogNormal         TVCL, TVV       
      OMEGA(2,2)  OM2 (TVV)         NA       NA           LogNormal         TVV             
      OMEGA(3,3)  OM3 (TVKA)        NA       NA           LogNormal         TVKA            
       
    Message
      -- Sigma Parameters --
      
    Output
       
      parameter   name  display  description  unit  parameterization
      ──────────  ────  ───────  ───────────  ────  ────────────────
      SIGMA(1,1)  NA    NA       NA           NA    NA              
      SIGMA(2,2)  NA    NA       NA           NA    NA              

# hyperion_nonmem_parameter_info works for unrun model input

    Code
      print(info)
    Message
      
      
      -- Model Parameter Info --------------------------------------------------------
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      parameter  name  display  description  unit      parameterization
      ─────────  ────  ───────  ───────────  ────────  ────────────────
      THETA1     CL/F  NA       NA           L/h       NA              
      THETA2     VC/F  NA       NA           L         NA              
      THETA3     KA    NA       NA           1/hr      NA              
      THETA4     F1    NA       NA           fraction  NA              
       
    Message
      -- Omega Parameters --
      
    Output
       
      parameter   name      display  description  parameterization  associated_theta
      ──────────  ────────  ───────  ───────────  ────────────────  ────────────────
      OMEGA(1,1)  OM1 (CL)  NA       NA           LogNormal         CL/F            
      OMEGA(2,2)  OM2 (VC)  NA       NA           LogNormal         VC/F            
      OMEGA(3,3)  OM3 (KA)  NA       NA           LogNormal         KA              
       
    Message
      -- Sigma Parameters --
      
    Output
       
      parameter   name  display  description  unit  parameterization
      ──────────  ────  ───────  ───────────  ────  ────────────────
      SIGMA(1,1)  SIG1  NA       NA           NA    NA              
      SIGMA(2,2)  SIG2  NA       NA           NA    NA              

