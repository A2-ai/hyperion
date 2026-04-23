# hyperion.nonmem-model print works

    Code
      print(mod)
    Message
      
      
      -- NONMEM Model: 1001 ----------------------------------------------------------
      Problem: PK Structural Model
      Run Status: Not Run
      Dataset: ../../../../data/derived/PK_Oral_Ex1.csv
      Ignore: @
      Aliased Columns: ATFD→TIME, ODV→DV
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      Parameter  Initial  Lower  Fixed  Comment      
      ─────────  ───────  ─────  ─────  ─────────────
      THETA1     19       0      No     CL/F (L/h)   
      THETA2     304      0      No     VC/F (L)     
      THETA3     2        0      No     KA (1/hr)    
      THETA4     1        NA     Yes    F1 (fraction)
       
    Message
      -- Omega Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment    
      ──────────  ───────  ─────  ───────────
      OMEGA(1,1)  0.1      No     OM1 CL :EXP
      OMEGA(2,2)  0.1      No     OM2 VC :EXP
      OMEGA(3,3)  0.1      No     OM3 KA :EXP
       
    Message
      -- Sigma Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment
      ──────────  ───────  ─────  ───────
      SIGMA(1,1)  0.1      No     SIG1   
      SIGMA(2,2)  2        No     SIG2   

---

    Code
      print(mod)
    Message
      
      
      -- NONMEM Model: everything ----------------------------------------------------
      Problem: Some header #2
      Run Status: Not Run
      Dataset: ..\path with spaces\data.csv
      Ignore: #, DVID.EQ.3, ID.EQ.3.14, DVID.EQ.3, AGE.GE.18, AGE.GT.3, AGE.LT.100,
      AGE.LE.65, TYPE.NE.0, TYPE.EQ.1, TYPE.EQN.1, TYPE.NEN.2, TYPE.EQ.1
      Records: 200
      Dropped Columns: DATE
      Aliased Columns: DOSE→AMT
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      Parameter  Initial  Lower  Upper  Fixed  Comment                          
      ─────────  ───────  ─────  ─────  ─────  ─────────────────────────────────
      THETA1     1.5      NA     NA     No     THETA(1) and THETA(2)            
      THETA2     0.5      0      2      No     THETA(1) and THETA(2)            
      THETA3     0.5      -Inf   10     No     THETA with -INF lower bound      
      THETA4     5        0      Inf    No     THETA with INF upper bound       
      THETA5     0.1      0      NA     No     Three identical THETAs           
      THETA6     0.1      0      NA     No     Three identical THETAs           
      THETA7     0.1      0      NA     No     Three identical THETAs           
      THETA8     1.5      0      10     No     Named THETA                      
      THETA9     0.5      0      NA     No     NAMES syntax                     
      THETA10    10       0      NA     No     NAMES syntax                     
      THETA11    2        0      NA     No     NAMES syntax                     
      THETA12    1.1      1      NA     No     Three identical THETAs with NAMES
      THETA13    1.1      1      NA     No     Three identical THETAs with NAMES
      THETA14    1.1      1      NA     No     Three identical THETAs with NAMES
      THETA15    2.3      NA     NA     Yes    THETA(3)                         
      THETA16    0.8      NA     NA     No     THETA(4) and THETA(5)            
      THETA17    0.25     NA     NA     No     THETA(4) and THETA(5)            
      THETA18    2.3      1      NA     Yes    THETA(6)                         
      THETA19    0.75     NA     NA     Yes    THETA(7)                         
       
    Message
      -- Omega Parameters --
      
    Output
       
      Parameter     Initial  Fixed  Parametrization  Comment                                    
      ────────────  ───────  ─────  ───────────────  ───────────────────────────────────────────
      OMEGA(1,1)    0.04     No                      ETA(1) - CL (diagonal)                     
      OMEGA(2,2)    0.17     No                                                                 
      OMEGA(3,3)    0.2      No     Correlation      ETA(2) - V (SD)                            
      OMEGA(4,3)    0.3      No     Correlation      ETA(2)-ETA(3) correlation, ETA(3) - KA (SD)
      OMEGA(4,4)    0.15     No     Correlation      ETA(2)-ETA(3) correlation, ETA(3) - KA (SD)
      OMEGA(5,5)    0.2      No     Correlation      ETA(2) - V (SD)                            
      OMEGA(6,5)    0.3      No     Correlation      ETA(2)-ETA(3) correlation, ETA(3) - KA (SD)
      OMEGA(6,6)    0.15     No     Correlation      ETA(2)-ETA(3) correlation, ETA(3) - KA (SD)
      OMEGA(7,7)    0.01121  Yes                                                                
      OMEGA(8,7)    0        Yes                                                                
      OMEGA(8,8)    0.3387   Yes                                                                
      OMEGA(9,9)    0.1      No                                                                 
      OMEGA(10,9)   0.01     No                                                                 
      OMEGA(10,10)  0.1      No                                                                 
      OMEGA(11,9)   0.01     No                                                                 
      OMEGA(11,10)  0.01     No                                                                 
      OMEGA(11,11)  0.1      No                                                                 
      OMEGA(12,9)   0.01     No                                                                 
      OMEGA(12,10)  0.01     No                                                                 
      OMEGA(12,11)  0.01     No                                                                 
      OMEGA(12,12)  0.1      No                                                                 
      OMEGA(13,13)  0.4      No                      Label=Value syntax for diagonal            
      OMEGA(14,14)  0.3      No                                                                 
      OMEGA(15,14)  0.01     No                      Label=Value syntax in block                
      OMEGA(15,15)  0.35     No                      Label=Value syntax in block                
      OMEGA(16,16)  0.03     No                                                                 
      OMEGA(17,16)  0.01     No                                                                 
      OMEGA(17,17)  0.03     No                                                                 
      OMEGA(18,16)  0.01     No                                                                 
      OMEGA(18,17)  0.01     No                                                                 
      OMEGA(18,18)  0.03     No                                                                 
      OMEGA(19,16)  0.01     No                                                                 
      OMEGA(19,17)  0.01     No                                                                 
      OMEGA(19,18)  0.01     No                                                                 
      OMEGA(19,19)  0.03     No                                                                 
      OMEGA(20,20)  0.2      No     Correlation                                                 
      OMEGA(21,20)  0.3      No     Correlation                                                 
      OMEGA(21,21)  0.15     No     Correlation                                                 
      OMEGA(22,20)  0.1      No     Correlation                                                 
      OMEGA(22,21)  0.05     No     Correlation                                                 
      OMEGA(22,22)  0.3      No     Correlation                                                 
      OMEGA(23,23)  0.2      No     Correlation                                                 
      OMEGA(24,23)  0.3      No     Correlation                                                 
      OMEGA(24,24)  0.15     No     Correlation                                                 
      OMEGA(25,23)  0.1      No     Correlation                                                 
      OMEGA(25,24)  0.05     No     Correlation                                                 
      OMEGA(25,25)  0.3      No     Correlation                                                 
      OMEGA(26,26)  6        Yes                                                                
      OMEGA(27,26)  0.005    Yes                                                                
      OMEGA(27,27)  0.3      Yes                                                                
      OMEGA(28,26)  0.001    Yes                                                                
      OMEGA(28,27)  0.002    Yes                                                                
      OMEGA(28,28)  0.1      Yes                                                                
       
    Message
      -- Sigma Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment                                     
      ──────────  ───────  ─────  ────────────────────────────────────────────
      SIGMA(1,1)  0.01     No     Proportional error variance                 
      SIGMA(2,1)  0.002    No     Prop-Add covariance, Additive error variance
      SIGMA(2,2)  0.25     No     Prop-Add covariance, Additive error variance
      SIGMA(3,3)  1        Yes                                                
      SIGMA(4,4)  0.036    No                                                 
      SIGMA(5,5)  0.04     No     Label=Value syntax for SIGMA                
      SIGMA(6,6)  0.01     No     diagonal SIGMA                              
      SIGMA(7,7)  0.02     No     diagonal SIGMA                              

---

    Code
      print(mod)
    Message
      
      
      -- NONMEM Model: example1 ------------------------------------------------------
      Problem: RUN# Example 1 (from samp5l)
      Run Status: Not Run
      Dataset: example1.csv
      Ignore: C
      Aliased Columns: DV→CONC, AMT→DOSE
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      Parameter  Initial  Lower  Fixed  Comment 
      ─────────  ───────  ─────  ─────  ────────
      THETA1     2        0.001  No     [LN(CL)]
      THETA2     2        0.001  No     [LN(V1)]
      THETA3     2        0.001  No     [LN(Q)] 
      THETA4     2        0.001  No     [LN(V2)]
       
    Message
      -- Omega Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment
      ──────────  ───────  ─────  ───────
      OMEGA(1,1)  0.15     No     [P]    
      OMEGA(2,1)  0.01     No     [F]    
      OMEGA(2,2)  0.15     No     [P]    
      OMEGA(3,1)  0.01     No     [F]    
      OMEGA(3,2)  0.01     No     [F]    
      OMEGA(3,3)  0.15     No     [P]    
      OMEGA(4,1)  0.01     No     [F]    
      OMEGA(4,2)  0.01     No     [F]    
      OMEGA(4,3)  0.01     No     [F]    
      OMEGA(4,4)  0.15     No     [P]    
       
    Message
      -- Sigma Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment
      ──────────  ───────  ─────  ───────
      SIGMA(1,1)  0.6      No     [P]    

---

    Code
      print(mod)
    Message
      
      
      -- NONMEM Model: iiv-cov -------------------------------------------------------
      Problem: PK Structural Model created from pharos see 1002_metadata.json for
      details.
      Run Status: Not Run
      Dataset: ../../data/derived/PK_Oral_Ex1.csv
      Ignore: @
      Aliased Columns: ATFD→TIME, ODV→DV
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      Parameter  Initial  Lower  Fixed  Comment         
      ─────────  ───────  ─────  ─────  ────────────────
      THETA1     19.65    0      No     1  CL/F [L/h]   
      THETA2     211      0      No     2  VC/F [L]     
      THETA3     2.18     0      No     3  KA [1/hr]    
      THETA4     1        NA     Yes    4  F1 [fraction]
      THETA5     2.5      0      No     5  Q/F [L/h]    
      THETA6     22       0      No     6  V2/F [L]     
       
    Message
      -- Omega Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment                       
      ──────────  ───────  ─────  ──────────────────────────────
      OMEGA(1,1)  0.8      No     IIV CL/F :lognormal           
      OMEGA(2,1)  0.7      No     OMEGA(2,1) Cov CL/F:V2/F ;corr
      OMEGA(2,2)  0.9      No     IIV V2/F :lognormal           
      OMEGA(3,3)  0.6      No     IIV KA :lognormal             
      OMEGA(4,4)  0        Yes    IIV Q/F :lognormal            
       
    Message
      -- Sigma Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment                        
      ──────────  ───────  ─────  ───────────────────────────────
      SIGMA(1,1)  0.068    No     11 PropErr ;Proportional [prop]
      SIGMA(2,2)  0        Yes    22 AddErr ;AddErr [ng/mL]      

---

    Code
      print(mod)
    Message
      
      
      -- NONMEM Model: iov -----------------------------------------------------------
      Problem: created from pharos see iov_metadata.json for details.
      Run Status: Not Run
      Dataset: test.csv
      Ignore: @
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      Parameter  Initial  Fixed  Comment       
      ─────────  ───────  ─────  ──────────────
      THETA1     1.75     No     KA (1/hr) :LOG
      THETA2     7.3      No     CL (L/hr) :LOG
      THETA3     4        No     V2 (L) :LOG   
      THETA4     12       No     Q (L/hr) :LOG 
      THETA5     12.2     No     V3 (L) :LOG   
      THETA6     0        Yes    F1 ([]) :LOG  
      THETA7     0.75     Yes    WT_on_CL ([]) 
       
    Message
      -- Omega Parameters --
      
    Output
       
      Parameter     Initial  Fixed  Comment    
      ────────────  ───────  ─────  ───────────
      OMEGA(1,1)    0.35     No     OM1 KA :LOG
      OMEGA(2,2)    0.15     No     OM2 CL :LOG
      OMEGA(3,3)    0.12     No     OM3 V2 :LOG
      OMEGA(4,4)    0        Yes    OM4 Q  :LOG
      OMEGA(5,5)    0.07     No     OM5 V3 :LOG
      OMEGA(6,6)    0        Yes    OM6 F1 :LOG
      OMEGA(7,7)    0.06     No     IOV :LOG   
      OMEGA(8,8)    0.06     No     IOV :LOG   
      OMEGA(9,9)    0.06     No     IOV :LOG   
      OMEGA(10,10)  0.06     No     IOV :LOG   
      OMEGA(11,11)  0.06     No     IOV :LOG   
       
    Message
      -- Sigma Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment   
      ──────────  ───────  ─────  ──────────
      SIGMA(1,1)  0.14     No     SIG1 :PROP
      SIGMA(2,2)  0.05     No     SIG2 :ADD 

---

    Code
      print(mod)
    Message
      
      
      -- NONMEM Model: multiline_table -----------------------------------------------
      Problem: Some header #2
      Run Status: Not Run
      Dataset: ..\data.csv
      Dropped Columns: DATE
      Aliased Columns: DOSE→AMT
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      Parameter  Initial  Lower  Upper  Fixed  Comment              
      ─────────  ───────  ─────  ─────  ─────  ─────────────────────
      THETA1     1.5      NA     NA     No     THETA(1) and THETA(2)
      THETA2     0.5      0      2      No     THETA(1) and THETA(2)
      THETA3     2.3      NA     NA     Yes    THETA(3)             
      THETA4     0.8      NA     NA     No     THETA(4) and THETA(5)
      THETA5     0.25     NA     NA     No     THETA(4) and THETA(5)

---

    Code
      print(mod)
    Message
      
      
      -- NONMEM Model: nmexample -----------------------------------------------------
      Problem: RUN# Example 1 (from samp5l)
      Run Status: Not Run
      Dataset: example1.csv
      Ignore: C
      Aliased Columns: DV→CONC, AMT→DOSE
    Output
       
    Message
      
      -- Theta Parameters --
      
    Output
       
      Parameter  Initial  Lower  Fixed  Comment 
      ─────────  ───────  ─────  ─────  ────────
      THETA1     2        0.001  No     [LN(CL)]
      THETA2     2        0.001  No     [LN(V1)]
      THETA3     2        0.001  No     [LN(Q)] 
      THETA4     2        0.001  No     [LN(V2)]
       
    Message
      -- Omega Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment
      ──────────  ───────  ─────  ───────
      OMEGA(1,1)  0.15     No     [P]    
      OMEGA(2,1)  0.01     No     [F]    
      OMEGA(2,2)  0.15     No     [P]    
      OMEGA(3,1)  0.01     No     [F]    
      OMEGA(3,2)  0.01     No     [F]    
      OMEGA(3,3)  0.15     No     [P]    
      OMEGA(4,1)  0.01     No     [F]    
      OMEGA(4,2)  0.01     No     [F]    
      OMEGA(4,3)  0.01     No     [F]    
      OMEGA(4,4)  0.15     No     [P]    
       
    Message
      -- Sigma Parameters --
      
    Output
       
      Parameter   Initial  Fixed  Comment
      ──────────  ───────  ─────  ───────
      SIGMA(1,1)  0.6      No     [P]    

