# print.hyperion_nonmem_scm shows the config

    Code
      print(scm)
    Message
      
      
      -- SCM: 1001 -------------------------------------------------------------------
      Model: extdata/models/scm/1001.mod
      Config: extdata/models/scm/scm/1001scm.toml
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Retries: 3 per failed fit, jittered 5%
      Covariance step: off in the round fits, on in the working full model's re-fit,
      on in the reduced covariate model's re-fit
      Rounds: all
    Output
       
    Message
      
      -- Effects --
      
    Output
       
      Effect   Type         Initial  Lower  Upper
      ───────  ───────────  ───────  ─────  ─────
      WT_CL    continuous   NA       NA     NA   
      CRCL_CL  continuous   NA       NA     NA   
      WT_V     categorical  1        0      5    

---

    Code
      print(scm)
    Message
      
      
      -- SCM: 1001 -------------------------------------------------------------------
      Model: extdata/models/scm/1001.mod
      Config: extdata/models/scm/scm/1001scm.toml
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Retries: 3 per failed fit, jittered 5%
      Covariance step: off in the round fits, on in the working full model's re-fit,
      on in the reduced covariate model's re-fit
      Rounds: all
      ! No effects yet: pass `effects` to `init_scm()` or add them with `add_scm_effect()`.

# ScmEffect prints on one line

    Code
      print(ScmEffect("WT_CL"))
    Output
      <ScmEffect> WT_CL: continuous
    Code
      print(ScmEffect("WT_V", type = "categorical", initial = 1, lower = 0, upper = 5))
    Output
      <ScmEffect> WT_V: categorical, initial = 1, lower = 0, upper = 5

# print methods for the SCM records lay them out as facts and tables

    Code
      print(s)
    Message
      
      
      -- SCM Summary: extdata/models/scm-complete/scm/1001 ---------------------------
      Model: extdata/models/scm-complete/1001.mod
      Status: completed · 5 rounds complete
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Covariance step: off in the round fits
      Candidates: WT_CL, CRCL_CL, WT_V
      Path: +WT_CL (forward 1) -> +CRCL_CL (forward 2) -> -CRCL_CL (backward 1) =>
      WT_CL
      
      -- Models --
      
      Model                    File                                              OFV       Covariates    
      ───────────────────────  ────────────────────────────────────────────────  ────────  ──────────────
      Base model               base/1001_base.mod                                1000.000  none          
      Working full model       working_full/1001_scm_working_full.mod            973.800   WT_CL, CRCL_CL
      Reduced covariate model  reduced_covariate/1001_scm_reduced_covariate.mod  979.500   WT_CL         
      
      -- Forward round 1 --
      
      v added WT_CL (p = 7.744e-6, dOFV = -20.000)
      Reference: base/1001_base.mod (OFV 1000.000)
      Retained: WT_CL
      
      Candidate  OFV      dOFV     p-value   Significant  Result
      ─────────  ───────  ───────  ────────  ───────────  ──────
      WT_CL      980.000  -20.000  7.74e-06  Yes          added 
      CRCL_CL    996.000  -4.000   0.046     Yes                
      WT_V       999.000  -1.000   0.317     No                 
      
      -- Forward round 2 --
      
      v added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
      Reference: forward_round1/1001_wt_cl.mod (OFV 980.000)
      Retained: WT_CL, CRCL_CL
      
      Candidate  OFV      dOFV    p-value  Significant  Result  Attempts
      ─────────  ───────  ──────  ───────  ───────────  ──────  ────────
      CRCL_CL    974.000  -6.000  0.014    Yes          added           
      WT_V       978.500  -1.500  0.221    No                   2       
      
      -- Forward round 3 --
      
      i no candidate significant at alpha = 0.05; forward selection stopped
      Reference: forward_round2/1001_crcl_cl.mod (OFV 974.000)
      Retained: WT_CL, CRCL_CL
      
      Candidate  OFV      dOFV    p-value  Significant
      ─────────  ───────  ──────  ───────  ───────────
      WT_V       973.000  -1.000  0.317    No         
      
      -- Backward round 1 --
      
      v dropped CRCL_CL (p = 1.431e-2, dOFV = +6.000)
      Reference: forward_round2/1001_crcl_cl.mod (OFV 974.000)
      Retained: WT_CL
      
      Candidate  OFV      dOFV     p-value   Significant  Result 
      ─────────  ───────  ───────  ────────  ───────────  ───────
      CRCL_CL    980.000  +6.000   0.014     No           dropped
      WT_CL      995.000  +21.000  4.59e-06  Yes          kept   
      
      -- Backward round 2 --
      
      i every covariate significant at alpha = 0.001; backward elimination stopped
      Reference: backward_round1/1001_crcl_cl.mod (OFV 980.000)
      Retained: WT_CL
      
      Candidate  OFV       dOFV     p-value   Significant  Result
      ─────────  ────────  ───────  ────────  ───────────  ──────
      WT_CL      1000.000  +20.000  7.74e-06  Yes          kept  
      
      ! 2 candidate fits were unusable; see the round tables above.

---

    Code
      print(get_scm_status(comp))
    Message
      
      
      -- SCM Status: extdata/models/scm-complete/scm/1001 ----------------------------
      Model: extdata/models/scm-complete/1001.mod
      Status: completed · 5 rounds complete
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Covariance step: off in the round fits
      Candidates: WT_CL, CRCL_CL, WT_V
      
      -- Models --
      
      Model                    File                                              OFV       Covariates    
      ───────────────────────  ────────────────────────────────────────────────  ────────  ──────────────
      Base model               base/1001_base.mod                                1000.000  none          
      Working full model       working_full/1001_scm_working_full.mod            973.800   WT_CL, CRCL_CL
      Reduced covariate model  reduced_covariate/1001_scm_reduced_covariate.mod  979.500   WT_CL         
      
      -- Forward round 1 --
      
      v added WT_CL (p = 7.744e-6, dOFV = -20.000)
      Reference: base/1001_base.mod (OFV 1000.000)
      Retained: WT_CL
      
      Candidate  OFV      dOFV     p-value   Significant  Result
      ─────────  ───────  ───────  ────────  ───────────  ──────
      WT_CL      980.000  -20.000  7.74e-06  Yes          added 
      CRCL_CL    996.000  -4.000   0.046     Yes                
      WT_V       999.000  -1.000   0.317     No                 
      
      -- Forward round 2 --
      
      v added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
      Reference: forward_round1/1001_wt_cl.mod (OFV 980.000)
      Retained: WT_CL, CRCL_CL
      
      Candidate  OFV      dOFV    p-value  Significant  Result  Attempts
      ─────────  ───────  ──────  ───────  ───────────  ──────  ────────
      CRCL_CL    974.000  -6.000  0.014    Yes          added           
      WT_V       978.500  -1.500  0.221    No                   2       
      
      -- Forward round 3 --
      
      i no candidate significant at alpha = 0.05; forward selection stopped
      Reference: forward_round2/1001_crcl_cl.mod (OFV 974.000)
      Retained: WT_CL, CRCL_CL
      
      Candidate  OFV      dOFV    p-value  Significant
      ─────────  ───────  ──────  ───────  ───────────
      WT_V       973.000  -1.000  0.317    No         
      
      -- Backward round 1 --
      
      v dropped CRCL_CL (p = 1.431e-2, dOFV = +6.000)
      Reference: forward_round2/1001_crcl_cl.mod (OFV 974.000)
      Retained: WT_CL
      
      Candidate  OFV      dOFV     p-value   Significant  Result 
      ─────────  ───────  ───────  ────────  ───────────  ───────
      CRCL_CL    980.000  +6.000   0.014     No           dropped
      WT_CL      995.000  +21.000  4.59e-06  Yes          kept   
      
      -- Backward round 2 --
      
      i every covariate significant at alpha = 0.001; backward elimination stopped
      Reference: backward_round1/1001_crcl_cl.mod (OFV 980.000)
      Retained: WT_CL
      
      Candidate  OFV       dOFV     p-value   Significant  Result
      ─────────  ────────  ───────  ────────  ───────────  ──────
      WT_CL      1000.000  +20.000  7.74e-06  Yes          kept  

---

    Code
      print(plan)
    Message
      
      
      -- SCM Plan: extdata/models/scm-complete/scm/1001 ------------------------------
      Model: extdata/models/scm-complete/1001.mod
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Retries: 3 per failed fit, jittered 5%
      Covariance step: off in the round fits
      Working full fit: re-fit the working full model with the cov step on
      Reduced covariate fit: re-fit the reduced covariate model with the cov step on
      Rounds: all
      Max models: 13 (incl. reference fit, excl. retries)
      
      -- Candidates --
      
      Effect   Theta     Type        Initial  Fixed
      ───────  ────────  ──────────  ───────  ─────
      WT_CL    THETA(4)  continuous  0.1      0    
      CRCL_CL  THETA(5)  continuous  0.1      0    
      WT_V     THETA(6)  continuous  0.1      0    
      
      Initial: the effect's initial estimate the first time it is tested.
      Fixed: what the effect is fixed at when it is held out.
      
      -- Process in the output directory --
      
      Progress: completed — 5 rounds complete (updated [TIMESTAMP])
      Selected: WT_CL
      Reduced covariate model: reduced_covariate/1001_scm_reduced_covariate.mod
      Changes: none — identical to the config the SCM process was submitted under
      
      ! cov_step is off: the initial model's $COVARIANCE record will be removed from the round models
      x The SCM process in 'extdata/models/scm-complete/scm/1001' cannot resume under this plan; submit with `overwrite = TRUE` to discard it.

---

    Code
      print(check_scm(comp, overwrite = TRUE))
    Message
      
      
      -- SCM Plan: extdata/models/scm-complete/scm/1001 ------------------------------
      ! overwrite = TRUE: submit discards the SCM process in extdata/models/scm-complete/scm/1001 (completed, 5 rounds complete) and starts fresh
      Model: extdata/models/scm-complete/1001.mod
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Retries: 3 per failed fit, jittered 5%
      Covariance step: off in the round fits
      Working full fit: re-fit the working full model with the cov step on
      Reduced covariate fit: re-fit the reduced covariate model with the cov step on
      Rounds: all
      Max models: 13 (incl. reference fit, excl. retries)
      
      -- Candidates --
      
      Effect   Theta     Type        Initial  Fixed
      ───────  ────────  ──────────  ───────  ─────
      WT_CL    THETA(4)  continuous  0.1      0    
      CRCL_CL  THETA(5)  continuous  0.1      0    
      WT_V     THETA(6)  continuous  0.1      0    
      
      Initial: the effect's initial estimate the first time it is tested.
      Fixed: what the effect is fixed at when it is held out.
      
      ! cov_step is off: the initial model's $COVARIANCE record will be removed from the round models

# a summary prints the view it was asked for

    Code
      print(s)
    Message
      
      
      -- SCM Summary: extdata/models/scm-complete/scm/1001 ---------------------------
      Model: extdata/models/scm-complete/1001.mod
      Status: completed · 5 rounds complete
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Covariance step: off in the round fits
      Candidates: WT_CL, CRCL_CL, WT_V
      Path: +WT_CL (forward 1) -> +CRCL_CL (forward 2) -> -CRCL_CL (backward 1) =>
      WT_CL
      Time: 11 fits (1 retry) · wall 1m 30s
      
      -- Models --
      
      Model                    File                                              OFV       Covariates    
      ───────────────────────  ────────────────────────────────────────────────  ────────  ──────────────
      Base model               base/1001_base.mod                                1000.000  none          
      Working full model       working_full/1001_scm_working_full.mod            973.800   WT_CL, CRCL_CL
      Reduced covariate model  reduced_covariate/1001_scm_reduced_covariate.mod  979.500   WT_CL         
      
      -- Forward round 2 --
      
      v added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
      Reference: forward_round1/1001_wt_cl.mod (OFV 980.000)
      Retained before: WT_CL
      Retained after: WT_CL, CRCL_CL
      Time: 3 fits, wall 1m 30s
      
      Candidate  OFV      dOFV    p-value  Significant  Attempts  Est. time
      ─────────  ───────  ──────  ───────  ───────────  ────────  ─────────
      WT_V       978.500  -1.500  0.221    No           2         2s       
      
      -- Attempts 
      
      Candidate  Model                              Outcome    Est. time
      ─────────  ─────────────────────────────────  ─────────  ─────────
      WT_V       forward_round2/1001_wt_v.mod       no ofv     1s       
      WT_V       forward_round2/1001_wt_v_try2.mod  succeeded  1s       
      
      -- Files 
      
      Candidate  Run directory                  lst                                               ext                                               Summary                                          
      ─────────  ─────────────────────────────  ────────────────────────────────────────────────  ────────────────────────────────────────────────  ─────────────────────────────────────────────────
      WT_V       forward_round2/1001_wt_v_try2  forward_round2/1001_wt_v_try2/1001_wt_v_try2.lst  forward_round2/1001_wt_v_try2/1001_wt_v_try2.ext  forward_round2/1001_wt_v_try2/pharos_summary.json

# a status prints its open round fit by fit and closes with the driver

    Code
      print(status)
    Message
      
      
      -- SCM Status: extdata/models/scm-complete/scm/1001 ----------------------------
      Model: extdata/models/scm-complete/1001.mod
      Status: running · 5 rounds complete
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Covariance step: off in the round fits
      Candidates: WT_CL, CRCL_CL, WT_V
      
      -- Models --
      
      Model               File                                    OFV       Covariates    
      ──────────────────  ──────────────────────────────────────  ────────  ──────────────
      Base model          base/1001_base.mod                      1000.000  none          
      Working full model  working_full/1001_scm_working_full.mod  973.800   WT_CL, CRCL_CL
      
      -- Forward round 1 --
      
      v added WT_CL (p = 7.744e-6, dOFV = -20.000)
      Reference: base/1001_base.mod (OFV 1000.000)
      Retained: WT_CL
      
      Candidate  OFV      dOFV     p-value   Significant  Result
      ─────────  ───────  ───────  ────────  ───────────  ──────
      WT_CL      980.000  -20.000  7.74e-06  Yes          added 
      CRCL_CL    996.000  -4.000   0.046     Yes                
      WT_V       999.000  -1.000   0.317     No                 
      
      -- Forward round 2 --
      
      v added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
      Reference: forward_round1/1001_wt_cl.mod (OFV 980.000)
      Retained: WT_CL, CRCL_CL
      
      Candidate  OFV      dOFV    p-value  Significant  Result  Attempts  Status  
      ─────────  ───────  ──────  ───────  ───────────  ──────  ────────  ────────
      CRCL_CL    974.000  -6.000  0.014    Yes          added                     
      WT_V                                                      2         unusable
      
      -- Attempts 
      
      Candidate  Model                              Outcome  
      ─────────  ─────────────────────────────────  ─────────
      WT_V       forward_round2/1001_wt_v.mod       no ofv   
      WT_V       forward_round2/1001_wt_v_try2.mod  succeeded
      
      -- Forward round 3 --
      
      i no candidate significant at alpha = 0.05; forward selection stopped
      Reference: forward_round2/1001_crcl_cl.mod (OFV 974.000)
      Retained: WT_CL, CRCL_CL
      
      Candidate  OFV      dOFV    p-value  Significant
      ─────────  ───────  ──────  ───────  ───────────
      WT_V       973.000  -1.000  0.317    No         
      
      -- Backward round 1 (open) --
      
      i 1/2 done · 1 running · running 4m 10s
      i every fit has ended; submit the config again to decide the round
      
           Candidate  Progress                                           
      ───  ─────────  ───────────────────────────────────────────────────
      ✓  CRCL_CL    1m 02s                                             
      ●  WT_CL      job 4521  running 3m 08s  iteration 42, OFV 995.120
      
      x Driver: NOT RUNNING (slurm job 4500 on node01, slurm) — the SCM process stopped before finishing submit its config again to resume
      ! Allocation: slurm allocation 4499 is still held; release it with `scancel 4499`

