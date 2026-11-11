# add_scm_effect() appends validated effects to the config

    Code
      writeLines(written[grep("^\\[covariates\\]", written):length(written)])
    Output
      [covariates]
      fixed = 0
      continuous = { initial = 0.1 }
      categorical = { initial = 1 }
      effects = [
          { name = "WT_CL" },
          { name = "CRCL_CL" },
          { name = "WT_V", type = "categorical", initial = 1.0, lower = 0.0, upper = 5.0 },
          { name = "AGE_CL", fixed = 0.0 },
          { name = "SEX_CL" },
          { name = "SEX_V" }
      ]

# init_scm() writes the starter config and returns its path

    Code
      config <- init_scm(p$model)
    Message
      v SCM config written: 'model/scm/1001scm.toml'
      i SCM directory: 'model/scm/1001'
      ! No effects yet: pass `effects` to `init_scm()` or add them with `add_scm_effect()`.
      Read it with `read_scm()`.
      `check_scm()` shows what a submit would do; `submit_scm_to_slurm()` runs it.

# init_scm() reports a config that already exists and returns its path

    Code
      config <- init_scm(p$model)
    Message
      i SCM config already exists: 'model/scm/1001scm.toml'
      `read_scm()` reads it; `overwrite = TRUE` replaces it.

# get_scm_status() reads a planned process

    Code
      print(status)
    Message
      
      
      -- SCM Status: extdata/models/scm/scm/1001 -------------------------------------
      Model: extdata/models/scm/1001.mod
      Status: planned · no rounds complete
      Direction: forward -> backward
      Alphas: forward = 0.05, backward = 0.001
      Covariance step: off in the round fits
      Candidates: WT_CL, CRCL_CL, WT_V
      Note: the SCM process has not started; submit its config to start it

# summary() renders the long, timing and files views

    Code
      print(summary(comp, round = "forward_round2", candidate = "WT_V", long = TRUE))
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
      
      -- Forward round 2 --
      
      v added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
      Reference: forward_round1/1001_wt_cl.mod (OFV 980.000)
      Retained before: WT_CL
      Retained after: WT_CL, CRCL_CL
      
      Candidate  OFV      dOFV    p-value  Significant  Estimate     95% CI  df   Attempts
      ─────────  ───────  ──────  ───────  ───────────  ───────────  ──────  ───  ────────
      WT_V       978.500  -1.500  0.221    No           0.050 (N/A)  N/A     1    2       
      
      -- Attempts 
      
      Candidate  Model                              Outcome  
      ─────────  ─────────────────────────────────  ─────────
      WT_V       forward_round2/1001_wt_v.mod       no ofv   
      WT_V       forward_round2/1001_wt_v_try2.mod  succeeded

# get_scm_lineage() builds the tree of the process's fits from its record

    Code
      print(tree)
    Message
      
      
      -- Hyperion Model Tree ---------------------------------------------------------
      i Models: 12
      
    Output
      extdata/models/scm-complete/scm/1001/base/1001_base base model; covariates: none
      +-extdata/models/scm-complete/scm/1001/forward_round1/1001_wt_cl add WT_CL; covariates: WT_CL
      | +-extdata/models/scm-complete/scm/1001/forward_round2/1001_crcl_cl add CRCL_CL; covariates: WT...
      | | +-extdata/models/scm-complete/scm/1001/forward_round3/1001_wt_v add WT_V; covariates: WT_CL...
      | | +-extdata/models/scm-complete/scm/1001/backward_round1/1001_wt_cl drop WT_CL; covariates: CRC...
      | | \-extdata/models/scm-complete/scm/1001/backward_round1/1001_crcl_cl drop CRCL_CL; covariates: W...
      | |   \-extdata/models/scm-complete/scm/1001/backward_round2/1001_wt_cl drop WT_CL; covariates: none
      | \-extdata/models/scm-complete/scm/1001/forward_round2/1001_wt_v_try2 add WT_V; covariates: WT_CL...
      +-extdata/models/scm-complete/scm/1001/forward_round1/1001_crcl_cl add CRCL_CL; covariates: CR...
      +-extdata/models/scm-complete/scm/1001/forward_round1/1001_wt_v add WT_V; covariates: WT_V
      +-extdata/models/scm-complete/scm/1001/working_full/1001_scm_working_full working full model; covaria...
      \-extdata/models/scm-complete/scm/1001/reduced_covariate/1001_scm_reduced_covariate reduced covariate model; co...

