# init_scm() writes the starter config and returns the object

    Code
      scm <- init_scm(p$model)
    Message
      v SCM config written: 'model/scm/1001scm.toml'
      i SCM directory: 'model/scm/1001'
      ! No effects yet: pass `effects` to `init_scm()` or add them with `add_scm_effect()`.
      `check_scm()` shows what a submit would do; `submit_scm_to_slurm()` runs it.

# write_scm() writes effects as rows and keeps untouched comments

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
          { name = "WT_V", type = "categorical", initial = 1.0, lower = 0.0, upper = 5.0 }
      ]

# get_scm_status() reads a planned process

    Code
      print(status)
    Message
      
      
      -- SCM Status: extdata/models/scm/scm/1001 -------------------------------------
    Output
      model        : extdata/models/scm/1001.mod
      status       : planned · no rounds complete
      direction    : forward -> backward
      alphas       : forward = 0.05, backward = 0.001
      cov step     : off
      candidates   : WT_CL, CRCL_CL, WT_V
      note         : the SCM process has not started; submit its config to start it

# summary() renders the long, timing and files views

    Code
      print(summary(comp, round = "forward_round2", candidate = "WT_V", long = TRUE))
    Message
      
      
      -- SCM Summary: extdata/models/scm-complete/scm/1001 ---------------------------
    Output
      model        : extdata/models/scm-complete/1001.mod
      status       : completed · 5 rounds complete
      direction    : forward -> backward
      alphas       : forward = 0.05, backward = 0.001
      cov step     : off
      candidates   : WT_CL, CRCL_CL, WT_V
      path         : +WT_CL (forward 1) -> +CRCL_CL (forward 2) -> -CRCL_CL (backward 1) => WT_CL
      forward model: forward_final/1001_scm_forward_final.mod (OFV 973.800)  covariates retained: WT_CL, CRCL_CL
      final model  : final/1001_scm_final.mod (OFV 979.500)  covariates retained: WT_CL
      
      forward_round2   added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
                       retained before this round: WT_CL
                       retained after this round: WT_CL, CRCL_CL
        candidate             OFV       dOFV         p
        reference         980.000                          forward_round1/1001_wt_cl.mod
        WT_V              978.500     -1.500     0.221     [2 attempts]
            est 0.050 (N/A) · CI95 N/A · df 1 · tries 2
            forward_round2/1001_wt_v.mod                 no ofv
            forward_round2/1001_wt_v_try2.mod            succeeded

