# init_scm() writes the starter config and returns the object

    Code
      writeLines(readLines(p$config))
    Output
      # SCM setup for 1001.mod.
      #
      # This file sits beside the SCM process's own directory, scm/1001/,
      # where everything the process writes lands.
      #
      # Fill in `effects` under [covariates] below, then submit the SCM process
      # (`scm run` or `scm slurm submit` on this file); `scm plan` on this file
      # shows what a submit would do without doing it.
      
      model = "model/1001.mod"    # relative to the project's pharos.toml
      
      # Which phases to run: "forward", "backward", or both. Forward always
      # runs before backward.
      direction = ["forward", "backward"]
      
      # The optional settings, at their defaults: change any of these as you
      # need to.
      forward_alpha = 0.05     # p-value to add a covariate in forward selection
      backward_alpha = 0.001   # p-value to keep a covariate in backward elimination
      max_retries = 3          # retries per failed fit, each from the previous attempt's estimates, jittered
      retry_jitter = 0.05      # how far a retry perturbs the estimates it starts from, as a fraction (0.05 = 5%)
      num_rounds = 0           # num_rounds = 0 means run all rounds. Use an integer for a specific number of rounds
      cov_step = false         # whether generated models run $COVARIANCE
      forward_final_cov_step = true # whether the forward model is re-fitted with $COVARIANCE on
      final_cov_step = true    # whether the final model is re-fitted with $COVARIANCE on at the end
      
      # Every covariate effect to be tested — insert all of them here, named the
      # way the initial model's $THETA records name it.
      #
      # The covariates can be entered just by their name, as in Example 1. Such an
      # effect is continuous, and is first tested at the initial estimate its type
      # gets below, unless the initial model already gives it one. Otherwise, the
      # covariates can also be entered like in Example 2. This allows you to declare
      # a categorical effect, and to control an effect's initial estimate, fixed
      # value, lower, or upper bounds as needed.
      #
      #   effects = [
      #     # Example 1
      #     "WT_CL", "CRCL_CL",
      #     # Example 2
      #     { name = "SEXEFF_CL", type = "categorical", fixed = 1, lower = 0, upper = 5 },
      #   ]
      #
      [covariates]
      fixed = 0                # default value a held-out effect's theta is fixed at
      continuous  = { initial = 0.1 } #Default initial estimate for a continuous-type covariate
      categorical = { initial = 1 } #Default initial estimate for a categorical-type covariate
      
      effects = []

---

    Code
      writeLines(readLines(file.path(p$out_dir, ".gitignore")))
    Output
      # Written by pharos (nonmem.scm track_in_git = "milestones"). Do not edit; change pharos.toml instead.
      /*
      !/.gitignore
      !/scm_summary.json
      !/scm_summary.md
      !/scm_driver.log
      !/base/
      !/full/
      !/forward_final/
      !/final/

# init_scm() prints where it wrote

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

# check_scm() returns the plan with its warnings

    Code
      print(plan)
    Message
      
      
      -- SCM Plan: extdata/models/scm/scm/1001 ---------------------------------------
    Output
      model      : extdata/models/scm/1001.mod
      direction  : forward -> backward
      forward    : alpha = 0.05
      backward   : alpha = 0.001
      on failure : retry up to 3x from the previous attempt's estimates, jittered 5%
      cov step   : off
      forward fit: re-fit the forward model with the cov step on
      final fit  : re-fit the final model with the cov step on
      candidates :
        name         theta     type          initial  FIXED
        WT_CL        THETA(4)  continuous        0.1      0
        CRCL_CL      THETA(5)  continuous        0.1      0
        WT_V         THETA(6)  continuous        0.1      0
                   (initial: the effect's initial estimate the first time it is tested; FIXED: what it is fixed at when held out)
      max models : 13 (incl. reference fit, excl. retries)
    Message
      ! cov_step is off: the initial model's $COVARIANCE record will be removed from the round models

---

    Code
      print(check_scm(scm_fixture("scm", "1001scm.toml"), num_rounds = 1))
    Message
      
      
      -- SCM Plan: extdata/models/scm/scm/1001 ---------------------------------------
    Output
      model      : extdata/models/scm/1001.mod
      direction  : forward -> backward
      forward    : alpha = 0.05
      backward   : alpha = 0.001
      on failure : retry up to 3x from the previous attempt's estimates, jittered 5%
      cov step   : off
      forward fit: re-fit the forward model with the cov step on
      final fit  : re-fit the final model with the cov step on
      num rounds : pause after 1 (resumable)
      candidates :
        name         theta     type          initial  FIXED
        WT_CL        THETA(4)  continuous        0.1      0
        CRCL_CL      THETA(5)  continuous        0.1      0
        WT_V         THETA(6)  continuous        0.1      0
                   (initial: the effect's initial estimate the first time it is tested; FIXED: what it is fixed at when held out)
      max models : 13 (incl. reference fit, excl. retries)
    Message
      ! cov_step is off: the initial model's $COVARIANCE record will be removed from the round models

# check_scm() reports a started process

    Code
      cat(scrub_scm(attr(plan, "text")))
    Output
      <scm plan> extdata/models/scm-complete/scm/1001
      model      : extdata/models/scm-complete/1001.mod
      direction  : forward -> backward
      forward    : alpha = 0.05
      backward   : alpha = 0.001
      on failure : retry up to 3x from the previous attempt's estimates, jittered 5%
      cov step   : off
      forward fit: re-fit the forward model with the cov step on
      final fit  : re-fit the final model with the cov step on
      candidates :
        name         theta     type          initial  FIXED
        WT_CL        THETA(4)  continuous        0.1      0
        CRCL_CL      THETA(5)  continuous        0.1      0
        WT_V         THETA(6)  continuous        0.1      0
                   (initial: the effect's initial estimate the first time it is tested; FIXED: what it is fixed at when held out)
      max models : 13 (incl. reference fit, excl. retries)
      
      progress     : completed — 5 rounds complete (updated [TIMESTAMP])
      selected     : WT_CL
      final model  : final/1001_scm_final.mod
      changes      : none — identical to the config the SCM process was submitted under

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

# summary() reads a planned process

    Code
      print(s)
    Message
      
      
      -- SCM Summary: extdata/models/scm/scm/1001 ------------------------------------
    Output
      model        : extdata/models/scm/1001.mod
      status       : planned · no rounds complete
      direction    : forward -> backward
      alphas       : forward = 0.05, backward = 0.001
      cov step     : off
      candidates   : WT_CL, CRCL_CL, WT_V
      note         : the SCM process has not started; submit its config to start it
      path         : none

# summary() and get_scm_status() read a completed process

    Code
      print(status)
    Message
      
      
      -- SCM Status: extdata/models/scm-complete/scm/1001 ----------------------------
    Output
      model        : extdata/models/scm-complete/1001.mod
      status       : completed · 5 rounds complete
      direction    : forward -> backward
      alphas       : forward = 0.05, backward = 0.001
      cov step     : off
      candidates   : WT_CL, CRCL_CL, WT_V
      forward model: forward_final/1001_scm_forward_final.mod (OFV 973.800)  covariates retained: WT_CL, CRCL_CL
      final model  : final/1001_scm_final.mod (OFV 979.500)  covariates retained: WT_CL
      
      reference complete: base model fitted (OFV 1000.000)
      
      forward_round1 complete: added WT_CL (p = 7.744e-6, dOFV = -20.000); retained: WT_CL
        candidate             OFV       dOFV         p
        WT_CL             980.000    -20.000    7.7e-6 *   <- added
        CRCL_CL           996.000     -4.000     0.046 *
        WT_V              999.000     -1.000     0.317
      
      forward_round2 complete: added CRCL_CL (p = 1.431e-2, dOFV = -6.000); retained: WT_CL, CRCL_CL
        candidate             OFV       dOFV         p
        CRCL_CL           974.000     -6.000     0.014 *   <- added
        WT_V              978.500     -1.500     0.221     [2 attempts]
      
      forward_round3 complete: no candidate significant at alpha = 0.05; forward selection stopped; retained: WT_CL, CRCL_CL
        candidate             OFV       dOFV         p
        WT_V              973.000     -1.000     0.317
      
      backward_round1 complete: dropped CRCL_CL (p = 1.431e-2, dOFV = +6.000); retained: WT_CL
        candidate             OFV       dOFV         p
        CRCL_CL           980.000     +6.000     0.014     <- dropped
        WT_CL             995.000    +21.000    4.6e-6 *   kept
      
      backward_round2 complete: every covariate significant at alpha = 0.001; backward elimination stopped; retained: WT_CL
        candidate             OFV       dOFV         p
        WT_CL            1000.000    +20.000    7.7e-6 *   kept

# summary() renders the long, timing and files views

    Code
      print(summary(comp, long = TRUE, timing = TRUE, files = TRUE))
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
      time         : 11 fits (1 retry) · wall 1m 30s
      
      reference        base/1001_base.mod succeeded (OFV: 1000.000)
                       1 fit · wall 1m 30s
            run dir  base/1001_base
            lst      base/1001_base/1001_base.lst
            ext      base/1001_base/1001_base.ext
            summary  base/1001_base/pharos_summary.json
      
      forward_round1   added WT_CL (p = 7.744e-6, dOFV = -20.000)
                       retained before this round: none
                       retained after this round: WT_CL
                       3 fits · wall 1m 30s
        candidate             OFV       dOFV         p
        reference        1000.000                                        base/1001_base.mod
        WT_CL             980.000    -20.000    7.7e-6 *    est 1.0s     <- added
            est 0.250 (N/A) · CI95 N/A · df 1 · tries 1
            forward_round1/1001_wt_cl.mod                   est 1.0s     succeeded
            run dir  forward_round1/1001_wt_cl
            lst      forward_round1/1001_wt_cl/1001_wt_cl.lst
            ext      forward_round1/1001_wt_cl/1001_wt_cl.ext
            summary  forward_round1/1001_wt_cl/pharos_summary.json
        CRCL_CL           996.000     -4.000     0.046 *    est 1.0s
            est 0.150 (N/A) · CI95 N/A · df 1 · tries 1
            forward_round1/1001_crcl_cl.mod                 est 1.0s     succeeded
            run dir  forward_round1/1001_crcl_cl
            lst      forward_round1/1001_crcl_cl/1001_crcl_cl.lst
            ext      forward_round1/1001_crcl_cl/1001_crcl_cl.ext
            summary  forward_round1/1001_crcl_cl/pharos_summary.json
        WT_V              999.000     -1.000     0.317      est 1.0s
            est 0.050 (N/A) · CI95 N/A · df 1 · tries 1
            forward_round1/1001_wt_v.mod                    est 1.0s     succeeded
            run dir  forward_round1/1001_wt_v
            lst      forward_round1/1001_wt_v/1001_wt_v.lst
            ext      forward_round1/1001_wt_v/1001_wt_v.ext
            summary  forward_round1/1001_wt_v/pharos_summary.json
      
      forward_round2   added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
                       retained before this round: WT_CL
                       retained after this round: WT_CL, CRCL_CL
                       3 fits · wall 1m 30s
        candidate             OFV       dOFV         p
        reference         980.000                                        forward_round1/1001_wt_cl.mod
        CRCL_CL           974.000     -6.000     0.014 *    est 1.0s     <- added
            est 0.150 (N/A) · CI95 N/A · df 1 · tries 1
            forward_round2/1001_crcl_cl.mod                 est 1.0s     succeeded
            run dir  forward_round2/1001_crcl_cl
            lst      forward_round2/1001_crcl_cl/1001_crcl_cl.lst
            ext      forward_round2/1001_crcl_cl/1001_crcl_cl.ext
            summary  forward_round2/1001_crcl_cl/pharos_summary.json
        WT_V              978.500     -1.500     0.221      est 2.0s     [2 attempts]
            est 0.050 (N/A) · CI95 N/A · df 1 · tries 2
            forward_round2/1001_wt_v.mod                    est 1.0s     no ofv
            forward_round2/1001_wt_v_try2.mod               est 1.0s     succeeded
            run dir  forward_round2/1001_wt_v_try2
            lst      forward_round2/1001_wt_v_try2/1001_wt_v_try2.lst
            ext      forward_round2/1001_wt_v_try2/1001_wt_v_try2.ext
            summary  forward_round2/1001_wt_v_try2/pharos_summary.json
      
      forward_round3   no candidate significant at alpha = 0.05; forward selection stopped
                       retained before this round: WT_CL, CRCL_CL
                       retained after this round: WT_CL, CRCL_CL
                       1 fit · wall 1m 30s
        candidate             OFV       dOFV         p
        reference         974.000                                        forward_round2/1001_crcl_cl.mod
        WT_V              973.000     -1.000     0.317      est 1.0s
            est 0.050 (N/A) · CI95 N/A · df 1 · tries 1
            forward_round3/1001_wt_v.mod                    est 1.0s     succeeded
            run dir  forward_round3/1001_wt_v
            lst      forward_round3/1001_wt_v/1001_wt_v.lst
            ext      forward_round3/1001_wt_v/1001_wt_v.ext
            summary  forward_round3/1001_wt_v/pharos_summary.json
      
      backward_round1  dropped CRCL_CL (p = 1.431e-2, dOFV = +6.000)
                       retained before this round: WT_CL, CRCL_CL
                       dropped after this round: CRCL_CL
                       2 fits · wall 1m 30s
        candidate             OFV       dOFV         p
        reference         974.000                                        forward_round2/1001_crcl_cl.mod
        CRCL_CL           980.000     +6.000     0.014      est 1.0s     <- dropped
            est 0.150 (N/A) · CI95 N/A · df 1 · tries 1
            backward_round1/1001_crcl_cl.mod                est 1.0s     succeeded
            run dir  backward_round1/1001_crcl_cl
            lst      backward_round1/1001_crcl_cl/1001_crcl_cl.lst
            ext      backward_round1/1001_crcl_cl/1001_crcl_cl.ext
            summary  backward_round1/1001_crcl_cl/pharos_summary.json
        WT_CL             995.000    +21.000    4.6e-6 *    est 1.0s     kept
            est 0.250 (N/A) · CI95 N/A · df 1 · tries 1
            backward_round1/1001_wt_cl.mod                  est 1.0s     succeeded
            run dir  backward_round1/1001_wt_cl
            lst      backward_round1/1001_wt_cl/1001_wt_cl.lst
            ext      backward_round1/1001_wt_cl/1001_wt_cl.ext
            summary  backward_round1/1001_wt_cl/pharos_summary.json
      
      backward_round2  every covariate significant at alpha = 0.001; backward elimination stopped
                       retained before this round: WT_CL
                       dropped after this round: none
                       1 fit · wall 1m 30s
        candidate             OFV       dOFV         p
        reference         980.000                                        backward_round1/1001_crcl_cl.mod
        WT_CL            1000.000    +20.000    7.7e-6 *    est 1.0s     kept
            est 0.250 (N/A) · CI95 N/A · df 1 · tries 1
            backward_round2/1001_wt_cl.mod                  est 1.0s     succeeded
            run dir  backward_round2/1001_wt_cl
            lst      backward_round2/1001_wt_cl/1001_wt_cl.lst
            ext      backward_round2/1001_wt_cl/1001_wt_cl.ext
            summary  backward_round2/1001_wt_cl/pharos_summary.json

---

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

