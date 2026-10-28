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
      Covariance step: rounds off, forward model on, final model on
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
      Covariance step: rounds off, forward model on, final model on
      Rounds: all
      ! No effects yet: pass `effects` to `init_scm()` or add them with `add_scm_effect()`.

# print methods for the SCM records show pharos' text under a title

    Code
      print(s)
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
      
      reference        base/1001_base.mod succeeded (OFV: 1000.000)
      
      forward_round1   added WT_CL (p = 7.744e-6, dOFV = -20.000)
        candidate             OFV       dOFV         p
        reference        1000.000                          base/1001_base.mod
        WT_CL             980.000    -20.000    7.7e-6 *   <- added
        CRCL_CL           996.000     -4.000     0.046 *
        WT_V              999.000     -1.000     0.317
      
      forward_round2   added CRCL_CL (p = 1.431e-2, dOFV = -6.000) [1 retry]
        candidate             OFV       dOFV         p
        reference         980.000                          forward_round1/1001_wt_cl.mod
        CRCL_CL           974.000     -6.000     0.014 *   <- added
        WT_V              978.500     -1.500     0.221     [2 attempts]
      
      forward_round3   no candidate significant at alpha = 0.05; forward selection stopped
        candidate             OFV       dOFV         p
        reference         974.000                          forward_round2/1001_crcl_cl.mod
        WT_V              973.000     -1.000     0.317
      
      backward_round1  dropped CRCL_CL (p = 1.431e-2, dOFV = +6.000)
        candidate             OFV       dOFV         p
        reference         974.000                          forward_round2/1001_crcl_cl.mod
        CRCL_CL           980.000     +6.000     0.014     <- dropped
        WT_CL             995.000    +21.000    4.6e-6 *   kept
      
      backward_round2  every covariate significant at alpha = 0.001; backward elimination stopped
        candidate             OFV       dOFV         p
        reference         980.000                          backward_round1/1001_crcl_cl.mod
        WT_CL            1000.000    +20.000    7.7e-6 *   kept
    Message
      ! 2 candidate fits were unusable; see the round tables above.

---

    Code
      print(get_scm_status(comp))
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

---

    Code
      print(plan)
    Message
      
      
      -- SCM Plan: extdata/models/scm-complete/scm/1001 ------------------------------
    Output
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
    Message
      ! cov_step is off: the initial model's $COVARIANCE record will be removed from the round models
      x The SCM process in 'extdata/models/scm-complete/scm/1001' cannot resume under this plan; submit with `overwrite = TRUE` to discard it.

