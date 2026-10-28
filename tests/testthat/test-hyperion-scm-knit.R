test_that("knit_print methods render the SCM objects as HTML", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  snapshot_knit_html(scm, "scm-knit-config")
  snapshot_knit_html(check_scm(scm), "scm-knit-plan")
  snapshot_knit_html(get_scm_status(scm), "scm-knit-status-planned")
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  snapshot_knit_html(get_scm_status(comp), "scm-knit-status-complete")
  snapshot_knit_html(summary(comp, round = "forward_round1"), "scm-knit-summary-round")
})
