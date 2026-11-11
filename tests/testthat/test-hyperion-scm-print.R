test_that("print.hyperion_nonmem_scm shows the config", {
  scm <- read_scm(scm_fixture("scm", "1001scm.toml"))
  scm$covariates$effects[[3]] <- list(name = "WT_V", type = "categorical", initial = 1, lower = 0, upper = 5)
  expect_snapshot(print(scm))
  scm$covariates$effects <- list()
  expect_snapshot(print(scm))
})

test_that("ScmEffect prints on one line", {
  expect_snapshot({
    print(ScmEffect("WT_CL"))
    print(ScmEffect("WT_V", type = "categorical", initial = 1, lower = 0, upper = 5))
  })
})

test_that("print methods for the SCM records lay them out as facts and tables", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  s <- summary(comp)
  s$totals$unusable <- 2
  expect_snapshot(print(s))
  expect_snapshot(print(get_scm_status(comp)))

  plan <- check_scm(comp)
  attr(plan, "resumable") <- FALSE
  expect_snapshot(print(plan), transform = scrub_scm)
  expect_false(any(grepl('"text"', capture.output(str(s, max.level = 1)), fixed = TRUE)))
})

test_that("a summary prints the view it was asked for", {
  comp <- read_scm(scm_complete_fixture("scm", "1001scm.toml"))
  s <- summary(comp, round = "2", candidate = "WT_V", timing = TRUE, files = TRUE)
  expect_equal(
    attr(s, "view"),
    list(round = "2", candidate = "WT_V", long = FALSE, timing = TRUE, files = TRUE)
  )
  parts <- build_scm_record_parts(s)
  titles <- vapply(parts$sections, function(section) section$title, character(1))
  expect_equal(titles, c("Models", "Forward round 2"))
  round <- parts$sections[[2]]
  expect_equal(round$tables[[1]]$data$Candidate, "WT_V")
  expect_equal(
    vapply(round$tables, function(table) table$title %||% "", character(1)),
    c("", "Attempts", "Files")
  )
  expect_snapshot(print(s))

  # Every round a candidate was in, and none it was not.
  parts <- build_scm_record_parts(summary(comp, candidate = "CRCL_CL"))
  expect_equal(
    vapply(parts$sections, function(section) section$title, character(1)),
    c("Models", "Forward round 1", "Forward round 2", "Backward round 1")
  )
})

# A running process, made from the completed one: backward round 1 open, a
# fit unusable, the driver gone with its allocation still held.
running_scm_status <- function() {
  status <- get_scm_status(read_scm(scm_complete_fixture("scm", "1001scm.toml")))
  status$status <- "running"
  status$rounds <- status$rounds[1:5]
  status$rounds[[3]]$candidates[[2]]$status <- "unusable"
  status$rounds[[3]]$candidates[[2]][c("ofv", "delta_ofv", "p_value", "significant")] <- NULL
  status$rounds[[5]]$complete <- FALSE
  status$reduced_covariate_model <- NULL
  lines <- strsplit(attr(status, "text"), "\n", fixed = TRUE)[[1]]
  open <- grep("^backward_round1", lines)
  attr(status, "text") <- paste(
    c(
      sub("completed", "running", lines[seq_len(open - 1)]),
      "backward_round1: 1/2 done \u00b7 1 running \u00b7 running 4m 10s",
      "  \u2713 CRCL_CL  1m 02s",
      "  \u25cf WT_CL    job 4521  running 3m 08s  iteration 42, OFV 995.120",
      "  every fit has ended; submit the config again to decide the round",
      "driver       : NOT RUNNING (slurm job 4500 on node01, slurm) \u2014 the SCM process stopped before finishing",
      "               submit its config again to resume",
      "allocation   : slurm allocation 4499 is still held; release it with `scancel 4499`"
    ),
    collapse = "\n"
  )
  status
}

test_that("a status prints its open round fit by fit and closes with the driver", {
  status <- running_scm_status()
  parts <- build_scm_record_parts(status)

  expect_equal(parts$facts$tone[parts$facts$label == "Status"], "blue")
  titles <- vapply(parts$sections, function(section) section$title, character(1))
  expect_equal(
    titles,
    c("Models", "Forward round 1", "Forward round 2", "Forward round 3", "Backward round 1 (open)")
  )
  # The unusable fit is marked in its round's table, with its attempts under it.
  round2 <- parts$sections[[3]]
  table <- round2$tables[[1]]$data
  expect_equal(table$Status, c("", "unusable"))
  expect_equal(attr(table, "cell_colors")[2, match("Status", names(table))], "red")
  expect_equal(attr(table, "cell_colors")[1, match("Result", names(table))], "green")
  expect_equal(round2$tables[[2]]$title, "Attempts")
  # The open round: pharos' live lines, as a table.
  open <- parts$sections[[5]]
  expect_equal(open$alerts[[1]]$text, "1/2 done \u00b7 1 running \u00b7 running 4m 10s")
  expect_equal(open$alerts[[2]]$text, "every fit has ended; submit the config again to decide the round")
  expect_equal(open$tables[[1]]$data$Candidate, c("CRCL_CL", "WT_CL"))
  expect_equal(open$tables[[1]]$data$Progress[2], "job 4521  running 3m 08s  iteration 42, OFV 995.120")
  # The driver's lines are alerts, the line continuing one joined to it.
  expect_equal(vapply(parts$alerts, function(a) a$type, character(1)), c("danger", "warning"))
  expect_match(parts$alerts[[1]]$text, "^Driver: NOT RUNNING .* submit its config again to resume$")
  expect_match(parts$alerts[[2]]$text, "^Allocation: slurm allocation 4499")

  expect_snapshot(print(status))
})

test_that("the pieces of pharos' text are read as facts", {
  facts <- scm_parse_facts(c(
    "model        : a/b.mod",
    "cov step     : off",
    "candidates :",
    "  WT_CL  THETA(4)",
    "max models : 13 (incl. reference fit: 1)",
    "overwrite = TRUE: submit discards the process"
  ))
  expect_equal(facts$label, c("model", "cov step", "candidates", "max models", ""))
  expect_equal(
    facts$value,
    c("a/b.mod", "off", "WT_CL  THETA(4)", "13 (incl. reference fit: 1)", "overwrite = TRUE: submit discards the process")
  )
  expect_equal(scm_fact_label(c("cov step", "num rounds", "something new")), c("Covariance step", "Rounds", "Something new"))
  expect_equal(scm_text_blocks(c("a", "b", "", "", "c")), list(c("a", "b"), "c"))
  expect_equal(scm_text_blocks(character()), list())

  details <- scm_round_details(
    c(
      "forward_round2   added CRCL_CL",
      "  candidate             OFV",
      "  CRCL_CL           974.000",
      "      est 0.150 (0.020) \u00b7 CI95 0.110, 0.190 \u00b7 df 1 \u00b7 tries 1 \u00b7 cond# 12",
      "  WT_V              978.500",
      "      df 1 \u00b7 tries 2"
    ),
    c("CRCL_CL", "WT_V")
  )
  expect_equal(details$CRCL_CL$est, "0.150 (0.020)")
  expect_equal(details$CRCL_CL$CI95, "0.110, 0.190")
  expect_equal(details$CRCL_CL[["cond#"]], "12")
  expect_null(details$WT_V$est)

  expect_equal(scm_fmt_p(0.04550026), "0.046")
  expect_equal(scm_fmt_p(7.744e-6), "7.74e-06")
  expect_equal(scm_fmt_delta(6), "+6.000")
  expect_equal(scm_fmt_duration(90), "1m 30s")
  expect_equal(scm_fmt_duration(3725), "1h 02m")
  expect_equal(scm_round_title("backward_round12"), "Backward round 12")
})
