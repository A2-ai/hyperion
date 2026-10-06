base_mod <- "$PROBLEM edit tests
$INPUT ID TIME EVID AMT CMT DV MDV WT
$DATA data.csv IGNORE=@
$SUBROUTINES ADVAN2 TRANS2
$PK
 TVCL = THETA(1)
 TVV = THETA(2)
 CL = TVCL * EXP(ETA(1))
 V = TVV * EXP(ETA(2))
 KA = THETA(3)
 S2 = V
$ERROR
 IPRED = F
 Y = IPRED + EPS(1)
$THETA
 (0, 1) ;CL
 (0, 30, 100) ;V
 (0, 1) ;KA
$OMEGA
 0.1 ;IIV CL
 0.1 ;IIV V
$SIGMA
 0.04 ;Add
$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999
$COV PRINT=E
$TABLE ID TIME DV IPRED NOPRINT ONEHEADER FILE=run001.tab
"

# A throwaway project with pharos.toml and the given model files.
local_project <- function(files = list(run001.mod = base_mod), .local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  file.copy(system.file("pharos.toml", package = "hyperion"), root)
  for (name in names(files)) {
    writeLines(files[[name]], file.path(root, name), sep = "")
  }
  withr::local_options(hyperion.config_dir = root, .local_envir = .local_envir)
  # copy_model() finds pharos.toml from the working directory.
  withr::local_dir(root, .local_envir = .local_envir)
  root
}

local_model <- function(text = base_mod, .local_envir = parent.frame()) {
  root <- local_project(list(run001.mod = text), .local_envir = .local_envir)
  read_model(file.path(root, "run001.mod"))
}

text_of <- function(model) attr(model, "model_text")

test_that("edits stay in memory until write_model()", {
  mod <- local_model()
  path <- edit_path(mod)
  edited <- update_theta(mod, 1, init = 2)

  expect_equal(attr(edited, "unsaved_edits"), 1L)
  expect_match(text_of(edited), " (0, 2) ;CL", fixed = TRUE)
  expect_false(any(grepl("(0, 2)", readLines(path), fixed = TRUE)))

  written <- write_model(edited)
  expect_equal(attr(written, "unsaved_edits"), 0L)
  expect_equal(text_of(read_model(path)), text_of(edited))
})

test_that("update arguments: left out keeps, NULL removes, NA is refused", {
  mod <- local_model()

  kept <- update_theta(mod, 1, init = 2)
  expect_match(text_of(kept), " (0, 2) ;CL", fixed = TRUE)

  no_lower <- update_theta(mod, 1, lower = NULL)
  expect_match(text_of(no_lower), "\n 1 ;CL\n", fixed = TRUE)

  no_comment <- update_theta(mod, 1, comment = NULL)
  expect_false(grepl(";CL", text_of(no_comment), fixed = TRUE))

  expect_error(update_theta(mod, 1, init = NA), "can't be NA")
  expect_error(update_theta(mod, 1, fix = NULL), "can't be removed")
  expect_error(update_theta(mod, 1, init = "2"), "single number")
})

test_that("placeholders follow THETA renumbering", {
  mod <- local_model()
  edited <- mod |>
    add_theta(0.75, comment = "WT-on-CL", ref = "wt_cl") |>
    add_theta(5, index = 1, comment = "new first") |>
    update_pk(TVCL, append = "* (WT / 70)**{wt_cl.theta}")

  expect_match(text_of(edited), "TVCL = THETA(2) * (WT / 70)**THETA(5)", fixed = TRUE)
  expect_equal(attr(edited, "refs")$index, 5L)
  expect_equal(attr(edited, "unsaved_edits"), 3L)
})

test_that("write_model() refuses declared refs that were never used", {
  mod <- local_model()
  expect_error(
    mod |> add_theta(1, ref = "unused") |> write_model(),
    "never used"
  )
})

test_that("edit attributes carry across every verb", {
  mod <- local_model()
  edited <- mod |>
    add_omega(0.2, comment = "IIV KA", ref = "iiv_ka") |>
    update_est(maxeval = 0) |>
    add_data_filter(ignore = "WT.GT.200") |>
    update_pk(KA, append = "* EXP({iiv_ka.eta})")

  refs <- attr(edited, "refs")
  expect_equal(refs$name, "iiv_ka")
  expect_equal(refs$index, 3L)
  expect_equal(attr(edited, "refs_used"), "iiv_ka")
  expect_equal(attr(edited, "unsaved_edits"), 4L)
  expect_match(text_of(edited), "KA = THETA(3) * EXP(ETA(3))", fixed = TRUE)
})

test_that("copy_model() -> edit -> diff_models() -> write_model() -> reread", {
  root <- local_project()
  new <- copy_model(
    file.path(root, "run001.mod"),
    file.path(root, "run002.mod"),
    description = "WT on CL"
  )
  expect_true(isTRUE(attr(new, "from_copy")))

  edited <- new |>
    add_theta(0.75, comment = "WT-on-CL", ref = "wt") |>
    update_pk(TVCL, append = "* (WT / 70)**{wt.theta}")

  d <- diff_models(edited)
  expect_setequal(names(d$changes), c("$PK", "$THETA"))
  expect_true(" TVCL = THETA(1) * (WT / 70)**THETA(4)" %in% sub("^\\+ ", "", d$changes[["$PK"]]))

  expect_error(
    copy_model(edited, file.path(root, "run003.mod"), description = "x"),
    "unsaved edit"
  )

  written <- write_model(edited)
  expect_length(diff_models(written)$changes, 0)
  reread <- read_model(file.path(root, "run002.mod"))
  expect_equal(text_of(reread), text_of(edited))
  # The copy also renames table files, so more than the edited records differ.
  vs_parent <- diff_models(reread, parent = file.path(root, "run001.mod"))
  expect_true(all(c("$PK", "$THETA") %in% names(vs_parent$changes)))
})

test_that("diff_models() knits as coloured HTML", {
  mod <- local_model()
  d <- mod |> update_theta(1, init = 2) |> diff_models()
  out <- as.character(knitr::knit_print(d))
  expect_match(out, "<pre><strong>$THETA</strong>\n", fixed = TRUE)
  expect_match(out, '<span style="color: #cf222e;">-  (0, 1) ;CL</span>', fixed = TRUE)
  expect_match(out, '<span style="color: #1a7f37;">+  (0, 2) ;CL</span>', fixed = TRUE)
  expect_match(out, "(unchanged: $PROBLEM, ", fixed = TRUE)
  expect_equal(as.character(knitr::knit_print(diff_models(mod))), "No changes.\n")
})

test_that("code verbs replace, place new lines, and set comments", {
  mod <- local_model()

  replaced <- update_error(mod, IPRED, replace = "LOG(F)")
  expect_match(text_of(replaced), "\n IPRED = LOG(F)\n", fixed = TRUE)
  expect_error(update_error(mod, IPRED, replace = "F", append = "+ 1"), "not both")
  expect_error(update_error(mod, replace = "F"), "needs `lhs`")

  placed <- update_pk(mod, append = "IF (WT.GT.100) CL = CL * 1.2")
  lines <- strsplit(text_of(placed), "\n")[[1]]
  at <- which(lines == " CL = TVCL * EXP(ETA(1))")
  expect_equal(lines[at + 1], " IF (WT.GT.100) CL = CL * 1.2")

  commented <- update_error(mod, Y, comment = "Additive")
  expect_match(text_of(commented), "Y = IPRED + EPS(1) ; Additive", fixed = TRUE)
})

test_that("$DATA filters add and remove", {
  mod <- local_model()

  ignored <- add_data_filter(mod, ignore = "WT.GT.100")
  expect_match(text_of(ignored), "$DATA data.csv IGNORE=@ IGNORE=(WT.GT.100)\n", fixed = TRUE)
  removed <- remove_data_filter(ignored, ignore = "wt.gt.100")
  expect_match(text_of(removed), "$DATA data.csv IGNORE=@\n", fixed = TRUE)

  # NONMEM allows ACCEPT next to IGNORE=@, but not next to an IGNORE list.
  accepted <- add_data_filter(mod, accept = "ID.EQ.1")
  expect_match(text_of(accepted), "IGNORE=@ ACCEPT=(ID.EQ.1)\n", fixed = TRUE)
  expect_error(add_data_filter(accepted, ignore = "DV.EQ.0"), "ACCEPT list")

  expect_error(add_data_filter(mod, ignore = "FOO.EQ.1"), "not an \\$INPUT column")
  expect_error(add_data_filter(mod), "exactly one")
  expect_error(remove_data_filter(mod, ignore = "WT.GT.100"), "has no IGNORE")
})

test_that("$EST records add and remove", {
  mod <- local_model()

  two <- add_est(mod, method = "IMP", eonly = 1)
  expect_match(text_of(two), "MAXEVAL=9999\n$EST METHOD=IMP EONLY=1\n", fixed = TRUE)
  expect_error(update_est(two, maxeval = 0), "give `index`")
  expect_match(
    text_of(update_est(two, niter = 5, index = 2)),
    "$EST METHOD=IMP EONLY=1 NITER=5",
    fixed = TRUE
  )

  one <- remove_est(two, 2)
  expect_equal(text_of(one), text_of(mod))
  expect_error(remove_est(mod, 1), "only \\$EST")
  expect_error(add_est(mod, method = "IMP", noabort = FALSE), "value or TRUE")
})

test_that("removing a row takes it out of the code and renumbers", {
  mod <- local_model()

  no_iiv_cl <- remove_omega(mod, 1)
  expect_match(text_of(no_iiv_cl), " CL = TVCL\n V = TVV * EXP(ETA(1))\n", fixed = TRUE)
  expect_match(text_of(no_iiv_cl), "$OMEGA\n 0.1 ;IIV V\n$SIGMA", fixed = TRUE)

  expect_error(remove_theta(mod, 1), "`TVCL`")
  expect_error(remove_omega(mod, 3), "no OMEGA\\(3,3\\)")
  expect_error(remove_sigma(mod, 0), "1 or more")
})

test_that("refs follow rows past a removed one", {
  mod <- local_model()
  edited <- mod |>
    add_theta(0.75, comment = "WT-on-CL", ref = "wt_cl") |>
    add_omega(0.2, comment = "IIV KA", ref = "iiv_ka") |>
    remove_omega(2) |>
    update_pk(KA, append = "* EXP({iiv_ka.eta})")

  expect_match(text_of(edited), "KA = THETA(3) * EXP(ETA(2))", fixed = TRUE)
  expect_equal(attr(edited, "refs")$index, c(4L, 2L))

  dropped <- remove_theta(edited, 4)
  expect_equal(attr(dropped, "refs")$name, "iiv_ka")
})

test_that("indexes must be whole numbers from 1", {
  mod <- local_model()
  expect_error(remove_est(mod, 0), "1 or more")
  expect_error(remove_est(mod, 3e9), "1 or more")
  expect_error(update_est(mod, maxeval = 0, index = 0), "1 or more")
  expect_error(update_theta(mod, 0, init = 1), "1 or more")
  expect_error(update_omega(mod, 1.5, init = 1), "1 or more")
  expect_error(update_table(mod, index = 0, append = "CL"), "1 or more")
  expect_error(add_theta(mod, 1, index = 0), "1 or more")
  expect_error(add_omega_block(mod, index = 0:1, init = c(0.1, 0, 0.1)), "consecutive ETA")
})

test_that("THETA edits keep labels and handle infinite bounds", {
  mod <- local_model(sub(" (0, 1) ;CL", " CL=(0,1) ;CL", base_mod, fixed = TRUE))

  fixed <- update_theta(mod, 1, fix = TRUE)
  expect_match(text_of(fixed), " CL=(0, 1) FIX ;CL\n", fixed = TRUE)

  no_lower <- update_theta(mod, 2, lower = NULL)
  expect_match(text_of(no_lower), " (-INF, 30, 100) ;V\n", fixed = TRUE)
  moved <- update_theta(no_lower, 2, init = 40, comment = "V [L]")
  expect_match(text_of(moved), " (-INF, 40, 100) ;V [L]\n", fixed = TRUE)

  upper_only <- mod |>
    add_theta(1, upper = 5, ref = "u") |>
    update_theta(4, comment = "x") |>
    update_pk(KA, append = "* {u.theta}")
  expect_match(text_of(upper_only), " (-INF, 1, 5) ;x\n", fixed = TRUE)
})

corr_mod <- sub(
  "$OMEGA\n 0.1 ;IIV CL\n 0.1 ;IIV V\n",
  "$OMEGA BLOCK(2) CORRELATION\n 0.1 ;IIV CL\n 0.5\n 0.1 ;IIV V\n",
  base_mod,
  fixed = TRUE
)

test_that("OMEGA block checks use the block's parameterization", {
  mod <- local_model(corr_mod)
  # As covariances 0.2, 0.5, 0.1 aren't positive definite; as a correlation they are.
  edited <- update_omega(mod, 1, init = 0.2)
  expect_match(text_of(edited), " 0.2 ;IIV CL\n 0.5\n", fixed = TRUE)
})

test_that("copy_model(update =) converts estimates to the block's parameterization", {
  sd_mod <- sub("BLOCK(2) CORRELATION", "BLOCK(2) SD CORRELATION", corr_mod, fixed = TRUE)
  root <- local_project(list(corr.mod = corr_mod, sd.mod = sd_mod))
  ext <- c(
    "TABLE NO.     1: First Order: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0",
    " ITERATION    THETA1       THETA2       THETA3       SIGMA(1,1)   OMEGA(1,1)   OMEGA(2,1)   OMEGA(2,2)   OBJ",
    "            0  1.00000E+00  3.00000E+01  1.00000E+00  4.00000E-02  1.00000E-01  5.00000E-02  1.00000E-01    100.0",
    "  -1000000000  1.00000E+00  3.00000E+01  1.00000E+00  4.00000E-02  2.00000E-01  5.00000E-02  1.00000E-01    90.0"
  )
  writeLines(ext, file.path(root, "est.ext"))

  copy_omega <- function(from) {
    copied <- copy_model(
      file.path(root, from),
      file.path(root, paste0("copy-", from)),
      ext_file = file.path(root, "est.ext"),
      update = "omega",
      description = "x",
      no_metadata = TRUE
    )
    lines <- strsplit(text_of(copied), "\n")[[1]]
    at <- grep("^\\$OMEGA", lines)
    as.numeric(sub(";.*$", "", lines[at + 1:3]))
  }

  # Variances 0.2 and 0.1 with covariance 0.05: correlation 0.354, SDs 0.447 and 0.316.
  expect_equal(copy_omega("corr.mod"), c(0.2, 0.354, 0.1))
  expect_equal(copy_omega("sd.mod"), c(0.447, 0.354, 0.316))
})
