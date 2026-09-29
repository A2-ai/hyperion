iiv_cov_info <- function() {
  mod_path <- system.file("extdata", "mod", "iiv-cov.mod", package = "hyperion")
  get_model_parameter_info(read_model(mod_path))
}

test_that("mask accepts nonmem names and user names", {
  info <- iiv_cov_info()
  kept <- c("ETA1//ETA-CL/F", "ETA2//ETA-V2/F", "ETA3//ETA-KA")

  expect_equal(get_eta_labels(info), c(kept, "ETA4//ETA-Q/F"))
  expect_equal(get_eta_labels(info, mask = "OMEGA(4,4)"), kept)
  expect_equal(get_eta_labels(info, mask = "IIV (Q/F)"), kept)
})

test_that("mask accepts display names", {
  info <- iiv_cov_info()
  info@omega[["OMEGA(4,4)"]]@display <- "IIV Q"

  expect_equal(
    get_eta_labels(info, mask = "IIV Q"),
    c("ETA1//ETA-CL/F", "ETA2//ETA-V2/F", "ETA3//ETA-KA")
  )
})

test_that("associated thetas and ETA numbers are not mask keys", {
  info <- iiv_cov_info()

  expect_equal(get_eta_labels(info, mask = "Q/F"), get_eta_labels(info))
  expect_equal(get_eta_labels(info, mask = "ETA4"), get_eta_labels(info))
})

test_that("surviving labels keep their original ETA number", {
  info <- iiv_cov_info()
  expect_equal(
    get_eta_labels(info, mask = c("OMEGA(2,2)", "OMEGA(3,3)")),
    c("ETA1//ETA-CL/F", "ETA4//ETA-Q/F")
  )
})

test_that("get_fixed_parameters output can be passed straight through", {
  mod_path <- system.file("extdata", "mod", "iiv-cov.mod", package = "hyperion")
  mod <- read_model(mod_path)
  info <- get_model_parameter_info(mod)

  expect_equal(
    get_eta_labels(info, mask = get_fixed_parameters(mod, kind = "OMEGA")),
    c("ETA1//ETA-CL/F", "ETA2//ETA-V2/F", "ETA3//ETA-KA")
  )
})

test_that("off-diagonal keys drop nothing and are not reported", {
  info <- iiv_cov_info()
  expect_silent(labels <- get_eta_labels(info, mask = "OMEGA(2,1)"))
  expect_equal(labels, get_eta_labels(info))
})

test_that("mask keys naming no omega are ignored", {
  info <- iiv_cov_info()
  expect_equal(
    get_eta_labels(info, mask = c("OMEGA(9,9)", "OMEGA(4,4)")),
    c("ETA1//ETA-CL/F", "ETA2//ETA-V2/F", "ETA3//ETA-KA")
  )
})

test_that("a key shared by two omegas masks both", {
  info <- ModelComments(
    theta = list(
      THETA1 = ThetaComment(nonmem_name = "THETA1", name = "CL"),
      THETA2 = ThetaComment(nonmem_name = "THETA2", name = "V")
    ),
    omega = list(
      `OMEGA(1,1)` = OmegaComment(
        nonmem_name = "OMEGA(1,1)",
        name = "IIV",
        associated_theta = "CL"
      ),
      `OMEGA(2,2)` = OmegaComment(
        nonmem_name = "OMEGA(2,2)",
        name = "IIV",
        associated_theta = "V"
      )
    )
  )
  expect_equal(get_eta_labels(info, mask = "IIV"), character(0))
})

test_that("mask must be a character vector", {
  expect_error(get_eta_labels(iiv_cov_info(), mask = 4), "mask must be a character vector")
})
