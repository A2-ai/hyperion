test_that("ModelComments validates duplicate theta names", {
  theta1 <- ThetaComment(nonmem_name = "THETA1", name = "CL")
  theta2 <- ThetaComment(nonmem_name = "THETA2", name = "CL")

  expect_error(
    ModelComments(theta = list(THETA1 = theta1, THETA2 = theta2)),
    "Duplicate names in theta"
  )
})

test_that("ModelComments validates omega associated_theta existence", {
  theta1 <- ThetaComment(nonmem_name = "THETA1", name = "CL")
  omega11 <- OmegaComment(
    nonmem_name = "OMEGA(1,1)",
    name = "IIV",
    associated_theta = "V"
  )

  expect_error(
    ModelComments(
      theta = list(THETA1 = theta1),
      omega = list(`OMEGA(1,1)` = omega11)
    ),
    "associated_theta"
  )
})

test_that("ModelComments enforces comment class types", {
  theta1 <- ThetaComment(nonmem_name = "THETA1", name = "CL")

  expect_error(
    ModelComments(theta = list(THETA1 = theta1, THETA2 = "bad")),
    "must be a ThetaComment object"
  )
})


test_that("duplicate omega errors identify all conflicting parameters", {
  omega <- list(
    OmegaComment(nonmem_name = "OMEGA(1,1)"),
    OmegaComment(
      nonmem_name = "OMEGA(2,2)", name = "IIV (FPBO)",
      associated_theta = "FPBO"
    ),
    OmegaComment(nonmem_name = "OMEGA(3,3)", name = "IOV"),
    OmegaComment(
      nonmem_name = "OMEGA(4,4)", name = "IIV (FPBO)",
      associated_theta = "FPBO"
    ),
    OmegaComment(nonmem_name = "OMEGA(5,5)", name = "IOV"),
    OmegaComment(
      nonmem_name = "OMEGA(6,6)", name = "IIV (FPBO)",
      associated_theta = "FPBO"
    )
  )
  names(omega) <- vapply(omega, function(cmt) cmt@nonmem_name, character(1))

  expect_error(
    ModelComments(
      theta = list(THETA1 = ThetaComment(nonmem_name = "THETA1", name = "FPBO")),
      omega = omega
    ),
    paste0(
      "Duplicate name + associated_theta in omega: ",
      "IIV (FPBO)|FPBO [OMEGA(2,2), OMEGA(4,4), OMEGA(6,6)]; ",
      "IOV| [OMEGA(3,3), OMEGA(5,5)]"
    ),
    fixed = TRUE
  )
})
