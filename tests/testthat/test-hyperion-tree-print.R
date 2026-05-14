test_that("hyperion_nonmem_tree print works", {
  tree <- structure(
    list(
      nodes = list(
        list(
          name = "base.mod",
          model = list(
            based_on = list(),
            description = "Base population PK model",
            tags = list()
          ),
          run = NULL
        ),
        list(
          name = "run001.mod",
          model = list(
            based_on = list("base.mod"),
            description = "Run 1",
            tags = list()
          ),
          run = NULL
        ),
        list(
          name = "run002.mod",
          model = list(
            based_on = list("run001.mod"),
            description = "Run 2 with covariate effects",
            tags = list()
          ),
          run = NULL
        )
      )
    ),
    class = "hyperion_nonmem_tree"
  )
  expect_snapshot(print(tree))
})

test_that("hyperion_nonmem_tree print honors verbose attr", {
  tree <- structure(
    list(
      nodes = list(
        list(
          name = "base.mod",
          model = list(
            based_on = list(),
            description = "Base population PK model",
            tags = list("base")
          ),
          run = list(start = list(
            model_hashes = list(blake3 = "f873a13ca1b2c3d4e5f6"),
            dataset_hashes = list(blake3 = "8d8189cfaabb11223344")
          ))
        ),
        list(
          name = "run001.mod",
          model = list(
            based_on = list("base.mod"),
            description = "Adding COV step",
            tags = list()
          ),
          run = NULL
        )
      )
    ),
    class = "hyperion_nonmem_tree"
  )
  attr(tree, "verbose") <- TRUE
  expect_snapshot(print(tree))
})

test_that("hyperion_nonmem_tree verbose print renders 6-column table", {
  tree <- structure(
    list(
      nodes = list(
        list(
          name = "base.mod",
          model = list(
            based_on = list(),
            description = "Base population PK model",
            tags = list("base")
          ),
          run = list(start = list(
            model_hashes = list(blake3 = "f873a13ca1b2c3d4e5f6"),
            dataset_hashes = list(blake3 = "8d8189cfaabb11223344")
          ))
        ),
        list(
          name = "run001.mod",
          model = list(
            based_on = list("base.mod"),
            description = "Adding COV step, unfixing CL",
            tags = list("covariates", "unfixed")
          ),
          run = list(start = list(
            model_hashes = list(blake3 = "1a0f07a1112233445566"),
            dataset_hashes = list(blake3 = "8d8189cfaabb11223344")
          ))
        ),
        list(
          name = "run002.mod",
          model = list(
            based_on = list("run001.mod"),
            description = "Not yet run",
            tags = list()
          ),
          run = NULL
        )
      )
    ),
    class = "hyperion_nonmem_tree"
  )
  expect_snapshot(print(tree, verbose = TRUE))
})
