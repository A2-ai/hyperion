test_that("hyperion_nonmem_tree knit_print works", {
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
  snapshot_knit_html(tree, "tree-knit")
})
