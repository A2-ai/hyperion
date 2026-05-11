#' @noRd
build_tree_display_parts <- function(x) {
  if (is.null(x$nodes) || length(x$nodes) == 0) {
    return(list(
      is_empty = TRUE,
      title = "Hyperion Model Tree"
    ))
  }

  # Index the Vec<LineageNode> by name for O(1) lookup downstream.
  nodes_by_name <- x$nodes
  names(nodes_by_name) <- vapply(x$nodes, function(n) n$name, character(1))

  tree_data <- build_cli_tree_data(nodes_by_name)
  total_models <- length(tree_data$parent)
  all_parents <- tree_data$parent
  all_children <- unlist(tree_data$children)
  root_nodes <- setdiff(all_parents, all_children)

  # Models the caller named explicitly (positional, from, to) get
  # highlighted in the print. The `focal` attribute is set by the rust
  # `get_model_lineage` wrapper.
  focal <- attr(x, "focal") %||% character()
  focal_display <- gsub("\\.(mod|ctl)$", "", focal)

  list(
    is_empty = FALSE,
    title = "Hyperion Model Tree",
    tree_data = tree_data,
    total_models = total_models,
    root_nodes = root_nodes,
    nodes = nodes_by_name,
    focal_display = focal_display
  )
}

#' Print Method for Hyperion Tree Objects
#'
#' Displays a hyperion_nonmem_tree in a readable tree format using cli::tree().
#' Shows the hierarchical relationships between models with Unicode tree characters.
#'
#' @param x A hyperion_nonmem_tree object
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#' @rawNamespace S3method(base::print, hyperion_nonmem_tree)
print.hyperion_nonmem_tree <- function(x, ...) {
  cli::cli_text("")
  parts <- build_tree_display_parts(x)

  if (parts$is_empty) {
    cli::cli_h1(parts$title)
    cli::cli_alert_warning("Empty tree - no models found")
    return(invisible(x))
  }

  cli::cli_h1(parts$title)
  cli::cli_alert_info("Models: {parts$total_models}")
  cli::cli_text("")

  final_output <- character()

  for (root_idx in seq_along(parts$root_nodes)) {
    root_node <- parts$root_nodes[root_idx]
    tree_output <- cli::tree(parts$tree_data, root = root_node)

    for (i in seq_along(tree_output)) {
      line <- tree_output[i]
      node_name <- gsub("^[^a-zA-Z0-9._]*", "", line)
      mod_key <- paste0(node_name, ".mod")
      ctl_key <- paste0(node_name, ".ctl")
      node_key <- if (mod_key %in% names(parts$nodes)) mod_key else ctl_key

      is_root <- (node_name %in% parts$root_nodes)
      children <- parts$tree_data$children[
        parts$tree_data$parent == node_name
      ][[1]]
      is_leaf <- length(children) == 0

      tree_prefix <- gsub(node_name, "", line, fixed = TRUE)
      is_focal <- node_name %in% parts$focal_display
      display_name <- if (is_focal) {
        cli::style_bold(cli::style_underline(node_name))
      } else {
        node_name
      }
      colored_node <- if (is_root) {
        cli::col_blue(cli::style_bold(display_name))
      } else if (is_leaf) {
        cli::col_green(display_name)
      } else {
        cli::col_yellow(display_name)
      }

      node_model <- parts$nodes[[node_key]]$model
      has_tags <- !is.null(node_model) && length(node_model$tags) > 0
      has_desc <- !is.null(node_model) &&
        !is.null(node_model$description) &&
        nzchar(node_model$description)
      suffix <- ""
      if (has_tags) {
        suffix <- paste0(
          " ",
          cli::col_cyan(paste(node_model$tags, collapse = ", "))
        )
      }
      if (has_desc) {
        desc_text <- node_model$description
        if (nchar(desc_text) > 50) {
          desc_text <- paste0(substr(desc_text, 1, 47), "...")
        }
        sep <- if (has_tags) cli::style_dim(" | ") else " "
        suffix <- paste0(suffix, sep, cli::style_dim(desc_text))
      }
      final_output <- c(final_output, paste0(tree_prefix, colored_node, suffix))
    }

    if (root_idx < length(parts$root_nodes)) {
      final_output <- c(final_output, "")
    }
  }

  cat(final_output, sep = "\n")
  invisible(x)
}

#' Build Tree Data for cli::tree()
#'
#' Internal helper function to convert hyperion_nonmem_tree nodes into the exact
#' data frame format expected by cli::tree().
#'
#' @param hyperion_nonmem_tree A hyperion_nonmem_tree object
#' @return A data frame suitable for cli::tree()
#' @keywords internal
#' @noRd
build_cli_tree_data <- function(nodes_by_name) {
  all_nodes <- names(nodes_by_name)

  # Build children map. Only treat a based_on reference as a parent edge if
  # the parent is actually in the tree — pharos slices intentionally exclude
  # ancestors outside the slice (e.g. `from = run002` excludes run001), so
  # synthesizing those parents would put a phantom "root" above the slice.
  children_map <- list()
  for (node_name in all_nodes) {
    node_info <- nodes_by_name[[node_name]]
    if (length(node_info$model$based_on) > 0) {
      parent <- node_info$model$based_on[[1]]
      if (!(parent %in% all_nodes)) {
        next
      }
      if (is.null(children_map[[parent]])) {
        children_map[[parent]] <- character(0)
      }
      children_map[[parent]] <- c(children_map[[parent]], node_name)
    }
  }

  unique_nodes <- all_nodes

  # Ensure all nodes have entries in children_map
  for (node in unique_nodes) {
    if (is.null(children_map[[node]])) {
      children_map[[node]] <- character(0)
    }
  }

  # Create result data frame
  data.frame(
    stringsAsFactors = FALSE,
    parent = gsub("\\.(mod|ctl)$", "", unique_nodes),
    children = I(lapply(unique_nodes, function(node) {
      gsub("\\.(mod|ctl)$", "", children_map[[node]])
    }))
  )
}

#' Knit print method for hyperion_nonmem_tree objects (for Quarto/R Markdown)
#' @param x A hyperion_nonmem_tree object
#' @param ... Additional arguments (ignored)
#' @return HTML/markdown output for rendered documents
#' @exportS3Method knitr::knit_print
knit_print.hyperion_nonmem_tree <- function(x, ...) {
  parts <- build_tree_display_parts(x)
  output <- character()

  if (parts$is_empty) {
    output <- c(
      output,
      "",
      paste0("<strong>", parts$title, "</strong>"),
      ""
    )
    output <- c(output, "\u26a0\ufe0f Empty tree - no models found", "")
    return(knitr::asis_output(paste(output, collapse = "\n")))
  }

  output <- c(
    output,
    "",
    paste0("<strong>", parts$title, "</strong>"),
    ""
  )
  output <- c(
    output,
    paste0("\u2139\ufe0f <strong>Models:</strong> ", parts$total_models),
    ""
  )

  for (root_idx in seq_along(parts$root_nodes)) {
    root_node <- parts$root_nodes[root_idx]
    tree_lines <- knit_print_tree_node(
      root_node,
      parts$tree_data,
      parts$nodes,
      parts$focal_display,
      level = 0
    )
    output <- c(output, tree_lines)

    if (root_idx < length(parts$root_nodes)) {
      output <- c(output, "")
    }
  }

  knitr::asis_output(paste(output, collapse = "\n"))
}

#' Helper function to recursively build tree structure in markdown
#' @param node_name Current node name
#' @param tree_data Tree data structure from build_cli_tree_data
#' @param nodes_info Original nodes information with descriptions
#' @param level Current indentation level
#' @return Character vector of markdown lines for this subtree
#' @keywords internal
#' @noRd
knit_print_tree_node <- function(
  node_name,
  tree_data,
  nodes_info,
  focal_display,
  level = 0
) {
  output <- character()

  # Create indentation
  indent <- paste(rep("  ", level), collapse = "")

  # Find node info
  mod_key <- paste0(node_name, ".mod")
  ctl_key <- paste0(node_name, ".ctl")
  node_key <- if (mod_key %in% names(nodes_info)) mod_key else ctl_key

  # Determine node type for styling
  all_parents <- tree_data$parent
  all_children <- unlist(tree_data$children)
  root_nodes <- setdiff(all_parents, all_children)

  is_root <- (node_name %in% root_nodes)
  children <- tree_data$children[tree_data$parent == node_name][[1]]
  is_leaf <- length(children) == 0
  is_focal <- node_name %in% focal_display

  # Apply HTML styling based on node type
  display_name <- if (is_focal) {
    paste0('<strong><u>', node_name, '</u></strong>')
  } else {
    node_name
  }
  styled_node <- if (is_root) {
    paste0('<strong style="color:blue">', display_name, '</strong>')
  } else if (is_leaf) {
    paste0('<span style="color:green">', display_name, '</span>')
  } else {
    paste0('<span style="color:orange">', display_name, '</span>')
  }

  # Add tags and description if available
  node_model <- nodes_info[[node_key]]$model
  has_tags <- !is.null(node_model) && length(node_model$tags) > 0
  has_desc <- !is.null(node_model) &&
    !is.null(node_model$description) &&
    nzchar(node_model$description)
  suffix <- ""
  if (has_tags) {
    suffix <- paste0(
      ' <span style="color:teal">',
      paste(node_model$tags, collapse = ", "),
      '</span>'
    )
  }
  if (has_desc) {
    desc_text <- node_model$description
    if (nchar(desc_text) > 50) {
      desc_text <- paste0(substr(desc_text, 1, 47), "...")
    }
    sep <- if (has_tags) ' <span style="color:gray">|</span> ' else ' '
    suffix <- paste0(
      suffix,
      sep,
      '<span style="color:gray">',
      desc_text,
      '</span>'
    )
  }
  node_line <- paste0(indent, "- ", styled_node, suffix)

  output <- c(output, node_line)

  # Recursively add children
  if (length(children) > 0) {
    for (child in children) {
      child_lines <- knit_print_tree_node(
        child,
        tree_data,
        nodes_info,
        focal_display,
        level + 1
      )
      output <- c(output, child_lines)
    }
  }

  return(output)
}

# ==============================================================================
# Lineage utility functions
# ==============================================================================

#' Get a model's ancestors
#'
#' @param mod A `hyperion_nonmem_model` object or a path to a `.mod`/`.ctl`
#'   file.
#' @return Character vector of ancestor project-relative paths (with
#'   extension, e.g., `"models/onecmt/run001.mod"`). Includes `mod` itself
#'   alongside its ancestors. Returns empty vector if the lineage has no
#'   ancestors.
#' @export
get_model_ancestors <- function(mod) {
  nodes <- get_model_lineage(to = mod)$nodes
  vapply(nodes, function(n) n$name, character(1))
}

#' Get a model's descendants
#'
#' @param mod A `hyperion_nonmem_model` object or a path to a `.mod`/`.ctl`
#'   file.
#' @return Character vector of descendant project-relative paths (with
#'   extension, e.g., `"models/onecmt/run002.mod"`). Does not include
#'   `mod` itself.
#' @export
get_model_descendants <- function(mod) {
  from_keys <- vapply(
    get_model_lineage(from = mod)$nodes,
    function(n) n$name,
    character(1)
  )
  # `slice(from = mod, to = mod)` resolves to just `mod` itself; strip it
  # out so the result is descendants only.
  self_key <- vapply(
    get_model_lineage(from = mod, to = mod)$nodes,
    function(n) n$name,
    character(1)
  )
  setdiff(from_keys, self_key)
}

#' Check if two models are in a direct lineage
#'
#' Returns TRUE if `m1` is an ancestor of `m2` or vice versa (i.e., they
#' are in a direct parent-child chain).
#'
#' @param m1 A `hyperion_nonmem_model` object or a path to a `.mod`/`.ctl`
#'   file.
#' @param m2 A `hyperion_nonmem_model` object or a path to a `.mod`/`.ctl`
#'   file.
#' @return Logical, TRUE if models are in direct lineage.
#' @export
are_models_in_lineage <- function(m1, m2) {
  length(get_model_lineage(from = m1, to = m2)$nodes) > 0 ||
    length(get_model_lineage(from = m2, to = m1)$nodes) > 0
}
