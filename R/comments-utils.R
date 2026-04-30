#' Convert comment list to data frame with values
#' @param comments Named list of comment objects
#' @param fields Character vector of field names to extract
#' @param value_resolver Function(comment, field) -> value or NULL
#' @return Data frame with parameter column and value columns
#' @noRd
comment_list_to_df <- function(comments, fields, value_resolver) {
  if (length(comments) == 0) {
    df <- data.frame(parameter = character(), stringsAsFactors = FALSE)
    for (f in fields) {
      df[[f]] <- character()
    }
    return(df)
  }

  rows <- lapply(names(comments), function(nm) {
    cmt <- comments[[nm]]
    row <- data.frame(parameter = nm, stringsAsFactors = FALSE)
    for (f in fields) {
      val <- value_resolver(cmt, f)
      if (is.null(val)) {
        row[[f]] <- NA_character_
      } else if (length(val) > 1) {
        row[[f]] <- paste(val, collapse = ", ")
      } else {
        row[[f]] <- val
      }
    }
    row
  })
  do.call(rbind, rows)
}

#' Build comment tables for theta/omega/sigma slots
#' @param comments_list Named list of comment lists
#' @param fields_list Named list of fields vectors
#' @param value_resolver Function(comment, field) -> value or NULL
#' @return Named list of data frames
#' @noRd
build_comment_tables <- function(comments_list, fields_list, value_resolver) {
  tables <- list()
  for (slot in names(comments_list)) {
    tables[[slot]] <- comment_list_to_df(
      comments_list[[slot]],
      fields_list[[slot]],
      value_resolver
    )
  }
  tables
}

#' Make a path relative to project root (pharos.toml directory)
#' @noRd
relative_path <- function(path) {
  if (is.null(path) || path == "default" || path == "user supplied") {
    return(path)
  }
  tryCatch(
    {
      config_path <- find_pharos_config_file()
      if (grepl("No pharos.toml", config_path)) {
        return(path)
      }
      root <- fs::path_dir(config_path)
      as.character(fs::path_rel(path, start = root))
    },
    error = function(e) path
  )
}

#' Set source paths for comment fields
#'
#' Always initializes the sources attribute to mark object as "initialized".
#' Fields with non-NULL values get source_path; NULL fields get "default".
#' @noRd
set_sources <- function(comment, fields, source_path) {
  source_path <- relative_path(source_path)
  sources <- list()
  for (f in fields) {
    val <- S7::prop(comment, f)
    if (!is.null(val)) {
      sources[[f]] <- source_path
    } else {
      sources[[f]] <- "default"
    }
  }
  attr(comment, "sources") <- sources
  comment
}

#' @noRd
create_comment_with_sources <- function(constructor, fields, mod_path, ...) {
  comment <- constructor(...)
  set_sources(comment, fields, mod_path)
}
