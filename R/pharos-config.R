#' Set comment parsing type in pharos.toml
#'
#' Writes `type = <value>` under the `[nonmem.comments]` section of
#' `pharos.toml`.
#'
#' @param type Comment parsing type. One of `"type1"` (strict structured) or
#'   `"type2"` (flexible structured grammar).
#' @param path Path to pharos.toml. If NULL, finds it automatically.
#' @return The path to the modified pharos.toml file (invisibly).
#' @export
#'
#' @examples \dontrun{
#' use_comments("type2")
#' }
use_comments <- function(type = c("type1", "type2"), path = NULL) {
  type <- match.arg(type)

  if (is.null(path)) {
    path <- find_pharos_config_file()
    if (grepl("No pharos.toml", path)) {
      rlang::abort("pharos.toml not found. Run init() first.")
    }
  }

  toml <- tomledit::read_toml(path)
  nonmem <- tomledit::get_item(toml, "nonmem")
  nonmem$comments$type <- type
  toml <- tomledit::insert_items(toml, nonmem = nonmem)
  tomledit::write_toml(toml, path)

  invisible(path)
}

#' @rdname use_comments
#' @export
use_type1_comments <- function(path = NULL) {
  .Deprecated("use_comments", old = "use_type1_comments")
  use_comments(type = "type1", path = path)
}
