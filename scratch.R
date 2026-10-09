pkgload::load_all("/Users/mattsmith/Documents/hyperion.mcp", quiet = TRUE)
reg <- operation_registry()
n <- unique(unlist(lapply(names(reg), function(x) names(operation_formals(x, reg[[x]])))))
print(sort(n[grepl("path|file|dir|to$|lookup|spec$|yspec|data", n)]))
