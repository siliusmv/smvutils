

#' @export
smv_size = function(x = NULL, recursive = 0) {
  if (is.null(x)) x = parent.frame()
  if (length(x) == 0) return(0)
  if (!typeof(x) %in% c("environment", "list")) return(as.numeric(lobstr::obj_size(x)))
  obj_sizes = sapply(x, function(y) as.numeric(lobstr::obj_size(as.list(y))))
  if (is.null(names(obj_sizes))) names(obj_sizes) = paste0(".", seq_along(obj_sizes))
  noname_index = which(names(obj_sizes) == "")
  if (length(noname_index) > 0) names(obj_sizes)[noname_index] = paste0(".", noname_index)
  if (recursive > 0) {
    obj_types = sapply(x, typeof)
    cleanup_index = NULL
    for (i in seq_along(obj_types)) {
      if (obj_types[i] %in% c("environment", "list")) {
        obj_name = names(obj_sizes)[i]
        new_obj_sizes = smv_size(x = as.list(x)[[i]], recursive = recursive - 1)
        names(new_obj_sizes) = paste0(obj_name, "/", names(new_obj_sizes))
        obj_sizes = c(obj_sizes, new_obj_sizes)
        cleanup_index = c(cleanup_index, i)
      }
    }
    if (length(cleanup_index) > 0) obj_sizes = obj_sizes[-cleanup_index]
  }
  sort(obj_sizes, decreasing = TRUE)
}
