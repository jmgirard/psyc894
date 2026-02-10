# _common.R

if (is.null(knitr::knit_hooks$get("source_orig"))) {
  old_source_hook <- knitr::knit_hooks$get("source")
  knitr::knit_hooks$set(source_orig = old_source_hook)
  knitr::knit_hooks$set(source = function(x, options) {
    if (!is.null(options$replace_path)) {
      x <- gsub(options$replace_path[1], options$replace_path[2], x, fixed = TRUE)
    }
    old_source_hook(x, options)
  })
}
