ensure.directory <- function(dir) {
  if (!dir.exists(dir)) {dir.create(dir)}
}
