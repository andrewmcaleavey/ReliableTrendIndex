.onAttach <- function(libname, pkgname) {
  options <- c("a", "b")
  packageStartupMessage(sample(options))
}