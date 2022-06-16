.onAttach <- function(libname, pkgname) {
  options <- c("Loading this package is not recommended, because you're probably not 
going to find a situation in which the reliability of change scores matters.", 
               "Think about your choices: would you rather develop a meaningful clinical test?
Or is reliability really the best possible idea?", 
               "You don't actually believe that the SD of a group has anything to do with the SD for your particular individual case, right?
That seems implausible for most constructs.")
  packageStartupMessage(sample(options, 1))
}