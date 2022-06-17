.onAttach <- function(libname, pkgname) {
  options <- c("Loading the ReliableTrendIndex package is not recommended, because you're probably not 
going to find a situation in which the reliability of change scores matters.", 
               "You've loaded the ReliableTrendIndex package. 
Think about your choices: would you rather develop a meaningful clinical test?
Or is reliability really the best possible idea?", 
               "You've loaded the ReliableTrendIndex package. 
You don't actually believe that the SD of a group has anything to do with any individual case, right?
That seems implausible for most constructs.")
  packageStartupMessage(sample(options, 1))
}