.onAttach <- function(libname, pkgname) {
  options <- c("Loading the ReliableTrendIndex package is not recommended, because you're probably not 
going to find a situation in which the reliability of change scores matters.", 
               "Think about your choices: would you rather develop a meaningful clinical test?
Or is reliability really the best possible idea?", 
               "You don't actually believe that the SD of a group has anything to do with any individual case, right?
That seems implausible for most constructs.", 
"Did you know that change scores are potentially misleading and 
end-state functioning is generally preferable as an evaluation of any treatment course?")
  packageStartupMessage(paste0("\n", "You loaded ReliableTrendIndex", "\n", sample(options, 1), "\n"))
}