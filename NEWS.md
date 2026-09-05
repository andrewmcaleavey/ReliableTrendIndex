# ReliableTrendIndex 0.3.1

* Extended `rti()` to accept static, per-timepoint, or function-valued
  measurement-error definitions and compute heteroskedastic slope standard
  errors.
* Added `rc.type = "jt"`, `"maassen"`, or `"mcnemar"` to `rti()`, including
  support for variable error inputs in each mode.
* Added tests confirming two-timepoint equivalence between `rti()` and `rci()`
  under Maassen and McNemar error specifications.
* Restored lifecycle/deprecation documentation for legacy wrappers and fixed
  `?rci` to use `rci()` as the primary documented function.
* Fixed vignette title metadata and replaced deprecated Pandoc MathJax options.

# ReliableTrendIndex 0.3.0

* Established rti(), rci(), and rti_by() as the supported analysis API.
* Added shared internal calculation helpers and regression tests for the
  supported API.
* Deprecated historical wrappers, which remain functional until at least
  version 1.0.0. See the new migration vignette for replacements.

# ReliableTrendIndex 0.2.0

The purpose of this update is to reduce reliance on `{metafor}` and better-document the RTI itself.  

* Added new backend for `rti()` and `rci`.  
* Added new vignettes: Get started with RTI and Introduction.  
* Updated `README`  
* Improved documentation across most functions.  
* Better results when running `R CMD CHECK`: currently no errors, no warnings, and 1 note.  
* Reduced dependencies on external packages.  
* Now requires base R pipes `|>`.  
* Now requires R (>= 4.1.0).  

# ReliableTrendIndex 0.1.0.9000 (development version)

Changed to development version on the main branch. To indicate that this is still a work in progress even though the version number can increment more. 

# ReliableTrendIndex 0.1.0

Initial "release." Indicates that the basic functions now work, even though they may be slow, inefficient, or imperfectly documented. Further development should not overwrite these functions until tested.  

* Added a `NEWS.md` file to track changes to the package.
