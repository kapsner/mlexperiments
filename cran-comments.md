## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.  
* Removed \dontrun{}-parts from `MLLearnerBase` (sorry, I thought this would be allowed in the case that examples cannot be executed at all; `MLLearnerBase` is a template-class that contains methods which need to be overwritten and filled with logic when inheriting from this class; the direct calling of the methods of this class does not make sense; however, the examples are not meaningless i.m.h.o. and are intended to demonstrated the expected use; the examples previously wrapped into \dontrun{}-parts are now commented out)

