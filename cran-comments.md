## Resubmission
This is a resubmission, with the goal of fixing the following issues which the following NOTE in the check results on `r-devel-linux-x86_64-debian-clang`, `r-devel-linux-x86_64-debian-gcc`, `r-devel-linux-x86_64-fedora-clang`, and `r-devel-linux-x86_64-fedora-gcc`:

    checking use of SHLIB_OPENMP_*FLAGS in Makefiles ... NOTE
      src/Makevars.win: SHLIB_OPENMP_CXXFLAGS is included in PKG_CXXFLAGS but not in PKG_LIBS
      src/Makevars.win: SHLIB_OPENMP_CFLAGS is included in PKG_LIBS but not in PKG_CFLAGS
    Use of these macros is discussed in sect 1.2.1.1 of ???Writing R
    Extensions???. The macros for different languages may differ so the
    matching macro must be used in PKG_CXXFLAGS (etc) and match that used
    in PKG_LIBS (except for F77: see the manual).


## Test environments
* local os X install, R 3.5.1
* Ubuntu 14.04 install (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies of this package.
