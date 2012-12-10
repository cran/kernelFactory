.onAttach <-
function(libname, pkgname) {
    kFversion <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                      fields="Version")
    packageStartupMessage(paste(pkgname, kFversion ))
    packageStartupMessage("Type kFNews() to see the change log")
}
