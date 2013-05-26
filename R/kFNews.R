kFNews <-
function() {
    newsfile <- file.path(system.file(package="kernelFactory"), "NEWS")
    file.show(newsfile)
}
