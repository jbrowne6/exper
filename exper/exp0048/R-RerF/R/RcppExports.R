# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

packForestRCPP <- function() {
    .Call('_rerf_packForestRCPP', PACKAGE = 'rerf')
}

predictRF <- function(mat, numCores) {
    .Call('_rerf_predictRF', PACKAGE = 'rerf', mat, numCores)
}

findSplit <- function(x, y, ndSize, I, maxdI, bv, bs, nzidx, cc) {
    .Call('_rerf_findSplit', PACKAGE = 'rerf', x, y, ndSize, I, maxdI, bv, bs, nzidx, cc)
}

