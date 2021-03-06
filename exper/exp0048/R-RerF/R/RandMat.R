#' Create a Random Matrix 
#'
#' Create a random matrix. At each node of a tree, a p-by-d random
#' matrix is sampled and used to generate a new set of d features, each
#' of which is a linear combination of the original p features. Thus,
#' the columns of the random matrix can be viewed as a set of bases in a
#' new feature space.
#' 
#' @param mat.options a list of parameters specifying the distribution
#' for sampling the random matrix. The first element specifies the
#' dimensionality p of the data (# features). The second element
#' specifies the number of columns d in the random matrix. The third
#' element specifies the "type" of distribution. The types of
#' distributions supported thus far are: "binary", "continuous",
#' "poisson", "rf", "frc", and "frcn." The fourth element specifies the
#' average density (proportion of nonzeros) rho of the random matrix.
#' For the "binary" distribution, rho is a real number between 0 and 1.
#' rho*d*p elements of the matrix are randomly chosen to be nonzero.
#' Each nonzero is randomly assigned -1 or 1 with equal probability. The
#' "continuous" distribution is the same as "binary", except the
#' nonzeros are sampled iid from the standard normal distribution.
#' "poisson" samples the number of nonzeros for each of the d columns
#' from a poisson(rho) distribution (rho is an integer >= 1). The
#' location of nonzeros in each column is randomly chosen, and the
#' nonzeros are assigned -1 or 1 with equal probability. "rf" randomly
#' samples d indices in {1,...,p} without replacement (d <= p is
#' required). Each of the d columns in the random matrix has a single
#' nonzero placed at the respective sampled index. This is equivalent to
#' the canonical random forest algorithm, which subsamples variables at
#' each node. Note that rho is irrelevant. "frc" is Breiman's forest-RC
#' algorithm, which samples rho nonzeros for each of the d columns (rho
#' must be an integer >= 1). The location of nonzeros in each column is
#' randomly chosen, and each nonzero location is assigned a value
#' uniformly randomly over the interval [-1,1]. "frcn" is the same as
#' "frc" except the nonzeros are sampled from the standard normal
#' distribution.  
#'
#' @return random.matrix
#'
#' @export
#'
#' @importFrom RcppZiggurat zrnorm
#' @importFrom stats rpois runif

RandMat <-
function(mat.options) {
  p <- mat.options[[1L]] # number of dimensions

  d <- mat.options[[2L]] # this determines the number of columns in the projection matrix.
  method <- mat.options[[3L]] # defines the distribution of the random projection matrix
  #Create the random matrix, a sparse matrix of 1's, -1's, and 0's.
  if (method == "binary") {
    rho <- mat.options[[4L]]
    prob <- mat.options[[5L]]
    nnzs <- round(p*d*rho)
    ind <- sort(sample.int((p*d), nnzs, replace = F))
    random.matrix <- cbind(((ind - 1L) %% p) + 1L, floor((ind - 1L) / p) + 1L,
                           sample(c(1L, -1L), nnzs, replace = T, prob = c(prob, 1 - prob)))
  } else if (method == "continuous") {
    rho <- mat.options[[4L]]
    nnzs <- round(p*d*rho)
    ind <- sort(sample.int((p*d), nnzs, replace = F))
    random.matrix <- cbind(((ind - 1L) %% p) + 1L, floor((ind - 1L) / p) + 1L,
                           zrnorm(nnzs))
  } else if (method == "rf") {
    random.matrix <- cbind(sample.int(p, d, replace = F), 1:d, rep(1L, d))
  } else if (method == "poisson") {
    lambda <- mat.options[[4L]]
    if (lambda <= 0) { 
      stop(" Wrong parameter for Poisson, make sure lambda > 0.") 
    }
    go <- T
    while (go) {
      nnzPerCol <- stats::rpois(d, lambda)
      go <- !any(nnzPerCol)
    }
    nnzPerCol[nnzPerCol > p] <- p
    nnz <- sum(nnzPerCol)
    nz.rows <- integer(nnz)
    nz.cols <- integer(nnz)
    start.idx <- 1L
    for (i in seq.int(d)) {
      if (nnzPerCol[i] != 0L) {
        end.idx <- start.idx + nnzPerCol[i] - 1L
        nz.rows[start.idx:end.idx] <- sample.int(p, nnzPerCol[i], replace = F)
        nz.cols[start.idx:end.idx] <- i
        start.idx <- end.idx + 1L
      }
    }
    random.matrix <- cbind(nz.rows, nz.cols, sample(c(-1L,1L), nnz, replace = T))
  } else if (method == "frc") {
    nmix <- mat.options[[4L]]
    nnz <- nmix*d
    nz.rows <- integer(nnz)
    nz.cols <- integer(nnz)
    start.idx <- 1L
    for (i in seq.int(d)) {
      end.idx <- start.idx + nmix - 1L
      nz.rows[start.idx:end.idx] <- sample.int(p, nmix, replace = F)
      nz.cols[start.idx:end.idx] <- i
      start.idx <- end.idx + 1L
    }
    random.matrix <- cbind(nz.rows, nz.cols, stats::runif(nnz, -1, 1))
  } else if (method == "frcn") {
    nmix <- mat.options[[4L]]
    nnz <- nmix*d
    nz.rows <- integer(nnz)
    nz.cols <- integer(nnz)
    start.idx <- 1L
    for (i in seq.int(d)) {
      end.idx <- start.idx + nmix - 1L
      nz.rows[start.idx:end.idx] <- sample.int(p, nmix, replace = F)
      nz.cols[start.idx:end.idx] <- i
      start.idx <- end.idx + 1L
    }
    random.matrix <- cbind(nz.rows, nz.cols, zrnorm(nnz))
  } else if (method == "ts-patch") {
    # pw holds all sizes of patch to filter on. 
    # There will be d patches of varying sizes
    pw.min <- mat.options[[4L]] # Minimum patch size
    pw.max <- mat.options[[5L]] # Maximum patch size
    pw <- sample.int(pw.max - pw.min, d, replace=T) + pw.min
    
    # nnz is sum over how many points the projection will sum over
    nnz <- sum(pw)
    nz.rows <- integer(nnz) # vector to hold row coordinates of patch points
    nz.cols <- integer(nnz) # vector to hold column coordinates of patch points
    
    # Here we create the patches and store them
    start.idx <- 1L
    for (i in seq.int(d)) {
      pw.start <- sample.int(p, 1) # Sample where to start the patch
      end.idx <- start.idx + pw[i] - 1L # Set the ending point of the patch
      for (j in 1:pw[i]) {
        # Handle boundary cases where patch goes past end of ts
        if (j + pw.start - 1L > p) {
          end.idx <- j + start.idx - 1L
          break
        }
        nz.rows[j + start.idx - 1L] <- pw.start + j - 1L
        nz.cols[j + start.idx - 1L] <- i
      }
      start.idx <- end.idx + 1L
    }
    random.matrix <- cbind(nz.rows, nz.cols, rep(1L, nnz))
    random.matrix <- random.matrix[random.matrix[,1] > 0,] # Trim entries that are 0
  } else if (method == "image-patch") {
    iw <- mat.options[[4L]] # image width
    ih <- mat.options[[5L]] # image height
    pw.min <- mat.options[[6L]] # minimum patch width
    pw.max <- mat.options[[7L]] # maximum patch width
    pw <- sample.int(pw.max - pw.min + 1L, 2*d, replace = T) + pw.min - 1L
    sample.height <- ih - pw[1:d] + 1L
    sample.width <- iw - pw[(d + 1L):(2*d)] + 1L
    nnz <- sum(pw[1:d]*pw[(d + 1L):(2*d)])
    nz.rows <- integer(nnz)
    nz.cols <- integer(nnz)
    start.idx <- 1L
    for (i in seq.int(d)) {
      top.left <- sample.int(sample.height[i]*sample.width[i], 1L)
      top.left <- floor((top.left - 1L)/sample.height[i])*(ih - sample.height[i]) + top.left
      # top.left <- floor((top.left - 1L)/sample.height[i]) + top.left
      end.idx <- start.idx + pw[i]*pw[i + d] - 1L
      nz.rows[start.idx:end.idx] <- sapply((1:pw[i + d]) - 1L, function(x) top.left:(top.left + pw[i] - 1L) + x*ih)
      nz.cols[start.idx:end.idx] <- i
      start.idx <- end.idx + 1L
    }
    # random.matrix <- cbind(nz.rows, nz.cols, sample(c(-1L,1L), nnz, replace = T))
    random.matrix <- cbind(nz.rows, nz.cols, rep(1L, nnz))
  } else if (method == "image-control") {
    iw <- mat.options[[4L]] # image width
    ih <- mat.options[[5L]] # image height
    pw.min <- mat.options[[6L]] # minimum patch width
    pw.max <- mat.options[[7L]] # maximum patch width
    pw <- sample.int(pw.max - pw.min + 1L, 2*d, replace = T) + pw.min - 1L
    nnzPerCol <- pw[1:d]*pw[(d + 1L):(2*d)]
    sample.height <- ih - pw[1:d] + 1L
    sample.width <- iw - pw[(d + 1L):(2*d)] + 1L
    nnz <- sum(nnzPerCol)
    nz.rows <- integer(nnz)
    nz.cols <- integer(nnz)
    start.idx <- 1L
    for (i in seq.int(d)) {
      end.idx <- start.idx + nnzPerCol[i] - 1L
      nz.rows[start.idx:end.idx] <- sample.int(p, nnzPerCol[i], replace = F)
      nz.cols[start.idx:end.idx] <- i
      start.idx <- end.idx + 1L
    }
    # random.matrix <- cbind(nz.rows, nz.cols, sample(c(-1L,1L), nnz, replace = T))
    random.matrix <- cbind(nz.rows, nz.cols, rep(1L, nnz))
  } else if (method == "custom") {
    nnz.sample <- mat.options[[4L]]
    try(if(any(nnz.sample > p) | any(nnz.sample == 0)) stop("nnzs per projection must be no more than the number of features"))
    nnz.prob <- mat.options[[5L]]
    go <- T
    nnzPerCol <- sample(nnz.sample, d, replace = T, prob = nnz.prob)
    nnz <- sum(nnzPerCol)
    nz.rows <- integer(nnz)
    nz.cols <- integer(nnz)
    start.idx <- 1L
    for (i in seq.int(d)) {
      end.idx <- start.idx + nnzPerCol[i] - 1L
      nz.rows[start.idx:end.idx] <- sample.int(p, nnzPerCol[i], replace = F)
      nz.cols[start.idx:end.idx] <- i
      start.idx <- end.idx + 1L
    }
    random.matrix <- cbind(nz.rows, nz.cols, zrnorm(nnz))
  }
  return(random.matrix)
}
