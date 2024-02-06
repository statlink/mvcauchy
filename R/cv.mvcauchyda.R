cv.mvcauchyda <- function(x, ina, nfolds = 10, folds = NULL, stratified = TRUE, seed = NULL) {
  ina <- as.numeric(ina)
  if ( is.null(folds) )  folds <- .makefolds(ina, nfolds = nfolds,
                                             stratified = stratified, seed = seed)
  nfolds <- length(folds)
  per <- numeric(nfolds)
  runtime <- proc.time()

  for (vim in 1:nfolds) {
    test <- x[ folds[[ vim ]], , drop = FALSE ]   ## test sample
    id <- ina[ folds[[ vim ]] ] ## groups of test sample
    train <- x[ -folds[[ vim ]], , drop = FALSE]  ## training sample
    ida <- ina[ -folds[[ vim ]] ]   ## groups of training sample
    group <- mvcauchy::mvcauchy.da(test, train, ida)$est
    per[vim] <- mean( group == id )
  }  ##  end  for (vim in 1:nfolds) {

  runtime <- proc.time() - runtime
  perf <- mean(per)
  list(perf = perf, runtime = runtime)
}
