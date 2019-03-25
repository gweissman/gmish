#' One-hot encodes a single data.frame column or vector

#' @param var A vector or data.frame column of categorical variables
#' @param drop_ref Logical, default = \code{TRUE}. Will return x - 1 columns for x unique categories. Choose \code{FALSE} to return x columns.
#' @return A data.frame of columns each representing a category in \code{var} with binary indicators 0/1.
#' @examples
#' # Encode a vector
#' lets <- rep(letters[1:3], 5)
#' ohe(lets)
#'
#' # Encode a single column of a data.frame
#' df < data.frame(lets, x = runif(9))
#' one(df$lets)
#'
#' # Encode just the categorical columns and combine
#' df2 <- data.frame(lets, LETS = rep(LETTERS[1:3], 3), x = rnorm(9))
#' data.frame(lapply(df2, function(x) if(is.factor(x)) { ohe(x) } else { x } ))
ohe <- function(vars, drop_ref = TRUE) {
  vname <- deparse(substitute(vars))
  lvls <- unlist(unique(vars))
  tmp_list <- list()
  for (lev in lvls) {
    tmp_list[[make.names(paste0(vname,'_',lev))]] <- as.numeric(vars == lev)
  }
  res <- as.data.frame(tmp_list)
  if (drop_ref) {
    return(res[,-1])
  } else {
    return(res)
  }
}
