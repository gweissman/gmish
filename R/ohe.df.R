#' One-hot encodes all categorical variables in a data.frame
#'@export
#'
#' @param df A data.frame that can contain columns of any variable type.
#' @param drop_ref Logical, default = \code{TRUE}. Will return x - 1 columns for x unique categories. Choose \code{FALSE} to return x columns.
#' @return A data.frame of all columns of class factor returned as one-hot encoded. All other column types are returned untouched.
#' @examples
#' df <- data.frame(lets = rep(letters[1:3], 3), LETS = rep(LETTERS[1:3], 3), x = rnorm(9))
#' ohe.df(df)
ohe.df <- function(df, drop_ref = TRUE) {
  data.frame(lapply(df, function(x) if(is.factor(x)) { ohe(x, drop_ref = drop_ref) } else { x } ))
}

