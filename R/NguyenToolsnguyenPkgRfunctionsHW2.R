#' Given a numeric vector x and a numeric matrix A, calculates x'inv(A)x, where ' is transposition
#' 
#' Computes x'inv(A)x of a numeric matrix A and numeric vector x.
#' 
#' @param x numeric vector
#' @param A numeric matrix
#' @return scalar number computed by x'inv(A)x
#' @export
#' @examples 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' ls()
#' a <- as.matrix(a)
#' x <- as.matrix(x)
#' xAx(a, x)
xAx <- function(a, x)
{
  # Stop if a is not numeric or finite
  # Stop if a is NA
  stopifnot(is.numeric(a))
  stopifnot(is.finite(a))
  stopifnot(!is.na(a))
  
  # Stop if x is not numeric or finite
  # Stop if x is NA
  stopifnot(is.numeric(x))
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  
  # If row dim of vec x does not match dim of sq. mx. a
  # exit; this is because t(x) %*% will not have conforming dim
  if(dim(x)[1] != dim(a)[1])
  {
    #stopifnot(dim(x)[1] != dim(a)[1])
    stop("The matrix A and vector x have non-conformable arguments")
  }
  result <- t(x) %*% solve(a) %*% x
  return(result)
}



#' Given a numeric vector x and a numeric matrix A, calculates x'inv(A)x, where ' is transposition
#' This is computed using the function as a binary operator
#' 
#' Computes x'inv(A)x of a numeric matrix A and numeric vector x.
#' 
#' @param x numeric vector
#' @param A numeric matrix
#' @return scalar number computed by x'inv(A)x
#' @export
#' @examples 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' ls()
#' a <- as.matrix(a)
#' x <- as.matrix(x)
#' a %xAx% x
`%xAx%` <- function(a, x)
{
  # Stop if a is not numeric or finite
  # Stop if a is NA
  stopifnot(is.numeric(a))
  stopifnot(is.finite(a))
  stopifnot(!is.na(a))
  
  # Stop if x is not numeric or finite
  # Stop if x is NA
  stopifnot(is.numeric(x))
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  
  # If row dim of vec x does not match dim of sq. mx. a
  # exit; this is because t(x) %*% will not have conforming dim
  if(dim(x)[1] != dim(a)[1])
  {
    #stopifnot(dim(x)[1] != dim(a)[1])
    stop("The matrix A and vector x have non-conformable arguments")
  }
  result <- t(x) %*% solve(a) %*% x
  return(result)
}

#' Given a numeric matrix, the following 'standardize' function standardizes the columns of the matrix
#' 
#' Standardizes the columns of a numeric matrix. Standardization is computed as (value - mean(col))/(sd(col))
#' @param a numeric matrix
#' @return Standardized matrix
#' @export
#' @examples 
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' ls()
#' standardize(a)
standardize <- function(a)
{
  stopifnot(nrow(a) > 1)
  colmeans <- apply(a, 2, mean)
  colsds <- apply(a, 2, sd)
  # look at ?apply
  # apply returns an array of dimension c(n, dim(X)[MARGIN]) if n > 1.
  # In this test case, dim(a)[1] returns 6, the number of rows
  # The result returned will be a 4 * 6, while the original a was a 6 * 4
  # This is the transpose of the thing that we want
  # >> transpose solution to get desired solution.
  a <- t(apply(a, 1, function(a) (a - colmeans)/colsds))
  return(a)
}

#' Given a matrix X, an integer MARGIN in c(1,2) indicating 1 for rows, and 2 for columns, and a function FUN
#' Returns the output of what the base R function apply() would do to the matrix X
#'
#' Runs a function FUN along the MARGIN of a matrix X
#' @param X a matrix of any type
#' @param MARGIN a binary value taking on '1' for rows or '2' for columns of a matrix
#' @param FUN a function that operates along the MARGIN of the matrix X
#' @return The output that is identical to what apply() function family would return
#' @export
#' @examples
#' fred <- matrix(1:6, ncol = 2)
#' myapply(fred, 1, mean)
#' myapply(fred, 2, mean)
#' myapply(fred, 1, max)
#' myapply(fred, 2, max)
#' myapply(fred, 1, function(x) quantile(x, 0.75))
#' myapply(fred, 2, function(x) quantile(x, 0.75))
myapply <- function(X, MARGIN, FUN, ...)
{
  FUN <- match.fun(FUN)
  result <- list()
  if (MARGIN == 1)
  {
    for (i in 1:nrow(X))
    {
      # Do FUN along the rows of X
      # X[i, ] <- FUN(X[i, ])
      result[[i]] <- FUN(X[i, ])
    }
  } 
  else if (MARGIN == 2)
  {
    for (j in 1:ncol(X))
    {
      # Do FUN along the columns of x
      # X[, j] <- FUN(X[, j])
      result[[j]] <- FUN(X[, j])
    }
  }
  #return(X)
  return(simplify2array(result))
}


#' A wrapper that returns solutions to problem 6 of HW2 from STAT 3701
#' 
#' Wrapper function that returns output of the median function along margins of a 3D dataset
#' @export
#' @examples 
#' p6wrapper()
p6wrapper <- function()
{
  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p6.rda"))
  ls()
  pat
  # Apply the R function median to the three 2-d margins
  # Apply the R function median to the three 1-d margins
  # combinations are (1,2, ); (1, 3, ); (2, 3, );
  median(pat[1, 2, ])
  median(pat[1, 3, ])
  median(pat[2, 3, ])
  # the only 1-d margins that have 3 elements are the type
  # of operating system
  for (i in 1:ncol(pat))
  {
    print(median(pat[, i, ]))
  }
}