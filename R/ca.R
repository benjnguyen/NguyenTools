
           #' 
           #' This function takes dcauchy of data
           #' @export
           #' @examples
           #' x <- 1:10
           #' cau(2, x)
           cau = function(theta, x) dcauchy(x, location = theta, log = TRUE)
           