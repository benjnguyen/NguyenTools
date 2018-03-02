
           #' 
           #' This function takes dgamma of data
           #' @export
           #' @examples
           #' x <- 1:10
           #' ga(2, x)
           ga  = function(theta, x) dgamma(x, shape = theta, log = TRUE)
           