
           #'
           #' This function returns mle
           #' @export
           #' @examples
           #' ga_data <- scan(url('http://www.stat.umn.edu/geyer/3701/data/q1p3.txt'))
           #' ga <- function(theta, x) dgamma(x, shape = theta, log = TRUE)
           #' lab5(ga_data,ga,c(0,3))
           lab5 = function(X,density,interval)
           {
           log1 = function(theta,x=X)
           {
           sum(density(theta,x))
           }
           oout <- optimize(log1, maximum = TRUE, interval)
           return(oout$maximum)
           }
           