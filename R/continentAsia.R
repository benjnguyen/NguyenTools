
           #' 
           #' This function takes slice of data from gp2007
           #' @export
           #' @examples
           #' continentAsia()
           continentAsia <- function()
           {
           library(magrittr)
           gp2007 %>%
           dplyr::filter(continent == 'Asia')
           }