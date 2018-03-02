
           #' 
           #' This vignette shows 3 ways to plot the data from gp2007
           #' @export
           gp2007 <- read.csv('http://users.stat.umn.edu/~almquist/3811_examples/gapminder2007ex.csv')
           ggplot2::ggplot(data=gp2007, ggplot2::aes(x=`gdpPercap`, y=`lifeExp`, size = `pop`, col = `continent`)) + ggplot2::geom_point()
           ggplot2::ggplot(data = gp2007, mapping = ggplot2::aes(x = gdpPercap)) + ggplot2::geom_histogram()
           ggplot2::ggplot(data = gp2007, ggplot2::aes(x = gdpPercap, y = lifeExp)) + ggplot2::geom_line()
           