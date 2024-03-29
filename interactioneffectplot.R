## Interaction effect plot example

library(plotly)
library(reshape2)

# Load data

Sepal.Length <- rnorm(500,1,.25)
Sepal.Width <- rnorm(500,1,.25)
Petal.Length <- .7*(Sepal.Length * Sepal.Width)

iris <- data.frame(Sepal.Length = Sepal.Length,Sepal.Width=Sepal.Width,Petal.Length=Petal.Length)

my_df <- iris

# Estimate linear model
petal_lm <- lm(Petal.Length ~ 0 + Sepal.Length * Sepal.Width,data = my_df)

graph_reso <- 0.05

#Setup Axis
axis_x <- seq(min(my_df$Sepal.Length), max(my_df$Sepal.Length), by = graph_reso)
axis_y <- seq(min(my_df$Sepal.Width), max(my_df$Sepal.Width), by = graph_reso)

#Sample points
petal_lm_surface <- expand.grid(Sepal.Length = axis_x,Sepal.Width = axis_y,KEEP.OUT.ATTRS = F)
petal_lm_surface$Petal.Length <- predict.lm(petal_lm, newdata = petal_lm_surface)
petal_lm_surface <- acast(petal_lm_surface, Sepal.Width ~ Sepal.Length, value.var = "Petal.Length") #y ~ x

hcolors=c("red","blue","green")[my_df$Species]
iris_plot <- plot_ly(my_df, 
                     x = ~Sepal.Length, 
                     y = ~Sepal.Width, 
                     z = ~Petal.Length,
                
                     type = "scatter3d", 
                     mode = "markers",
                     marker = list(color = hcolors))

iris_plot <- add_trace(p = iris_plot,
                       z = petal_lm_surface,
                       x = axis_x,
                       y = axis_y,
                       type = "surface")

iris_plot