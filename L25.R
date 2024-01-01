## Lecture 25


## Q&A examples

lm(mpg ~ vs + am, data = mtcars) |> summary()

library(MASS)
lm(Pulse ~ Exer + Smoke, data = survey) |> summary()
lm(Pulse ~ Smoke, data = survey) |> summary()
