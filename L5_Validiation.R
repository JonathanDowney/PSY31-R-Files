# Lecture 5: Validity discussion

# Nerv2 as 1-2
nerv2 <- unclass(as.factor(df$nerv2))
mean(nerv2)
sd(nerv2)

# Nerv4 as 1-4
nerv4 <- unclass(as.factor(df$nerv4))
mean(nerv4/2)
sd(nerv4/2)

# Conf4 as 1-4
conf4 <- unclass(as.factor(df$conf4))
conf5 <- unclass(as.factor(df$conf5))
conf13 <- unclass(as.factor(df$conf13))

mean(nerv4/2)
sd(nerv4/2)

# Boxplot
boxplot(nerv2, nerv4)
boxplot(conf4, conf5, conf13)

plot(jitter(nerv2,.5), jitter(rev(nerv4),.5),
     xlab = "Nervousness (1-2)",
     ylab = "Nervousness (1-4)",
     main = "How Nervious Are You?",
     axes = FALSE)
axis(side = 1, at = c(1,2))
axis(side = 2, at = c(1,2,3,4))

plot(jitter(conf,.5), jitter(rev(nerv4),.5),
     xlab = "Nervousness (1-2)",
     ylab = "Nervousness (1-4)",
     main = "How Nervious Are You?",
     axes = FALSE)
axis(side = 1, at = c(1,2))
axis(side = 2, at = c(1,2,3,4))

boxplot(
  jitter(conf4*13/6.5,.5),
  jitter(conf13*4/6.5,.5),
  main = "How confident are you? (Rescaled, 1-10)",
  names = c("(Scale: 1-5)","(Scale: 1-13)"))

plot(
  jitter(conf4,.35),
  jitter(conf13,.35),
  main = "How confident are you?",
  xlab = "Scale: 1-5",
  ylab = "Scale: 1-13",
  ylim = c(1,13),
  yaxt='n')
axis(side = 2, cex.axis = 0.75, at = c(1:13))

mean(conf4*13/6.5)
mean(conf13*4/6.5)



