### Lecture 16: Chi-square

## Chi-square test of goodness of fit:
# Testing one variable against an expected distribution

# Are draws uniformly distributed across A, B, and C?
ice_cream <- c(chocolate = 20, vanilla = 15, pisatchio = 25)

barplot(ice_cream,
        xlab = "Flavors",
        ylab = "Freq",
        main = "Ice Cream Sales",
        col = c("chocolate4","cornsilk", "darkseagreen1"))

expected <- c(chocolate = 20, vanilla = 20, pisatchio = 20)

barplot(expected,
        xlab = "Flavors",
        ylab = "Freq",
        main = "Ice Cream Sales (Expected)",
        col = c("chocolate4","cornsilk", "darkseagreen1"))

# Optional because expected values are all equal:
obs_exp <- data.frame(observed = ice_cream, expected = expected)

# Chi-square goodness of fit
chisq.test(obs_exp)

# What if I want to test against the null hypothesis that 
# membership in A is twice as probable as B or C:
ice_cream <- c(chocolate = 20, vanilla = 15, pisatchio = 25)
chisq.test(ice_cream, p = c(.50,.25,.25)) # or
chisq.test(ice_cream, p = c(2,1,1), rescale.p = T)

## Chi-squared test of independence: 
# Testing two variables against each other

# Here's an example from the help documentation:
# H0: Party and gender are independent.
politics <- data.frame(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(politics) <- list(gender = c("F", "M"), party = c("Democrat","Independent", "Republican"))
politics
chisq.test(politics)  # probably not!

# Post-hoc (don't forget to adjust your p-values to correct for multiple comparisons)
chisq.test(politics$Democrat) # Compare frequencies of Democreat Males and Democrat Females
chisq.test(politics[1,c(1,2)]) # Compare frequencies of Democrat Females and Independent Females 

## Effect size
# Degree of dependence between party and gender:
# Phi:
sqrt(chisq.test(politics)$statistic/100)

# Cramer's v:
min_row_or_col = 2
sqrt(chisq.test(politics)$statistic/(100*(min_row_or_col-1)))
