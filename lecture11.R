### Lecture 11

## P-hacking example:

# Generate 26 groups of cholesterol data from same population
set.seed(5)
cholesterol <- replicate(27, rnorm(40,240,5))

# Column names for control and treatments
colnames(cholesterol) <- c("control", paste0("treat",LETTERS[1:ncol(cholesterol)-1]))  

# Collect my values here...
pvals <- NULL

control <- cholesterol[,1] # control data is first column

# Loop through 26 treatments

for (i in 1:(ncol(cholesterol))){
  treatment <- cholesterol[,i+1] # plus 1 to skip the control group
  print(paste0("Treatment ", LETTERS[i]))
  ttest <- t.test(control, treatment, alternative = "greater")
  print(ttest)
  pvals[i] <- ttest$p.value
}

# But let's adjust for the inflated risk of type 1 error from multiple comparisons:
# The conservative Bonferroni-Holm correction raises all values to 1!
p.adjust(pvals, method = "holm") 
# Even a less conservative correction says "Definitely NO!"
p.adjust(pvals, method = "fdr") 

# If you want to run "pairwise" t-tests (compare every group with every other group):
# pairwise.t.test(data$score, data$group)
