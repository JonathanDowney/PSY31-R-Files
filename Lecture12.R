### Lecture 12

install.packages(xtable) # run this if xtable isn't installed yet
library(xtable) # for making ANOVA tables in LaTeX

## Data snooping example:

# Generate the fake data again: 26 groups of cholesterol data from same population
set.seed(5)
cholesterol <- replicate(27, rnorm(40,240,5))

# Add column names for control and treatments
colnames(cholesterol) <- c("control", paste0("treat",LETTERS[1:ncol(cholesterol)-1]))  

# Let's snoop on the medians:
boxplot(cholesterol,
        names = c("*", LETTERS[1:26]),
        main = "Cholesterol Treatements",
        xlab = "Treatment (* = control)",
        ylab = "Cholesterol Level",)
abline(h=mean(cholesterol[,"control"]), lwd=2, col = "red")

cholesterol_control <- cholesterol[,1]
cholesterol_H <- cholesterol[,8]
cholesterol_W <- cholesterol[,23]

t.test(cholesterol_control, cholesterol_H, alternative = "greater")
t.test(cholesterol_control, cholesterol_W, alternative = "greater")

# Can I get away with just adjusting for 2x multiple comparison (instead of 26x)?
p.adjust(c(0.6979,0.3672)) # adjusts to 0.7344 and 0.7344 with Holm adjustment
# No! This is "snooping"

## ANOVA example

# Let's make some fake data: "Points scored in the last 12 games"

nuggets <- c(104,103,108,88,113,100,94,98,87,104,101,97)
warriors <- c(103,108,101,100,84,92,102,91,94,93,101,94)
celtics <- c(100,88,92,89,92,92,82,100,93,93,96,93)

# make a "long format" data frame:

nba <- data.frame(score = c(nuggets, warriors, celtics),    
                  team =  factor(c(
                    rep("nuggets",12),
                    rep("warriors",12),
                    rep("celtics",12))))

# If you want to run "pairwise" t-tests (compare every group with every other group)"
pairwise.t.test(nba$score, nba$team)

# My beautiful ANOVA charts:

# Chart with group and grand means:
colors <- c("green","gold","blue")
plot.default(nba$score ~ nba$team,
             xlab =  "team",
             ylab = "points scored",
             main = "Points per game by team",
             pch = 1, # hollow points,
             xaxt = "n",
             xlim = range(as.integer(unique(nba$team))) + c(-0.4, 0.4),
             ylim = range(as.integer(unique(nba$score))),
             col = colors[nba$team]
)
axis(1, at = seq_along(levels(nba$team)), labels = levels(nba$team))
team_means <- aggregate(nba$score, by = list(nba$team), mean)
abline(h = mean(nba$score), col = 'grey', lwd = 2)
points(1:3, team_means$x,col = "red", pch = 16)

# Within-group variance (no group and grand means):
colors <- c("green","gold","blue")
plot.default(nba$score ~ nba$team,
             xlab =  "team",
             ylab = "points scored",
             main = "Points per game by team",
             pch = 1, # hollow points,
             xaxt = "n",
             xlim = range(as.integer(unique(nba$team))) + c(-0.4, 0.4),
             ylim = range(as.integer(unique(nba$score))),
             col = colors[nba$team]
)
axis(1, at = seq_along(levels(nba$team)), labels = levels(nba$team))

# Within-group SD
aggregate(nba$score, by = list(nba$team), sd)

# Chart with only team means (small variance)
plot.default(c(1,1,1), team_means$x, 
             xlab =  "all teams",
             ylab = "points scored",
             main = "Team means",
             pch = 16, 
             ylim = range(as.integer(unique(nba$score))), # same range as before
             col = "red",
             xaxt = "n",
)

# SD of team means
sd(team_means$x)

# Chart with all data
plot.default(rep(1, nrow(nba)), nba$score, 
             xlab =  "all teams",
             ylab = "points scored",
             main = "Points scored (all teams)",
             pch = 1, 
             ylim = range(as.integer(unique(nba$score))), # same range as before
             col = colors[nba$team],
             xaxt = "n")

# Grand mean
abline(h = mean(nba$score), col = 'grey', lwd = 2)
legend("topright", legend=c("celtics","nuggets","warriors"), fill = colors, title="Group")

# SD of all scores
sd(nba$score)

# The actual ANOVA:

my_anova <- aov(nba$score ~ nba$team)
summary(my_anova)

# Let's make a snazzy LaTeX table:
xtable(my_anova) # copy and paste the resulting code into your markdown!
