### Lecture 23

## Data types and data structures
set.seed(123)

num_vec <- c(1,2,3) # numeric vector
char_vec <- c("one", "two", "three") # character vector

unordered <- c("one", "two", "three", "one", "three", "three", "two", "three", "one") 
table(unordered)
barplot(table(unordered), col = rainbow(15)[8:10])

ordered <- factor(unordered, levels = c("one","two","three"))
table(ordered)
barplot(table(ordered), col = rainbow(15)[8:10])

## Another example

# Make some data:
months_obs <- sample(month.name, 30, replace = T)
barplot(table(months_obs), col = rainbow(40)[20:31])

month.name # character vector of month abbreviations
months_obs_ordered <- factor(months_obs, levels = month.name)
barplot(table(months_obs_ordered), col = rainbow(40)[20:31])

# Vector of abbreviated month names (preloaded in R)
month.abb

# Abbreviate names on barplot
barplot(table(months_obs_ordered),
        col = rainbow(40)[20:31],
        names.arg = month.abb)

barplot(table(months_obs_ordered),
        col = c(rev(heat.colors(6)), heat.colors(6)),
        names.arg = month.abb,
        main = "Days Off in 2022",
        ylab = "# of Days")

barplot(table(months_obs_ordered),
        col = c(c("blue","darkgreen","green", "yellow","orange","red"),
                rev(c("blue","darkgreen","green", "yellow","orange","red"))),
        names.arg = month.abb)

## Changing factor structures to numeric

# Can I change this data to numeric?

as.numeric(unordered) # No
as.numeric(ordered)   # Yes, because an order of levels has been specified!

# But watch out:

unordered2 <- c("four", "two", "three", "four", "three", "three", "two", "three", "four") 
ordered2 <- factor(unordered2, levels = c("two","three","four"))
table(ordered2)
as.numeric(ordered2) # Probably not what you want!

ordered2a <- factor(unordered2, levels = c("two","three","four"))
as.numeric(ordered2a)

ordered2b <- factor(unordered2, levels = c("one","two","three","four"))
as.numeric(ordered2b)


## Interaction effects practice: Theme Park

# Let's create some fake data:
set.seed(146)
n <- 175 
ride_speed <- rnorm(n,20,5)
cotton_candy <- sample(c(0,1), n, replace=T)
smiles <- .2*ride_speed + 5*cotton_candy - .2*(ride_speed*cotton_candy) + rnorm(n,10,4)
themepark <- data.frame(smiles = smiles, ride_speed = ride_speed, cotton_candy = cotton_candy)

# Hypothesis: negative interaction between ride_speed and cotton_candy

# Q1: Why might Dr. Downey hypothesize an interaction effect?

# Speed-induced tummy aches? 

# Q2: Is there evidence of an interaction effect?

mod_tp <- lm(smiles ~ ride_speed * cotton_candy, data = themepark)
summary(mod_tp)

# Q3: How many smiles would I predict at a theme park with:

# ride speed = 20mph, cotton candy = no (0)
mod_tp$coefficients[1]+mod_tp$coefficients[2]*20+mod_tp$coefficients[3]*0 + 0
# or
7.825 + 0.317*20 + 5.321*0 - 0.253*(20*0)

# ride speed = 70mph, cotton candy = yes (1)
mod_tp$coefficients[1]+mod_tp$coefficients[2]*70+mod_tp$coefficients[3]*1 + mod_tp$coefficients[4]*(70*1)
# or
7.825 + 0.317*70 + 5.321*1 -.253*(70*1)

# Q4: Should a theme park with an average ride speed
# of 40 mph sell cotton candy?

# At what ride speed is the interaction effect
# larger than the cotton candy effect?

# 5.321 = .253*x 
5.321/.253 # at 21 mph

newdata1 = data.frame(ride_speed = c(1:100), cotton_candy = rep(1,100))
plot(predict(mod_tp, newdata = newdata1),
     pch = 16,
     col = "darkgreen",
     xlim = c(0,100),
     ylim =c(0,50),
     xlab = "Average ride speed (mph)",
     ylab = "# of smiles")
legend("topleft", legend=c("yes", "no"),  
       fill = c("darkgreen","red"), title = "Cotton Candy") 

newdata2 = data.frame(ride_speed = c(1:100), cotton_candy = rep(0,100))
points(predict(mod_tp, newdata = newdata2),pch = 16, col = "red")