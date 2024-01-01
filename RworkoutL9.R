# Question #1: What will "df" look like? 
 
height_inch = rnorm(100,64,2.2)
height_cm = 2.54*height_inch
measure = sample(c("metric","imperial"),100,replace = T)
df <- data.frame(height = 
                   ifelse(measure == "imperial", height_inch, height_cm)
                 )
# or

df <- data.frame(measure = measure,
                 height_cm = height_cm,
                 height_inch, height_inch)

df$height[measure == "imperial"] <- df$height_inch[measure == "imperial"] 
df$height[measure == "metric"] <- df$height_cm[measure == "metric"]
df <- df["height"]

# Question #2: What should you not like about "df"?

# Heights are given in different units.

# Question #3: How can you fix it?

df$height <- ifelse(measure == "imperial", df$height*2.54, df$height)

colnames(df) <- "height_cm"
womens_heights <- df

# or if you want to be more verbose:
# colnames(df)[colnames(df) == "height"]<- "height in cm"
