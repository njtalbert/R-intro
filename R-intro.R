
# __          __  _                            _          _____  
# \ \        / / | |                          | |        |  __ \ 
#  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | |__) |
#   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  |  _  / 
#    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | | \ \ 
#     \/  \/ \___|_|\___\___/|_| |_| |_|\___|  \__\___/  |_|  \_\


# Functions and Objects ---------------------------------------------------

# R works by applying functions to objects

# Use the arrow function to create an object called 'A' with a value of '1'
A <- 1

# Likewise for 'B'
B <- 2

# Use the '+' function to add the values of the objects
A+B

# Or use the 'sum' function to add the values
sum(A, B)

# Note: Normally, if you want to apply a function to multiple objects,
# you'll probably need to use a concatenate 'c()' or sapply function.

# Importing Data ----------------------------------------------------------

# Before you load a CSV into R, you need to delete the extraneous 2nd and 3rd
# rows via Excel or some other CSV reader. Otherwise, R will see character data 
# in each column and read them all as character data.

# Import a file as a dataframe
# /// Make sure to use the correct slashes ///
nerd.data <-
  read.csv("[your file path]/Nerdy Personality Attributes Scale data.csv")

nerd.data <-
  read.csv("C:/Users/neilb/OneDrive/Documents/school/OU/CGSA/R brown bag workshop/Nerdy Personality Attributes Scale data.csv")

# Or trigger a graphic prompt to choose your file
nerd.data <-
  read.csv(file.choose())

# Basic Functions ---------------------------------------------------------

# Get the column (variable) names in a dataframe
names(nerd.data)

# Get the general information (structure) of your data
str()

# Get a specific column (variable) from a data from
dataframe$column

# Find how many values are missing in a columns
sum(is.na())

# Get descriptive statistics on a column (variable)
install.packages("pastecs")
library(pastecs)
stat.desc()

# Do a t.test
t.test(nerd01 ~ vocab1,
       data = nerd.data)

# Save your data to a .csv
write.csv(nerd.data, "filepath/nerd_data.csv", row.names = FALSE)

# Save your data to a .sav (for SPSS)
install.packages("haven")
library(haven)
write_sav(nerd.data, "filepath/nerd_data.sav")

# Somewhat More Advanced Functions ----------------------------------------

stat.desc(nerd.data$nerd01)

# Use pipes to make strings of operations more readable
nerd.data$nerd01 %>% 
  stat.desc()

# Select a set of columns from a data frame
install.packages("dplyr")
library(dplyr)
df %>% 
  select(a:d) %>% # or select(a, b, c, d) %>% 
  stat.desc()

# Create a mean compositive variable
pers.agr <-
  nerd.data %>% 
  select(pers.agr.1, pers.agr.2) %>% 
  rowMeans()

pers.ext <-
  nerd.data %>% 
  select(pers.ext.1, pers.ext.2) %>% 
  rowMeans()

# Bind vector (variable) objects to a new data frame
nerd.means <-
  cbind(pers.agr, pers.ext) %>% 
  data.frame()

# Check out the new data frame
str(nerd.means)

# In a data frame for the 8th row of data,
# display the value for a column named 'pers.agr'  
nerd.means[8,'pers.agr']

# Display for the rows 5 to 10 
# the value for columns named 'age' and 'gender'
nerd.means[c(5:10),c('pers.agr','pers.ext')]

# Run a regression
lm(cdc.msg.att ~ trust.gen, data = trustvax.fac.cdc) %>% 
  summary

# Run an ANOVA
install.packages("car")
library(car)
lm(nerd01~married*hand, data = nerd.data) %>% 
  car::Anova(type=3)

# Apply a function to every column in a dataframe.
sapply(data_frame, FUN = stat.desc)
# Example
sapply(nerd.means, FUN = median, na.rm = TRUE)

# display only those rows of data where 'variable' is equal to 4
df[df$variable == 4,]

# Dislay the first 100 rows for the columns specified
# (good for allocating credit on SONA or Prolific)
trustvax.pre[c(1:100),c('prolific.id','Q_RecaptchaScore','trust.partic_9',
                        'norms.descript.covid_4','age','occupation')]

# For rows where a value is duplicated for a given variable
# (such as SONA or Prolific IDs), display the given columns.
trustvax[duplicated(trustvax$prolific.id) | duplicated(trustvax$prolific.id, fromLast = TRUE),
         c('prolific.id','trust.partic_9',
           'norms.descript.covid_4','age')]

# Logical expressions
1 < 5
1 > 5
5*5 == 25
1 == 1
1 != 2
nerd.means$pers.agr == 4

# Create a 'bad case column' tagging rows
# with invalid attention checks using a logical statement
trustvax$bad.case <-
  as.integer(trustvax$trust.partic_9 != 1 | trustvax$norms.descript.covid_4 !=7)

# Visualize your data 
install.packages("ggplot2")
library(ggplot2)
ggplot(trustvax.fac, aes(x = norms.descript.covid,
                         y = attitude.vacc.covid,
                         col = trust.gen.categ)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# Excluding rows 1 through 8,
# output descriptive statistics for duration in minutes
# (rounded to three digits)
trustvax.pre[-c(1:8), ] %>%
  select(Duration..in.seconds.) %>% 
  `/`(60) %>% 
  stat.desc() %>% 
  round(digits = 3)

# Display descriptive statistics for a given set of variables
nerd.data[nerd.data$nerd26 == 5,] %>% 
  select(nerd01:nerd25) %>% 
  stat.desc(basic = F, norm = T) %>% # Don't show basic stats, but do show normality
  round(digits = 3) # Round to the third decimal place

# See of the mean skewness of a set of variables (including only rows of data
# where the bad.case column is equal to 0) improves
# with a square transformation
nerd.data[nerd.data$nerd26 == 5,] %>% 
  select(nerd01:nerd25) %>% 
  `^`(2) %>% 
  sapply(skew) %>% # sapply applies a function (skew) to a set of columns
  abs() %>% 
  mean() %>% 
  round(digits = 3)

# EFA ---------------------------------------------------------------------

library(psych)
library(GPArotation)

nerd.data %>%
  select(nerd01:nerd26) %>% 
  fa.parallel(fm = 'pa', quant = .95)

nerd.factors <-
  nerd.data %>% 
  select(nerd01:nerd26) %>% 
  fa(nfactors = 5, rotate = "oblimin", residuals = TRUE, fm = "pa")

nerd.factors$e.values %>% 
  round(digits = 3)

nerd.factors$communality

nerd.factors$loadings

