
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

# c() means "combine into one vector (like a column of data in SPSS)"

practice.vector <- c(1,2,2,3,5,8,10)

sum(practice.vector)
mean(practice.vector)
median(practice.vector)
sd(practice.vector)
min(practice.vector)
max(practice.vector)

mean(practice.vector) + sd(practice.vector)

# Importing Data ----------------------------------------------------------

# If you want to make it easier to save and load data, you should create a project first.
# However, we can skip this step for now.

# Before you load a CSV into R, you need to delete the extraneous 2nd and 3rd
# rows via Excel or some other CSV reader. Otherwise, R will see character data 
# in each column and read them all as character data.

# Set the working directory.
setwd("[the file path for the folder you're working in]")
# Note: To get a full file path, click a file in the folder and select "Copy as path." Then delete the file name.
# Also, in R, you'll need to make all backslashes \ into forward slashes /.

# Import a file as a dataframe
nerd.data <-
  read.csv("Nerdy Personality Attributes Scale data.csv")

# Or trigger a graphic prompt to choose your file
nerd.data <-
  read.csv(file.choose())
# However, doing this makes it harder to fully replicate the code later.

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
t.test(DV ~ IV,
       data = nerd.data)

t.test(nerd01 ~ vocab1,
       data = nerd.data)

# Save your data to a .csv
write.csv(nerd.data, "nerd_data.csv", row.names = FALSE)

# Save your data to a .sav (for SPSS)
install.packages("haven")
library(haven)
write_sav(nerd.data, "nerd_data.sav")

# Somewhat More Advanced Functions ----------------------------------------

stat.desc(nerd.data$nerd01)

# Use pipe operators %>% to make strings of operations more readable
install.packages("dplyr")
library(dplyr)

nerd.data$nerd01 %>% 
  stat.desc()

# Select a set of columns from a data frame
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
lm(DV ~ IV, data = data.frame) %>% 
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

# Determine if, for a given SONA ID, at least 7 attention checks (out of 8)
# are valid
with(data.frame[data.frame$sona.id == 64123 & !is.na(data.frame$sona.id), ], 
     rowSums(cbind(
       Empathy_9 == 2,
       Stability_9 == 5,
       Information_4 == 4,
       Problem_5 == 7,
       Metacognitive.CQ_5 == 7,
       Motivational.CQ_6 == 4,
       Enjoyment_4 == 2,
       Attentiveness_4 == 7
     )) >= 7)

# Check the responses to attention checks for a given SONA ID
data.frame[data.frame$sona.id == "64111" & !is.na(data.frame$sona.id),
               c("Empathy_9", "Stability_9", "Information_4",
                 "Problem_5", "Metacognitive.CQ_5",
                 "Motivational.CQ_6","Enjoyment_4",
                 "Attentiveness_4")]


# For rows where a value is duplicated for a given variable
# (such as SONA or Prolific IDs), display the given columns.
data.frame[duplicated(data.frame$prolific.id) | duplicated(data.frame$prolific.id, fromLast = TRUE),
         c('prolific.id','trust.partic_9',
           'norms.descript.covid_4','age')]

# Logical expressions ------------------------------------------------------------
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

# Descriptive statistics ---------------------------------------------------------

# Excluding rows 1 through 8,
# output descriptive statistics for duration in minutes
# (rounded to three digits)
data.frame[-c(1:8), ] %>%
  select(Duration..in.seconds.) %>% 
  `/`(60) %>% 
  stat.desc() %>% 
  round(digits = 3)

# Display descriptive statistics for a given set of variables
nerd.data[nerd.data$nerd26 == 5,] %>% 
  select(nerd01:nerd25) %>% 
  stat.desc(basic = F, norm = T) %>% # Don't show basic stats, but do show normality
  round(digits = 3) # Round to the third decimal place


# Checking normality --------------------------------------------------------------

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

# Exploratory factor analysis -------------------------------------------------------------

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

