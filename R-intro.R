
# __          __  _                            _          _____  
# \ \        / / | |                          | |        |  __ \ 
#  \ \  /\  / /__| | ___ ___  _ __ ___   ___  | |_ ___   | |__) |
#   \ \/  \/ / _ \ |/ __/ _ \| '_ ` _ \ / _ \ | __/ _ \  |  _  / 
#    \  /\  /  __/ | (_| (_) | | | | | |  __/ | || (_) | | | \ \ 
#     \/  \/ \___|_|\___\___/|_| |_| |_|\___|  \__\___/  |_|  \_\
                                                                

# Importing Data ----------------------------------------------------------

nerdy.data <-
  read.delim("C:/Users/neilb/OneDrive/Documents/school/OU/CGSA/R brown bag workshop/nerdy_personality_attributes_scale.csv", header = FALSE)

write.csv(nerdy.data, file = "C:/Users/neilb/OneDrive/Documents/school/OU/CGSA/R brown bag workshop/nerdy_personality_attributes_scale_resaved.csv", row.names = FALSE)

nerd.data <-
  read.csv("C:/Users/neilb/OneDrive/Documents/school/OU/CGSA/R brown bag workshop/nerdy_personality_attributes_scale_resaved2.csv")

str(nerd.data)

library(pastecs)

stat.desc(nerd.data$age) %>% 
  round(digits = 3)

hist(nerd.data$VCL9)

# Nerd Factoring ----------------------------------------------------------

nerd.data %>%
  select(Q1:Q26) %>% 
  fa.parallel(fm = 'pa', quant = .95)

library(psych)
library(GPArotation)

nerd.factors <-
  nerd.data %>% 
  select(Q1:Q26) %>% 
  fa(nfactors = 5, rotate = "oblimin", residuals = TRUE, fm = "pa")

nerd.factors$e.values %>% 
  round(digits = 3)

nerd.factors$communality

nerd.factors$loadings



# Basic Analysis ----------------------------------------------------------

nerdiness <-
  nerd.data %>% 
  select(Q1:Q26) %>% 
  rowMeans()

nerd.data %>% 
  select(Q1:Q26) %>% 
  str()

str(nerdiness)

names(nerd.data)

lm(nerdiness ~ education, data = nerd.data) %>% 
  summary()

lm(nerdiness ~ ASD, data = nerd.data) %>% 
  summary()

lm(nerdiness ~ TIPI6, data = nerd.data) %>% 
  summary()

# Installing packages -----------------------------------------------------




# Data Analysis -----------------------------------------------------------


