## ----setup, echo = FALSE-------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '~/Dropbox/Workshops/IntrotoRII/')


## ----knit_ex, eval = FALSE-----------------------------------------------------------------------------------------------
## data(cars)
## summary(cars)


## ----pressure, echo=FALSE, fig.align = 'center', fig.cap = "For example, this figure was created with `echo = FALSE` and `eval = TRUE`."----
plot(pressure)


## ----eval = F------------------------------------------------------------------------------------------------------------
## install.packages('knitr')
## library(knitr)
## purl('IntroToRII_20231024.Rmd')


## ----eval = F------------------------------------------------------------------------------------------------------------
## getwd()
## setwd("~/Dropbox/Workshops/IntrotoRII/")


## ----eval = F------------------------------------------------------------------------------------------------------------
## knitr::opts_knit$set(root.dir = "~/Dropbox/Workshops/IntrotoRII/")


## ----read_csv------------------------------------------------------------------------------------------------------------
?read.csv
pets <- read.csv("Seattle_Pet_Licenses.csv")
write.csv(pets, file = 'Seattle_Pets_copy.csv',
          row.names = FALSE) 


## ----save_rda------------------------------------------------------------------------------------------------------------
pets_copy <- pets
names(pets_copy)

## Remove periods from names
## grepl() let's you look for a string pattern in a vector of strings
## Try it out!
# three_names <- c("Jessica", "Lizzy", "Breon")
# grepl("e", three_names)
# grep("e", three_names)
# three_names[grepl("e", three_names)]
## The \\ is called an "escape", so that R recognizes the period
names(pets_copy) <- gsub("\\.", "", names(pets_copy))
save(pets_copy, file = 'SeattlePets.rda') 
rm(pets_copy) 


## ----load_rda------------------------------------------------------------------------------------------------------------
load('SeattlePets.rda') 


## ----xlsx----------------------------------------------------------------------------------------------------------------
# install.packages(c("readxl", "writexl"))
library(readxl)
library(writexl)

write_xlsx(pets_copy, path = "SeattlePets.xlsx")
rm(pets_copy)
pets_copy <- read_xlsx("SeattlePets.xlsx")


## ----read_haven----------------------------------------------------------------------------------------------------------
## If you have not installed the `haven` pkg use the following
# install.packages("haven")
library(haven)
write_dta(pets_copy, path = "SeattlePets.dta") #Save as a stata data frame
rm(pets_copy)

pets_copy <- read_dta(file = "SeattlePets.dta")


## ------------------------------------------------------------------------------------------------------------------------
is.data.frame(pets)  
is.data.frame(pets_copy)
rm(pets_copy)

## Tidyverse packages for data cleaning & manipulation
# install.packages(c("tidyr", "dplyr"))
library(tidyr)
library(dplyr)
pets_tib <- as_tibble(pets)
is.data.frame(pets_tib)


## ----head----------------------------------------------------------------------------------------------------------------
head(pets)
head(pets_tib)


## ----names---------------------------------------------------------------------------------------------------------------
# VARIABLE NAMES
names(pets)
colnames(pets)


## ----rownames, eval = F--------------------------------------------------------------------------------------------------
## rownames(pets)


## ----dim-----------------------------------------------------------------------------------------------------------------
dim(pets) # this gives rows and then columns (n X p)
nrow(pets)
ncol(pets)
length(pets) # NOT ADVISED TO USE WITH MATRICES OR DATA FRAMES


## ----summary-------------------------------------------------------------------------------------------------------------
summary(pets)
str(pets)


## ----summary_peng--------------------------------------------------------------------------------------------------------
# install.packages("palmerpenguins")
## Load the penguins dataset
library(palmerpenguins)
data(penguins)
summary(penguins)
str(penguins)


## ----echo = T, eval = F--------------------------------------------------------------------------------------------------
## pets$Species
## ## Note what comes before the , specifies ROW selection
## ## what comes after the , specifies COLUMN selection
## pets[1:10 , "Species"]
## 
## ## We can select multiple columns by passing a
## ## vector of column names after the ,
## pets[1:10, c("Species", "Primary.Breed")]


## ----echo = T, eval = F--------------------------------------------------------------------------------------------------
## select(pets, Species)


## ----echo = T, eval = T--------------------------------------------------------------------------------------------------
head(pets)
## table() is a handy base R function
## for counting the frequency of unique values in a vector
## if you do not specify the useNA arguement, it will not count NAs
table(pets$Species, useNA = "ifany")

cat_base <- pets[pets$Species == "Cat", ]
dim(cat_base)
head(cat_base)


## ----echo = TRUE, eval = FALSE-------------------------------------------------------------------------------------------
## pets$Species == "Cat


## ----echo = T, eval = TRUE-----------------------------------------------------------------------------------------------
cat_tidy <- filter(pets, Species == "Cat")
dim(cat_tidy)
head(cat_tidy)


## ----echo = T, eval = TRUE-----------------------------------------------------------------------------------------------
cat_tidy_pipe <- pets %>% filter(Species == "Cat" )
dim(cat_tidy_pipe)
head(cat_tidy_pipe)


## ----echo = T, eval = F--------------------------------------------------------------------------------------------------
## pets %>% filter(Species == "Cat" ) %>%
##   select(Primary.Breed)
## 
## ## Typically you'll want to assign the output
## ## of a pipe to a new object
## cat_breeds <- pets %>% filter(Species == "Cat" ) %>%
##   select(Primary.Breed)


## ----factor_base, eval = TRUE, echo = TRUE-------------------------------------------------------------------------------
summary(pets)
class(pets$Species)

## create a copy to modify
pets_base <- pets

?factor
pets_base$Species <- factor(pets_base$Species,
                            levels = c("Cat", "Dog", "Goat", "Pig"))
summary(pets_base)


## ----factor_tidy---------------------------------------------------------------------------------------------------------
pets_tidy <- pets %>% 
  mutate(Species = factor(Species, levels = c("Cat", "Dog", "Goat", "Pig")),
         Primary.Breed = as.factor(Primary.Breed))

summary(pets_tidy)
## How many unique breeds are there?
# length(unique(pets$Primary.Breed))


## ----echo = T, eval = T--------------------------------------------------------------------------------------------------
## Make a copy of the dataset we will manipulate 
penguins_base <- penguins
head(penguins_base)

## Create new column with body mass in kg 
penguins_base$body_mass_kg <- penguins_base$body_mass_g/1000
head(penguins_base)


## ----echo = T, eval = F--------------------------------------------------------------------------------------------------
## # dplyr
## penguins_tidy <- mutate(penguins, body_mass_kg = body_mass_g/1000)
## head(penguins_tidy)
## 
## penguins_tidy_pipe <- penguins %>%
##   mutate(body_mass_kg = body_mass/1000)


## ----sumfns--------------------------------------------------------------------------------------------------------------
## What is the output of 
# min(penguins$flipper_length_mm)
min(penguins$flipper_length_mm, na.rm = TRUE)
max(penguins$flipper_length_mm, na.rm = TRUE)
mean(penguins$flipper_length_mm, na.rm = TRUE)
sd(penguins$flipper_length_mm, na.rm = TRUE)
var(penguins$flipper_length_mm, na.rm = TRUE)
sqrt(var(penguins$flipper_length_mm, na.rm = TRUE)) # same as the sd
median(penguins$flipper_length_mm, na.rm = TRUE)

quantile(penguins$flipper_length_mm, probs = 0.5, na.rm = TRUE)
quantile(penguins$flipper_length_mm, probs = 0.25, na.rm = TRUE)
quantile(penguins$flipper_length_mm, probs = 0.75, na.rm = TRUE)
quantile(penguins$flipper_length_mm, probs = c(0.25,0.5,0.75), na.rm = TRUE)


## ----echo = T, eval = T--------------------------------------------------------------------------------------------------
peng_sum_base <- data.frame(flip_length_mean = mean(penguins$flipper_length_mm,
                                                    na.rm = TRUE),
                            flip_length_sd = sd(penguins$flipper_length_mm,
                                                na.rm = TRUE))
head(peng_sum_base)

## The round() function can help summaries look pretty!
round(peng_sum_base, digits = 2)


## ----peng_sum_agg--------------------------------------------------------------------------------------------------------

## Column names might have to be changed afterwards
## lefthand side of formula is the variable you want to make summaries of
## right hand side is the groups... use 1 for entire dataset
peng_sum_agg <- aggregate(flipper_length_mm ~ 1, 
                          data = penguins,
                          FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                              sd = sd(x, na.rm = TRUE)))
peng_sum_agg


## ----peng_sum_tidy, echo = T ,eval =F------------------------------------------------------------------------------------
## peng_sum_tidy <- penguins %>%
##   summarize(flip_length_mean = mean(flipper_length_mm, na.rm = TRUE),
##             flip_length_sd = sd(flipper_length_mm, na.rm = TRUE))
## 
## peng_sum_tidy


## ----sum_across----------------------------------------------------------------------------------------------------------
penguins %>% 
  summarize(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))


## ----echo = T, eval = T--------------------------------------------------------------------------------------------------

peng_sumby_agg <- aggregate(cbind(flipper_length_mm, body_mass_g) ~
                              sex + species, 
          data = penguins, 
          FUN = function(x){
            c(mean = round(mean(x, na.rm = TRUE), digits = 2),
              sd = round(sd(x, na.rm = TRUE), digits = 2))
          })
peng_sumby_agg


## ----agg_tot-------------------------------------------------------------------------------------------------------------
penguins$total <- 1
peng_agg_tot <- aggregate(total ~ sex + species, 
          data = penguins, 
          FUN = sum)
peng_agg_tot

## Check our work
## Notice the aggregate didn't include the 11 penguins
## for which sex is missing
table(penguins$sex, penguins$species, useNA = "ifany")


## ----group_by, echo = T, eval = F----------------------------------------------------------------------------------------
## peng_sum_tidy <- penguins %>%
##   group_by(species, sex) %>%
##   summarize(total = n(),
##             flip_length_mean = mean(flipper_length_mm, na.rm = TRUE),
##             flip_length_sd = sd(flipper_length_mm, na.rm = TRUE),
##             body_mass_mean = mean(body_mass_g, na.rm = TRUE),
##             body_mass_sd = sd(body_mass_g, na.rm = TRUE)) %>%
##   ungroup()
## 
## peng_sum_tidy


## ----peng_spec_isl-------------------------------------------------------------------------------------------------------
peng_by_island <- penguins %>% 
  group_by(island, year) %>%
  summarize(n = n())

head(peng_by_island)  


## ----peng_spec_isl_wide--------------------------------------------------------------------------------------------------
peng_island_wide <- peng_by_island %>%
  pivot_wider(id_cols = "island",
              names_from = "year",
              values_from = "n")

peng_island_wide


## ----penc_spec_isl_long--------------------------------------------------------------------------------------------------
peng_island_long <- peng_island_wide %>% 
  pivot_longer(cols = contains("200"),
               names_to = "year",
               values_to = "n")

peng_island_long

