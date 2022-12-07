################################################################################
##### ----------------------- R script template -------------------------- #####
################################################################################
## ------ CLEAN THE WORK ENVIRONMENT ------
rm(list=ls())


## ------ IMPORT REQUIRED LIBRARIES ------
library(stringr)
library(abind)
library(R.utils)
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)

library(tidyverse)



## ------ SET REQUIRED WORKING DIRECTORIES ------
generalWD <- getwd()
dataWD <- file.path(generalWD, "Data")
figureWD <- file.path(generalWD, "Figures")
analysisWD <- file.path(generalWD, "Analysis")



## -----------------------------------------------------------------------------
## ------ I. LOAD RAW DATA ------
read.csv()
read.csv2()
read_xlsx()



## -----------------------------------------------------------------------------
## ------ II. PROCESS DATA ------
filter(iris, Sepal.Length > 7) %>%     ## Extract rows that meet logical criteria
  summarise(., avg = mean(Petal.Width))## Summarize based on a given function



## -----------------------------------------------------------------------------
## ------ III. VISUALIZE DATA ------
## ------   1. plot multiple lines for repeated measures ----
## e.g. plot concentration levels in water in different days for different sites
df <- economics %>%
  select(date, psavert, uempmed) %>%
  gather(key = "variable", value = "value", -date)
head(df)

# Visualization
ggplot(df, aes(x = date, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue"))

# pdf( file = file.path(figureWD, "Example.pdf"), width = 20, height = 12)
a <- ggplot(economics, aes(date, unemploy))
a + geom_ribbon(aes(ymin = unemploy - 900,
                    ymax = unemploy + 900)) +
  geom_path(lineend = "butt",
            linejoin = "round",
            linemitre = 1,
            col = "red") 
# graphics.off()




## ------   2. multiple boxplots in ggplot ----
f <- ggplot(mpg, aes(model, hwy))
f + geom_boxplot()

t <- ggplot(mpg, aes(cty, hwy)) + geom_point()

#facet into columns based on fl
t + facet_grid(cols = vars(fl))
#facet into rows based on years
t + facet_grid(rows = vars(year))

## -----------------------------------------------------------------------------
## ------ IV. ANALYSIS ------- 
## -----------------------------------------------------------------------------
## ------ V. PROCESS OUTPUTS ------
##------------------------------------------------------------------------------
## ----- Reshape a dataframe to long format -----
test <- cbind.data.frame(lokalitet = c(rep("lok1",10),
                       rep("lok2",10)),
                       bark = rep(1:10,2),
              species_A = rbinom(20,1,0.9),
              species_B = rbinom(20,1,0.9),
              species_C = rbinom(20,1,0.1))
test

colIndex <- c(3,4,5)
newData <- do.call(rbind,
                   lapply(colIndex, function(x){
                     cbind.data.frame(lokalitet = test$lokalitet,
                                      bark = test$bark,
                                      arter = names(test)[x],
                                      presence = test[ ,x])
                   }))

newData

sumSpecies <- group_by(newData, lokalitet) %>%
              summarise(n = length(unique(arter[presence > 0])))

barplot( sumSpecies$n,
         names.arg = sumSpecies$lokalitet)

group_by(newData, lokalitet, bark) %>%
  summarise(n = length(unique(arter[presence > 0])))


