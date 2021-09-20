#Task 3 - Applied Empirical Economics I
#Qinglin Ouyang
#qinglin.ouyang@sbs.su.se
#Sep 16, 2021

library(haven)
library(dplyr)
library(tidyverse)
library(AER)
library(stargazer)
library(ggplot2)

rm(list = ls())

rootdir <- "/Users/qiou3954/Library/Mobile Documents/com~apple~CloudDocs/Year 2/Applied Empirical Economics I/Qinglin_Ouyang/Task_3"
setwd(rootdir)
# Prepare for Log folder
system('rmdir ./Log /s /q') 
dir.create("./Log")

# Prepare for building
system('rmdir ./Build/Input')
dir.create("./Build/Input")
file.copy("./Raw/NEW7080.dta", "./Build/Input/NEW7080.dta")

# Rename variables
dir.create("./Analysis/Input")
dir.create("./Analysis/Output")
source("./Build/Code/DataClean.R") #data clean process

# Table reproduction
source("./Analysis/Code/Table_rep.R")

# Figure V reproduction
source("./Analysis/Code/FigureV_rep.R")
