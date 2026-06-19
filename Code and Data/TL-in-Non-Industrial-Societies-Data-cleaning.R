library(tidyverse)

geo <- read.csv("../Code and Data/geography.csv") #this has elevation, latitude, and longitude 
codes <- read.csv("../Code and Data/tightness_codes.csv") #this is the file that contains our tightness codes
tl1 <- read.csv("../Code and Data/overall_code.csv") #this file has the overall tightness scores
meta <- read.csv("../Code and Data/sccs_meta.csv") #this file has a bunch of sccs meta-data
resid <- read.csv("../Code and Data/social_organization.csv") #this file has the patri- and matri-locality variables
subsistence<-read.csv("../Code and Data/subsistence.csv")