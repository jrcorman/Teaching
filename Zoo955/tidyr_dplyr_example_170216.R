# Example data processing using tidyr and dplyr

# Step 1. Load appropriate libraries ####
library(tidyr)
library(dplyr)
library(ggplot2)

# Step 2. Download data from LTER Data Portal ####

# Package ID: knb-lter-ntl.106.6 Cataloging System:https://pasta.lternet.edu.
# Data set title: Historical Birge - Juday Lake Survey - major ions 1900 - 1943.
# Data set creator:  Edward Birge - NTL LTER 
# Data set creator:  Chauncy Juday - NTL LTER 
# Metadata Provider:    - North Temperate Lakes LTER 
# Contact:    - LNO Information Manager LTER Network Office  - 
# Contact:    - NTL LTER Information Manager University of Wisconsin  - infomgr@lter.limnology.wisc.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-ntl.106.6
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@lternet.edu 

infile2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/106/6/930401365d17f8fb01296fe3f550d109" 
infile2 <- sub("^https","http",infile2) 
dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "bj_code",     
                 "lakename",     
                 "year4",     
                 "sampledate",     
                 "obs",     
                 "laketype",     
                 "z",     
                 "tc",     
                 "sd",     
                 "col",     
                 "con",     
                 "ph",     
                 "do",     
                 "alk",     
                 "co2",     
                 "nh3_n",     
                 "org_n",     
                 "no3_n",     
                 "no2_n",     
                 "sol_p",     
                 "org_p",     
                 "tp",     
                 "cl",     
                 "so4",     
                 "si",     
                 "ca",     
                 "mg",     
                 "fe",     
                 "plank",     
                 "res",     
                 "mn",     
                 "fe3",     
                 "fe2",     
                 "cu",     
                 "al",     
                 "chla",     
                 "h2s",     
                 "po4",     
                 "bor",     
                 "air_temp",     
                 "sample_time",     
                 "eh",     
                 "notes",     
                 "state"    ), check.names=TRUE)

# attempting to convert dt2$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
dt2$sampledate<-as.Date(dt2$sampledate,format=tmpDateFormat)
rm(tmpDateFormat) 
str(dt2)

# Step 2. 

# Step 3. Analyze data

# Calculate mean and standard deviation of selected elements and ratios 
# for each lake across all sampling dates

dt2 %>%
  subset(z < 5) %>% # select samples collected < 5 m from surface
  select(lakename, sampledate, so4, ca, co2, org_n, org_p) %>%
  mutate(org_NP = (org_n/org_p)*(30.97/14.01)) %>% # calculate molar N:P ratio
  gather("chemical", "value", 3:8) %>%
  group_by(lakename, chemical) %>%
  summarize(avg = mean(value, na.rm=TRUE), 
            std=sd(value, na.rm=TRUE)) %>%
  ggplot(aes(x=lakename, y=avg)) +
    geom_errorbar(aes(ymin=avg-std, ymax=avg+std)) +
    geom_point(aes(color=lakename)) +
    facet_wrap(~chemical, scales="free_y") +
    theme(legend.position = "none")
