# Clear 
rm(list = ls()) 

# Libraries
library(dplyr)
library(tidyverse)
library(maps)
library(ggplot2)
library(tseries)
library(Hmisc)
library(httr)
library(stargazer)
options(stringsAsFactors = FALSE)
options(Encoding="UTF-8")

# Set Working directory and read CSVs
# The dataset is constantly updated, which is core to the analysis. As such, I have setup R to read the newest version.
# The initial analysis was prepared on 3-27-2019 and uses that dataset, which is included. 

setwd("C:/Users/Dom/Desktop/Applied Regression")
#URL <- read_csv(text=GET("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"), header=T)
#COVID <- read_csv(content(URL, "text"))
COVID <- read.csv("C:/Users/Dom/Desktop/Applied Regression/NYTCOVID19.txt") # Legacy Version
Elec2016 <- read.csv("C:/Users/Dom/Desktop/Applied Regression/2016Elect.txt")
POP <- read.csv("C:/Users/Dom/Desktop/Applied Regression/POP.csv")
Csize <- read.csv("C:/Users/Dom/Desktop/Applied Regression/Csize.csv")

# Clean Data Sets to add 2019 population and 2016 Election County Results
# The original datasets have a lot of information we do not care about, this removes all of that before merging.

Elec2016$Democrat <- ifelse(Elec2016$lead == "Hillary Clinton", 1, 0)
Elec2016$Remove <- ifelse(Elec2016$cand == "Hillary Clinton" | Elec2016$cand == "Donald Trump" , 1, 0)
Elec2016 <- Elec2016[!(Elec2016$Remove=="0"),]
Elec2016 <- Elec2016[!(Elec2016$cand=="Hillary Clinton" & Elec2016$Democrat=="0"),]
Elec2016 <- Elec2016[!(Elec2016$cand=="Donald Trump" & Elec2016$Democrat=="1"),]
E16 <-subset(Elec2016, nchar(as.character(fips)) >= 3)
E16 <- E16[order(E16$county, E16$state.name),]
POP <- POP[order(POP$county, POP$state.name),]

Elec2016 <- merge(E16, POP, by=c("county", "state.name"))

# Merge Data sets
# This is merging our previously constructed datasets into our working dataset, COVID.
COVID <- merge(COVID, Elec2016[ ,c("fips", "Democrat", "POP2019")], by = "fips", 
                   all.x = TRUE)
COVID <- merge(COVID, Csize[ ,c("fips", "LA")], by = "fips", all.x = TRUE)
COVID <- na.omit(COVID)

# Set up Time Series, LogPopulation, Population Density and Subsets
  
COVID$date <- as.Date(COVID$date)
COVID <- COVID[order(COVID$county, COVID$date),]
COVID$LogPOP <- log10(COVID$POP2019)
COVID$CBLP <- COVID$cases/COVID$LogPOP
COVID$Density <- COVID$POP2019/COVID$LA
RC <-subset(COVID, COVID$Democrat=="0")
DC <-subset(COVID, COVID$Democrat=="1")
Current <-subset(COVID, COVID$date=="2020-03-27") #This will be changed later to a dynamic function for the current day.

# Visualize Data to see how to proceed with analysis. The color scheme is maintained for future visuals.

ggplot(data = DC, aes(x = date, y = cases))+ # Democrats
  geom_line(color = "Blue", size = 1)
ggplot(data = RC, aes(x = date, y = cases))+ # Republican
  geom_line(color = "Red", size = 1)

# Set Date Limits

min <- as.Date("2020-03-01")
max <- NA

# Visualize again

ggplot(COVID, aes(x = date, y = cases, color=as.factor(Democrat))) +
         geom_point() +
  scale_x_date(limits = c(min, max), date_labels = "%b/%d")

ggplot(data = COVID, aes(x = date, y = cases)) + 
  geom_area(aes(color = as.factor(Democrat), fill = as.factor(Democrat)), 
            alpha = 0.5, position = position_dodge(0.8), size = 1) +
  scale_color_manual(values = c("Red", "Blue")) +
  scale_fill_manual(values = c("Dark Red", "Dark Blue")) +
  scale_x_date(limits = c(min, max), date_labels = "%b/%d") +
  labs(title = "Number of cases by Political Party: Side-by-side")

LM1 <- lm(cases~Democrat+date+POP2019, data = COVID) # Not useful for anything at the moment

# Section data by LogPOP and Density 
Current$CutLP <- cut2(Current$LogPOP, g=6)
COVID$CutLP <- cut2(COVID$LogPOP, g=6)
Current$CutDE <- cut2(Current$Density, g=6)
COVID$CutDE <- cut2(COVID$Density, g=6)

# Visualize new variables

ggplot(data = COVID, aes(x = CutLP, y = CBLP)) + 
  geom_boxplot(aes(color = as.factor(Democrat)), 
            alpha = 0.5, position = position_dodge(1), size = 1) +
  scale_color_manual(values = c("Red", "Blue")) +
  scale_fill_manual(values = c("Dark Red", "Dark Blue")) +
  labs("Cases by party and LOG Population") +
  xlab("LOG Population") +
  ylab("Cases by LOG Population")

ggplot(data = COVID, aes(x = CutLP)) + 
  geom_bar(aes(color = as.factor(Democrat)), 
            alpha = 0.5, position = position_dodge(1), size = 1) +
  scale_color_manual(values = c("Red", "Blue")) +
  scale_fill_manual(values = c("Dark Red", "Dark Blue")) +
  labs(title = "Count of counties by Party and LOG Population") +
  xlab("Log Population Bins")

ggplot(data = Current, aes(x = CutDE, y = cases)) + 
  geom_boxplot(aes(color = as.factor(Democrat)), 
               alpha = 0.5, position = position_dodge(1), size = 1) +
  scale_color_manual(values = c("Red", "Blue")) +
  scale_fill_manual(values = c("Dark Red", "Dark Blue")) +
  labs(title = "Cases by Party and Population Density") +
  xlab("Population Density")

ggplot(data = Current, aes(x = CutDE)) + 
  geom_bar(aes(color = as.factor(Democrat)), 
           alpha = 0.5, position = position_dodge(1), size = 1) +
  scale_color_manual(values = c("Red", "Blue")) +
  scale_fill_manual(values = c("Dark Red", "Dark Blue")) +
  labs(title = "Number of counties by Party and Population Density") +
  xlab("Population Density Bins")

# Basic Linear Models

LM2 <- lm(cases~Democrat+Density, data = COVID)
LM3 <- lm(deaths~Democrat+LogPOP, data = COVID)
LM4 <- lm(cases~Democrat+Density+Democrat*Density, data = Current)

# Extract LM4 with Stargazer
T1 <- stargazer(LM4, title="Political Party and Population Density Regression Results", 
                report = ('vcstp*'), type="text")
