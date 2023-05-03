
PM25 <- readRDS("~/summarySCC_PM25.rds")

Class_Code <- readRDS("~/Source_Classification_Code.rds")

library(tidyverse)
library(ggplot2)

### Question 1 ####
# Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing 
# the total PM2.5 emission from all sources for each of the years 
# 1999, 2002, 2005, and 2008.

# data
Q1_0 <- PM25

# Select only Emissions & year
Q1_1 <- Q1_0 %>% select('Emissions','year')

# sum emissions by year
Q1_2 <- tapply(Q1_1$Emissions, Q1_1$year, sum) 

# png
png("Plot1.png",width=480,height=480)

# barpolot
barplot(Q1_2,
        xlab = "Years",
        ylab = "Emissions",
        main = "Total emissions PM2.5")

dev.off()


### Question 2 ###
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base 
# plotting system to make a plot answering this question.

# data
Q2_0 <- PM25

# subset 
Q2_1 <- subset(Q2_0, Q2_0$fips == "24510")

# sum emissions by year
Q2_2 <- tapply(Q2_1$Emissions, Q2_1$year, sum) 

# png
png("Plot2.png",width=480,height=480)

# barpolot
barplot(Q2_2,
        xlab = "Years",
        ylab = "Emissions",
        main = "Total emissions PM2.5 (Baltimore City)")

dev.off()

### Question 3 ###
# Of the four types of sources indicated by the type (point, 
# nonpoint, onroad, nonroad) variable, which of these four 
# sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions 
# from 1999–2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

# library ggplot2
library(ggplot2)

# data
Q3_0 <- PM25

# subset 
Q3_1 <- subset(Q2_0, Q2_0$fips == "24510")

# sum emissions by year
Q3_2 <- aggregate(Emissions ~ year + type, Q3_1, sum)

# png
png("Plot3.png",width=480,height=480)

ggplot(Q3_2) +
  geom_line(aes(x = year,
                y =  Emissions,
                col = type)) +
  labs(title = "Emissions from 1999–2008 for Baltimore City")+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

### Question 4 ###
# Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?

# data
Q4_0 <- PM25
Q4_1 <- Class_Code

# Extract only "coal"
Q4_1_coal <- subset(Q4_1, Q4_1$EI.Sector == "Fuel Comb - Electric Generation - Coal")
Q4_2_coal <- merge(Q4_0,Q4_1_coal,by="SCC")

# sum emissions by year
Q4_3 <- tapply(Q4_2_coal$Emissions, Q4_2_coal$year, sum) 
Q4_4 <- as.data.frame(Q4_3)
names(Q4_4)[1] <- "Emissions"
rownames(Q4_4) <- c(1:4)
Q4_4$Year <- c(1999, 2002, 2005, 2008)


# png
png("Plot4.png",width=480,height=480)

ggplot(Q4_4)+
  geom_line(aes(x = Year,
                y = Emissions)) +
  labs(title = "emissions from coal 1999–2008")+
  theme(plot.title = element_text(hjust = 0.5))


dev.off()

### Question 5 ###
# How have emissions from motor vehicle sources 
# changed from 1999–2008 in Baltimore City?

# data
Q5_0 <- PM25
Q5_1 <- Class_Code
Q5_2_vehicle <- merge(Q5_0,Q5_1,by="SCC")

# subset Baltimore 
Q5_3_Baltimore <- subset(Q5_2_vehicle, 
               Q5_2_vehicle$fips == "24510")

# Subset car
Q5_4_car <- subset(Q5_3_Baltimore, 
                   Q5_3_Baltimore$type == "ON-ROAD")



# sum emissions by year
Q5_5 <- tapply(Q5_4_car$Emissions, Q5_4_car$year, sum) 
Q5_6 <- as.data.frame(Q5_5)
names(Q5_6)[1] <- "Emissions"
rownames(Q5_6) <- c(1:4)
Q5_6$Year <- c(1999, 2002, 2005, 2008)

# png
png("Plot5.png",width=480,height=480)

ggplot(Q5_6) + 
  geom_line(aes(x = Year,
                y = Emissions))+
  labs(title = "Emissions from motor vehicle sources 1999–2008 in Baltimore City")+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()
