## Script to load EPA air quality data and generate an exploratory plot to answer
## this question:
## "Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008?"

# Check whether the data files are already availabile in this directory
if(!file.exists("datazip.zip")){
        download.file(url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",destfile="datazip.zip")
        unzip("datazip.zip")
}

if(!exists("NEI") | !exists("SCC")){
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        names(NEI) <- tolower(names(NEI))
        names(SCC) <- tolower(names(SCC))
}

# Load libraries required for this script
library(dplyr)
library(ggplot2)

# Summarise the data for Baltimore city by year and type of source. 
# Type also needs to be a factor for ease of graphing
baltimore <- subset(NEI,fips=="24510")
baltimore <- group_by(baltimore,year,type)
baltimore_sum <- summarise(baltimore, emitsum = sum(emissions))
baltimore_sum <- transform(baltimore_sum,type=as.factor(type))

# Create object with ggplot2 method. Use facets to display side-by-side comparison
# of the different types of emission sources
g <- ggplot(baltimore_sum,aes(year,emitsum))
print(g + geom_point() + facet_grid(.~type) + labs(x="Year",y="Sum of PM2.5 emissions")
      + labs(title="Baltimore Annual Tons PM2.5") + theme(plot.title = element_text(hjust = 0.5)))

# Send the same plot to PNG file for storage
png(filename = "Plot3.png",width=680)
print(g + geom_point() + facet_grid(.~type) + labs(x="Year",y="Sum of PM2.5 emissions")
      + labs(title="Baltimore Annual Tons PM2.5") + theme(plot.title = element_text(hjust = 0.5)))
dev.off()