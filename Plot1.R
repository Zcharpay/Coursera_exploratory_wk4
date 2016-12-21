## Script to load EPA air quality data and generate an exploratory plot to answer
## this question:
## "Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?"

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

# Summarise the data by year
annualtotals <- with(NEI,tapply(emissions,year,sum))

# Create a plot with the base plot system that summarises this data.
# copy the plot to PNG file as well
plot(names(annualtotals),annualtotals,xlab="Year",ylab="National Annual Tons PM25"
     ,type="l",main="National Total Tons PM2.5 Per Year")
dev.copy(png, file = "Plot1.png")
dev.off()