## Script to load EPA air quality data and generate an exploratory plot to answer
## this question:
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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

# Use the ei.sector field in the SCC data to subset entries that are motor vehicles
# from both the NEI and SCC dataframes. EPA supporting documentation lists the 
# EIS sectors that are relevant to this analysis in section 4.6
# https://www.epa.gov/sites/production/files/2015-07/documents/2008_neiv3_tsd_draft.pdf
mvidx <- grepl("[Oo]n(.*)[Rr]oad(.*)[Vv]ehicle",SCC$ei.sector)
SCC <- transform(SCC,scc=as.character(scc))
mvsources <- select(SCC,1:10,-(data.category))[mvidx,]
mvdata <- subset(NEI,scc %in% mvsources$scc & fips=="24510")

# Merge the subsetted NEI and SCC data into one dataframe
mvdata <- merge(x=mvdata,y=mvsources,by="scc")

# Group and summarise data to enable multi-panel plot with ggplot
mvdata_grp1 <- group_by(mvdata,year,ei.sector)
mvdata_grp1 <- summarise(mvdata_grp1, emitsum = sum(emissions))
facetnames <- c("Mobile - On-Road Diesel Heavy Duty Vehicles"="Diesel Heavy Duty",
                "Mobile - On-Road Diesel Light Duty Vehicles"="Diesel Light Duty",
                "Mobile - On-Road Gasoline Heavy Duty Vehicles"="Gasoline Heavy Duty",
                "Mobile - On-Road Gasoline Light Duty Vehicles"="Gasoline Light Duty")

g <- ggplot(mvdata_grp1,aes(year,log10(emitsum)))
plot(g+geom_line()+facet_grid(.~ei.sector,labeller=as_labeller(facetnames))+labs(x = "Year",
        y = "Log10 of Annual Emissions (Tons)",title="Baltimore City Motor Vehicle Emissions")
     + theme(plot.title = element_text(hjust = 0.5)))

# Send the same plot to PNG file for storage
png(filename = "Plot5.png",width=680)
plot(g+geom_line()+facet_grid(.~ei.sector,labeller=as_labeller(facetnames))+labs(x = "Year",
        y = "Log10 of Annual Emissions (Tons)",title="Baltimore City Motor Vehicle Emissions")
     + theme(plot.title = element_text(hjust = 0.5)))
dev.off()