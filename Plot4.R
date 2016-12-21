## Script to load EPA air quality data and generate an exploratory plot to answer
## this question:
## "Across the United States, how have emissions from coal combustion-related 
## sources changed from 1999–2008?"

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

# Use the short.name field in the SCC data to subset entries that include combustion and coal
# from both the NEI and SCC dataframes
coalidx <- grepl("[Cc][Oo][Mm][Bb](.*)[Cc][Oo][Aa][Ll]|[Ii]n-[Pp]rocess(.*)[Cc][Oo][Aa][Ll]|[Cc]harcoal(.*)[Gg]rill|[Mm]arine(.*)[Cc]oal(.*)[Aa]ll [Vv]essel [Tt]ypes",SCC$short.name)
SCC <- transform(SCC,scc=as.character(scc))
coalcombsources <- select(SCC,1:10,-(data.category))[coalidx,]
coalcombdata <- subset(NEI,scc %in% coalcombsources$scc)

# Merge the subsetted NEI and SCC data into one dataframe, and create a new 
# feature (stateid) by extracting the stated code from the start of FIPS
coalcombdata <- merge(x=coalcombdata,y=coalcombsources,by="scc")
coalcombdata <- mutate(coalcombdata,stateid=substr(fips,1,2))
# recreate this factor to reduce the number of levels carried forward, simplifies things later
coalcombdata <- transform(coalcombdata,scc.level.one=as.factor(as.character(scc.level.one)))

# Summarise the data by year
annualtotals <- with(coalcombdata,tapply(emissions,year,sum))
# Group and transform data to enable multi-panel plot with ggplot
coalcombdata_grp1 <- group_by(coalcombdata,year,stateid,type,scc.level.one)
coalcombdata_grp1 <- summarise(coalcombdata_grp1, emitsum = sum(emissions))
coalcombdata_grp1 <- transform(coalcombdata_grp1,type=as.factor(type))
# For boxplots, we need the x variable (year) as a factor
coalcombdata_grp2 <- transform(coalcombdata_grp1,year=as.factor(year))

# Plot to the local graphics device
# boxplot with mean indivator (diamond) allows us to see change in range, median and mean
# of the states for each year, and each of the coal combustion sources
gboxlog <- ggplot(coalcombdata_grp2,aes(year,log10(emitsum)))
plot(gboxlog+geom_boxplot()+facet_grid(.~scc.level.one)+labs(x = "Year",
        y = "Log10 of Annual Emissions (Tons)",title="State Emissions from Coal Combustion Sources")
        + theme(plot.title = element_text(hjust = 0.5))
        + stat_summary(fun.y=mean, geom="point", shape=23, size=4))

# Send the same plot to PNG file for storage
png(filename = "Plot4.png",width=880)
plot(gboxlog+geom_boxplot()+facet_grid(.~scc.level.one)+labs(x = "Year",
        y = "Log10 of Annual Emissions (Tons)",title="State Emissions from Coal Combustion Sources")
     + theme(plot.title = element_text(hjust = 0.5))
     + stat_summary(fun.y=mean, geom="point", shape=23, size=4))
dev.off()