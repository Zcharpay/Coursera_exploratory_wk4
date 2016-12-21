## Script to load EPA air quality data and generate an exploratory plot to answer
## this question:
## "Compare emissions from motor vehicle sources in Baltimore City with emissions 
##  from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
##  Which city has seen greater changes over time in motor vehicle emissions?"

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
mvdatabaltimore <- subset(NEI,scc %in% mvsources$scc & fips=="24510")
mvdataLA <- subset(NEI,scc %in% mvsources$scc & fips=="06037")

# Merge the subsetted NEI and SCC data into one dataframe
mvdatabaltimore <- merge(x=mvdatabaltimore,y=mvsources,by="scc")
mvdataLA <- merge(x=mvdataLA,y=mvsources,by="scc")

# Group and summarise data to enable multi-panel plot with ggplot
mvdatabaltimore_grp <- group_by(mvdatabaltimore,year,ei.sector)
mvdatabaltimore_grp <- summarise(mvdatabaltimore_grp, emitsum = sum(emissions))
mvdataLA_grp <- group_by(mvdataLA,year,ei.sector)
mvdataLA_grp <- summarise(mvdataLA_grp, emitsum = sum(emissions))

# Calculate summary variables for both cities: change in emissions versus 1999, and
# annual emissions expressed as a percentage of 1999 levels.
# These will allow me to test which city has seen a greater change from two different
# persepectives, i.e. total reduction in tons, and percentage reduction in tons
statsbalti <- dcast(mvdatabaltimore_grp,year ~ ei.sector,value.var="emitsum")
colnames(statsbalti) <- c("year","hvydiesel","lightdiesel","hvygas","lightgas")
statsbalti <- mutate(statsbalti,deltahvydiesel=hvydiesel-statsbalti$hvydiesel[1],
                     deltalightdiesel=lightdiesel-statsbalti$lightdiesel[1],
                     deltahvygas=hvygas-statsbalti$hvygas[1],
                     deltalightgas=lightgas-statsbalti$lightgas[1])
statsbalti <- mutate(statsbalti,pcthvydiesel=(hvydiesel)/statsbalti$hvydiesel[1],
                     pctlightdiesel=(lightdiesel)/statsbalti$lightdiesel[1],
                     pcthvygas=(hvygas)/statsbalti$hvygas[1],
                     pctlightgas=(lightgas)/statsbalti$lightgas[1])
statsbalti <- melt(statsbalti,id="year")
statsbalti <- cbind(statsbalti[1:16,],delta=statsbalti[17:32,"value"],pct=statsbalti[33:48,"value"])
statsbalti <- arrange(statsbalti,year)
mvdatabaltimore_grp <- cbind(ungroup(mvdatabaltimore_grp),deltato99=statsbalti$delta,pctof99=statsbalti$pct)
mvdatabaltimore_grp <- melt(mvdatabaltimore_grp,id=c("year","ei.sector","emitsum"),measure.vars=c("deltato99","pctof99"))

statsla <- dcast(mvdataLA_grp,year ~ ei.sector,value.var="emitsum")
colnames(statsla) <- c("year","hvydiesel","lightdiesel","hvygas","lightgas")
statsla <- mutate(statsla,deltahvydiesel=hvydiesel-statsla$hvydiesel[1],
                     deltalightdiesel=lightdiesel-statsla$lightdiesel[1],
                     deltahvygas=hvygas-statsla$hvygas[1],
                     deltalightgas=lightgas-statsla$lightgas[1])
statsla <- mutate(statsla,pcthvydiesel=(hvydiesel)/statsla$hvydiesel[1],
                     pctlightdiesel=(lightdiesel)/statsla$lightdiesel[1],
                     pcthvygas=(hvygas)/statsla$hvygas[1],
                     pctlightgas=(lightgas)/statsla$lightgas[1])
statsla <- melt(statsla,id="year")
statsla <- cbind(statsla[1:16,],delta=statsla[17:32,"value"],pct=statsla[33:48,"value"])
statsla <- arrange(statsla,year)
mvdataLA_grp <- cbind(ungroup(mvdataLA_grp),deltato99=statsla$delta,pctof99=statsla$pct)
mvdataLA_grp <- melt(mvdataLA_grp,id=c("year","ei.sector","emitsum"),measure.vars=c("deltato99","pctof99"))

# Merge all the data into one dataframe, for ease of plotting with ggplot2
plotdata <- rbind(cbind(mvdataLA_grp,City=rep("LA",nrow(mvdataLA_grp))),
                  cbind(mvdatabaltimore_grp,City=rep("Baltimore",nrow(mvdatabaltimore_grp))))

# rename the factors to make the chart easier to read for end-user
facetnames <- c("Mobile - On-Road Diesel Heavy Duty Vehicles"="Diesel Heavy Duty",
                "Mobile - On-Road Diesel Light Duty Vehicles"="Diesel Light Duty",
                "Mobile - On-Road Gasoline Heavy Duty Vehicles"="Gasoline Heavy Duty",
                "Mobile - On-Road Gasoline Light Duty Vehicles"="Gasoline Light Duty",
                "deltato99"="Change in Annual Tons Vs 1999",
                "pctof99"="Annual Tons as Percent of 1999 Levels")

# Plot the data as facet grid plot with ggplot2. Thw two cities will be separate data series
g <- ggplot(test3,aes(year,value))
plot(g+geom_line(aes(color=city))
        +facet_grid(variable~ei.sector,scales="free_y",labeller=as_labeller(facetnames))
        +labs(title="Comparison of Baltimore and LA Change in Motor Vehicle Emissions, 1999 to 2008")
        +labs(x = "Year",y=""))

# Send the same plot to PNG file for storage
png(filename = "Plot6.png",width=880)
plot(g+geom_line(aes(color=city))
     +facet_grid(variable~ei.sector,scales="free_y",labeller=as_labeller(facetnames))
     +labs(title="Comparison of Baltimore and LA Change in Motor Vehicle Emissions, 1999 to 2008")
     +labs(x = "Year",y=""))
dev.off()