# Coursera_exploratory_wk4
Course assignment for the exploratory data analysis module (data science specialisation) on Coursera

The assignment is to answer a number of questions about air quality data from the US EPA using exploratory data analysis techniques learned during the module. The relevant data is provided on the course website https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

## Data Input
Two files are required as input to this analysis, provided by the course instructors (see above):

1.  PM2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.
  * fips: A five-digit number (represented as a string) indicating the U.S. county
  * SCC: The name of the source as indicated by a digit string (see source code classification table)
  * Pollutant: A string indicating the pollutant
  * Emissions: Amount of PM2.5 emitted, in tons
  * type: The type of source (point, non-point, on-road, or non-road)
  * year: The year of emissions recorded

2. Source Classification Code Table (Source_Classification_Code.rds): This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.
