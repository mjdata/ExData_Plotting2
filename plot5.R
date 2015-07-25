################################################################################
################################################################################


# Libraries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download the raw data sets to use in this assignment.
#
# This function is aware of the OS in order to set the correct download method.
#
# @param dataSetsURL  character(1) The URL of the data sets.
# @param dataSetsPath character(1) The file system path to use in order to save
#                                  the data sets on disk.
downloadDataSets <- function(dataSetsURL, dataSetsPath) {
  downloadMethod <- if (Sys.info()['sysname'] == 'Windows') {
    'auto'
  } else {
    'curl'
  }

  download.file(dataSetsURL, dataSetsPath, downloadMethod)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Main ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Paths & URLs +++++++++++++++++++++++++++++++++++++++++++++
dataDirPath <- file.path('.', 'data')
dataSetsPath <- file.path('.', dataDirPath, 'datasets.zip')
dataSetsURL <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Download the data sets to disk if it's necessary +++++++++
if (!file.exists(dataSetsPath)) {
  if (!file.exists(dataDirPath)) {
    dir.create(dataDirPath)
  }

  print('[INFO] Downloading the raw data sets...')

  downloadDataSets(dataSetsURL, dataSetsPath)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Unzip the date sets ++++++++++++++++++++++++++++++++++++++
print('[INFO] Unzipping the data sets...')

unzip(dataSetsPath, exdir=dataDirPath)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load the data sets into memory +++++++++++++++++++++++++++
print('[INFO] Loading the raw data sets into memory...')

pm25 <- readRDS(
  file.path(dataDirPath, 'summarySCC_PM25.rds')
)

scc <- readRDS(
  file.path(dataDirPath, 'Source_Classification_Code.rds')
)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Build the plot +++++++++++++++++++++++++++++++++++++++++++
print('[INFO] Building the plot...')

baltimoreCityVehiclePm25 <- subset(pm25,
  (fips == '24510') & (SCC %in% scc[grepl('.*Vehicle.*', scc$Short.Name),]$SCC)
)

totalBaltimoreCityVehiclePm25ByYear <- tapply(
  baltimoreCityVehiclePm25$Emissions, baltimoreCityVehiclePm25$year, sum
)

totalBaltimoreCityVehiclePm25Emissions <- data.frame(
  tons=totalBaltimoreCityVehiclePm25ByYear / (10 ** 3),
  year=names(totalBaltimoreCityVehiclePm25ByYear)
)

tobj <- (
  ggplot(totalBaltimoreCityVehiclePm25Emissions, aes(x=year, y=tons)) +
  geom_bar(stat='identity', fill='Grey') +
  ylab('PM2.5 emission (Kilotons)') +
  ggtitle('PM2.5 emissions from motor\nvehicle sources in Baltimore City') +
  theme_bw() +
  theme(legend.position='none', plot.title=element_text(face='bold'))
)

ggsave('plot5.png', tobj, width=4.5, height=4.5, dpi=100)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
