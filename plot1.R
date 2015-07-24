################################################################################
################################################################################


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
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Build the plot +++++++++++++++++++++++++++++++++++++++++++
print('[INFO] Building the plot...')

# Open a new file device
png(filename='plot1.png')

par(mar=c(6, 6, 4, 4))

totalEmissionByYear <- tapply(pm25$Emissions, pm25$year, sum)

barplot(
  totalEmissionByYear / (10 ** 6),
  main='Total PM2.5 emission from all sources\nin the US for each year',
  xlab='Year',
  ylab='PM2.5 emission (Megatons)',
  ylim=c(0, 8),
)

# Close the file device
dev.off()
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
