# Load necessary libraries
library(dplyr)
library(openxlsx)
library(readr)
library(tidyr)


# Set options
options(stringsAsFactors = FALSE)

# Specify working directory
workingDir <- "/Users/Simar/VIRSCAN/data/qcReports"
setwd(workingDir)

# Read the VirScan data from Excel file
d <- read.xlsx("QC_Reports.xlsx", sheet = 1)

# Define a function to calculate quality calls
calculate_quality_call <- function(originalQCreport) {
  quality_call <- originalQCreport %>%
    select(SampleID, reads_mapped, readDepth) %>%
    mutate(
      alignedReadsQC = case_when(
        reads_mapped >= 200000 ~ "Good",
        reads_mapped >= 160000 ~ "PossibleGood",
        reads_mapped >= 100001 ~ "Questionable",
        TRUE ~ "Fail"  # This handles cases where reads_mapped < 100001
      ),
      readDepthQC = case_when(
        readDepth >= 4 ~ "Good",
        readDepth >= 3.5 ~ "PossibleGood",
        readDepth >= 2.5 ~ "Questionable",
        TRUE ~ "Fail"  # This handles cases where readDepth < 2.5
      )
    ) %>%
    mutate(
      lowestQCval = pmin(alignedReadsQC, readDepthQC)  # Select the stricter QC value
    ) %>%
    select(SampleID, alignedReadsQC, readDepthQC, lowestQCval)
  
  return(quality_call)
}

# Apply the quality assessment function
qualityCalls <- calculate_quality_call(d)

# Join quality calls with the original data
dataWithQualityCalls <- d %>%
  left_join(qualityCalls, by = "SampleID")

# Write the results to an Excel file
write.xlsx(dataWithQualityCalls, "virscan_data_with_quality.xlsx")
