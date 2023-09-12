# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

# Read the provided data (replace with your data source)
# Specify working directory
workingDir <- "/Users/Simar/VIRSCAN/data/qcReports"
setwd(workingDir)

# Read the VirScan data from Excel file
d <- read.xlsx("VS56QC_ReportWithAnnotation.xlsx", sheet = 1)

# Control Samples Analysis
control_analysis <- d %>%
  filter(grepl("Control", Sample.Annotation)) %>%
  group_by(PoolID) %>%
  mutate(
    mean_SumAllSpecies = mean(sumAllSpecies),
    mean_ViralScore = mean(Human.herpesvirus.1),  # Choose a relevant QC virus
    mean_mapped_reads = mean(reads_mapped),
    mean_percent_reads_aligned = mean(pctReadsAligned),
    mean_corr_counts = mean(pearsonCor),
    mean_corr_pct_detected = mean(pctDetected)
  ) %>%
  select(Sample.Annotation, starts_with("mean_")) %>%
  unique()

# Samples Analysis
samples_analysis <- d %>%
  filter(grepl("Emperical", Sample.Annotation)) %>%
  group_by(PoolID) %>%
  mutate(
    mean_mapped_reads = mean(reads_mapped),
    median_pct_reads_aligned = median(pctReadsAligned),
    mean_corr_counts = mean(pearsonCor),
    mean_corr_pct_detected = mean(pctDetected)
  ) %>%
  select(Sample.Annotation, starts_with("mean_")) %>%
  unique()

# Mock IP Analysis
mock_analysis <- d %>%
  filter(grepl("Mock-IP", Sample.Annotation)) %>%
  group_by(PoolID) %>%
  mutate(
    mean_mapped_reads = mean(reads_mapped),
    median_pct_reads_aligned = median(pctReadsAligned),
    mean_corr_counts = mean(pearsonCor),
    mean_corr_pct_detected = mean(pctDetected)
  ) %>%
  select(Sample.Annotation, starts_with("mean_")) %>%
  unique()

# Batch Level Comparison
batch_comparison <- d %>%
  group_by(PoolID) %>%
  summarize(
    num_controls = sum(grepl("Control", Sample.Annotation)),
    num_samples = sum(grepl("Emperical", Sample.Annotation)),
    num_mock = sum(grepl("Mock-IP", Sample.Annotation)),
    mean_SumAllSpecies = mean(sumAllSpecies),
    mean_ViralScore = mean(Human.herpesvirus.1),  # Choose a relevant QC virus
    mean_mapped_reads = mean(reads_mapped),
    median_pct_reads_aligned = median(pctReadsAligned),
    mean_corr_counts = mean(pearsonCor),
    mean_corr_pct_detected = mean(pctDetected)
    # Add more relevant batch-level metrics here
  ) %>%
  ungroup()

# Create a table for batch-level comparison
batch_comparison_table <- kable(batch_comparison, format = "markdown")

# add another row to batch_comparison to simulate another batch

batchComparison <- bind_rows(batch_comparison, batch_comparison)
batchComparison$PoolID[2] <- "VS57"
batchComparison$mean_SumAllSpecies[2] <- 140
batchComparison$mean_mapped_reads[2] <- 900000
batchComparison <- melt(data.frame(batchComparison))

# generate individual plots, then present separately in a grid format

sumAllSpeciesPlot <- ggplot(batchComparison %>% filter(variable == "mean_SumAllSpecies"), aes(PoolID, value)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Mean Sum of All Species")

meanMappedReadsPlot <- ggplot(batchComparison %>% filter(variable == "mean_mapped_reads"), aes(PoolID, value)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  ylab("Mean Mapped Reads")

grid.arrange(sumAllSpeciesPlot, meanMappedReadsPlot,
             nrow = 1,
             heights = c(8))