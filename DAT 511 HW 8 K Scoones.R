
# Homework 8 - DAT 511

# Question 1: Idaho Housing Data

# Load the housing data
housing <- read.csv("getdata_data_ss06hid.csv")

# Check the data
dim(housing)
names(housing)
head(housing)

# Check the two variables

table(housing$ACR, useNA = "ifany")
table(housing$AGS, useNA = "ifany")


# Create logical vector
agricultureLogical <- housing$ACR == 3 & housing$AGS == 6

# Identify the matching rows
matching_rows <- which(agricultureLogical)

# Display the first three row numbers
q1result <- head(matching_rows, 3)

print(q1result)



# Question 2: JPEG Image

# Download the image
image_url <- "https://drive.google.com/uc?export=download&id=10KOr-BZuoL0bVcce8tlM7_GvQyECpjPp"

download.file(
  image_url,
  destfile = "image.jpg",
  mode = "wb"
)

# Load the jpeg package
library(jpeg)

# Read the image using native = TRUE
img <- readJPEG("image.jpg", native = TRUE)

# Calculate the 30th and 80th quantiles
q2result <- quantile(img, probs = c(0.30, 0.80))

print(q2result)



# Question 3: GDP and Education Data

# Load GDP data
gdp <- read.csv(
  "gdp.csv",
  header = FALSE,
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

# Keep the necessary columns
gdp <- gdp[, c(1, 2, 4, 5)]

# Rename columns
names(gdp) <- c("CountryCode", "Rank", "Country", "GDP")

# Convert ranking to numeric
gdp$Rank <- as.numeric(gdp$Rank)

# Keep only ranked countries
gdp <- gdp[!is.na(gdp$Rank), ]

# Check the cleaned data
print(nrow(gdp))
print(head(gdp))
print(tail(gdp))

edu <- read.csv("edu.csv")

# Check the education data
names(edu)
head(edu)


# Merge GDP and education data by country code
combined <- merge(gdp, edu, by = "CountryCode")

# Check the merged data
print(dim(combined))
names(combined)
head(combined)

# Average GDP rank for high-income groups
high_income <- combined[
  combined$Income.Group %in% c(
    "High income: OECD",
    "High income: nonOECD"
  ),
]

print(aggregate(
  Rank ~ Income.Group,
  data = high_income,
  FUN = mean
))


# Divide GDP rankings into five quantile groups
combined$RankGroup <- cut(
  combined$Rank,
  breaks = quantile(
    combined$Rank,
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  ),
  include.lowest = TRUE
)

# Check the five rank groups
print(table(combined$RankGroup))

# Compare rank groups with income groups
print(table(combined$RankGroup, combined$Income.Group))

# Count lower-middle-income countries among the top 38
q3result <- sum(
  combined$Rank <= 38 &
    combined$Income.Group == "Lower middle income",
  na.rm = TRUE
)

print(q3result)
