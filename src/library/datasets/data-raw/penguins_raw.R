# Code adapted from the palmerpenguin package
# by Allison Horst, Alison Hill, and Kristen Gorman
# https://github.com/allisonhorst/palmerpenguins

# penguins raw ------------------------------------------------------------

# Download raw data
# Adelie penguin data from: https://doi.org/10.6073/pasta/abc50eed9138b75f54eaada0841b9b86
uri_adelie <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-pal.219.3&entityid=002f3893385f710df69eeebe893144ff"

# Gentoo penguin data from: https://doi.org/10.6073/pasta/2b1cff60f81640f182433d23e68541ce
uri_gentoo <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-pal.220.3&entityid=e03b43c924f226486f2f0ab6709d2381"

# Chinstrap penguin data from: https://doi.org/10.6073/pasta/409c808f8fc9899d02401bdb04580af7
uri_chinstrap <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-pal.221.2&entityid=fe853aa8f7a59aa84cdd3197619ef462"

# Combining the URIs
uris <- c(uri_adelie, uri_gentoo, uri_chinstrap)

# Download data and combine into one dataframe
penguins_raw_list <- lapply(uris, read.csv) 
penguins_raw <- do.call(rbind, penguins_raw_list)

# Adjustments to make penguins_raw identical to palmerpenguins:::penguins_raw
penguins_raw$Sample.Number <- as.numeric(penguins_raw$Sample.Number)
penguins_raw$Date.Egg <- as.Date(penguins_raw$Date.Egg)
penguins_raw$Flipper.Length..mm. <- as.numeric(penguins_raw$Flipper.Length..mm.)
penguins_raw$Body.Mass..g. <- as.numeric(penguins_raw$Body.Mass..g.)
penguins_raw$Sex <- replace(penguins_raw$Sex, penguins_raw$Sex %in% c("", "."), NA)
penguins_raw$Comments <- replace(penguins_raw$Comments, penguins_raw$Comments == "", NA)

colnames(penguins_raw) <- c(
  "studyName", "Sample Number", "Species", "Region", "Island", "Stage",
  "Individual ID", "Clutch Completion", "Date Egg", "Culmen Length (mm)",
  "Culmen Depth (mm)", "Flipper Length (mm)", "Body Mass (g)", "Sex",
  "Delta 15 N (o/oo)", "Delta 13 C (o/oo)", "Comments"
)

# add sample numbers that correspond to test/train set in Gorman et al. (2014)
# these have been provided by Kristen Gorman
ADPE_train_sample_nums <- c(
  1, 2, 3, 5, 14, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 31, 32, 33,
  34, 41, 42, 43, 45, 46, 47, 49, 50, 52, 56, 57, 61, 62, 63, 64, 66, 67, 71,
  73, 74, 76, 78, 81, 84, 85, 88, 89, 91, 92, 93, 94, 95, 96, 98, 99, 102, 
  104, 105, 107, 108, 112, 113, 115, 116, 117, 118, 119, 120, 123, 124, 125, 
  128, 129, 130, 133, 136, 138, 142, 143, 144, 145, 147, 148, 149, 150, 151, 
  152
)

ADPE_test_sample_nums <- c(
  6, 13, 15, 28, 35, 36, 37, 38, 44, 51, 53, 54, 55, 58, 59, 60, 65, 68, 72, 
  75, 77, 79, 80, 82, 83, 86, 87, 90, 97, 100, 101, 103, 106, 109, 110, 111, 
  114, 126, 127, 134, 135, 137, 141, 146
)

CHPE_train_sample_nums <- c(
  3, 5, 6, 7, 8, 9, 13, 15, 16, 19, 22, 29, 30, 32, 33, 34, 35, 37, 41, 42, 
  43, 45, 47, 48, 50, 52, 53, 54, 55, 56, 57, 58, 61, 63, 67, 68
)

CHPE_test_sample_nums <- c(4, 10, 11, 12, 14, 20, 21, 31, 36, 38, 44, 46, 49, 
                           51, 59, 60, 62, 64)

GEPE_train_sample_nums <- c(
  2, 4, 5, 7, 9, 10, 13, 14, 15, 20, 21, 22, 24, 25, 26, 28, 30, 31, 32, 33, 
  34, 35, 36, 37, 38, 39, 40, 44, 49, 50, 52, 53, 54, 55, 60, 62, 63, 64, 65, 
  66, 69, 70, 73, 75, 76, 77, 78, 79, 82, 84, 85, 86, 89, 90, 91, 93, 94, 95, 
  97, 98, 99, 101, 102, 103, 106, 109, 110, 112, 114, 115, 118, 121, 123, 124
)

GEPE_test_sample_nums <- c(
  1, 3, 6, 8, 16, 17, 18, 19, 23, 29, 43, 45, 46, 51, 56, 57, 58, 59, 61, 68,
  71, 72, 74, 80, 81, 83, 87, 88, 92, 96, 100, 104, 107, 108, 111, 113, 116,
  122
)

# get count of each species
n_Adelie <- sum(grepl("Adelie", penguins_raw$Species))
n_Gentoo <- sum(grepl("Gentoo", penguins_raw$Species))
n_Chinstrap <- sum(grepl("Chinstrap", penguins_raw$Species))

# vector of train/test for each species, then together
Adelie_sample <- rep(NA, n_Adelie)
Adelie_sample[ADPE_train_sample_nums] <- "train"
Adelie_sample[ADPE_test_sample_nums] <- "test"
Gentoo_sample <- rep(NA, n_Gentoo)
Gentoo_sample[GEPE_train_sample_nums] <- "train"
Gentoo_sample[GEPE_test_sample_nums] <- "test"
Chinstrap_sample <- rep(NA, n_Chinstrap)
Chinstrap_sample[CHPE_train_sample_nums] <- "train"
Chinstrap_sample[CHPE_test_sample_nums] <- "test"
Sample <- c(Adelie_sample, Gentoo_sample, Chinstrap_sample)

# Add sample column to penguins_raw
penguins_raw$Sample <- Sample

dump("penguins_raw", "./src/library/datasets/data/penguins_raw.R")
