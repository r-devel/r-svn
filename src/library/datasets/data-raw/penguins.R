# Code adapted from the palmerpenguin package
# by Allison Horst, Alison Hill, and Kristen Gorman
# https://github.com/allisonhorst/palmerpenguins

load("./src/library/datasets/data/penguins_raw.rda")

penguins <- penguins_raw[, c("Species", "Island", 
                             "Culmen Length (mm)", "Culmen Depth (mm)", 
                             "Flipper Length (mm)", "Body Mass (g)", 
                             "Sex", "Date Egg")] 
colnames(penguins) <- c(
  "species", "island", "bill_length_mm", "bill_depth_mm", "flipper_length_mm",
  "body_mass_g", "sex", "year"
)
penguins$species <- regmatches(penguins$species,  
                               regexpr("^\\w+\\b", penguins$species))
penguins$species <- as.factor(penguins$species)
penguins$island <- as.factor(penguins$island)
penguins$flipper_length_mm <- as.integer(penguins$flipper_length_mm)
penguins$body_mass_g <- as.integer(penguins$body_mass_g)
penguins$sex <- tolower(penguins$sex)
penguins$sex <- as.factor(penguins$sex)
penguins$year <- regmatches(penguins$year,  
                            regexpr("\\d{4}", penguins$year))
penguins$year <- as.integer(penguins$year)

save(penguins, file = "./src/library/datasets/data/penguins.rda")

# Check identical with version palmerpenguins package
# rm(penguins)
# load("./src/library/datasets/data/penguins.rda")
# identical(penguins, palmerpenguins:::penguins_df)
