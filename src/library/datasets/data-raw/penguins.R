# Code adapted from the palmerpenguin package
# by Allison Horst, Alison Hill, and Kristen Gorman
# https://github.com/allisonhorst/palmerpenguins

load("./src/library/datasets/data/penguins_raw.rda")

penguins <- penguins_raw[, c("Species", "Island", 
                             "Culmen Length (mm)", "Culmen Depth (mm)", 
                             "Flipper Length (mm)", "Body Mass (g)", 
                             "Sex", "Date Egg")] 
colnames(penguins) <- c(
  "species", "island", "bill_len", "bill_dep", "flipper_len",
  "body_mass", "sex", "year"
)
penguins$species <- regmatches(penguins$species,  
                               regexpr("^\\w+\\b", penguins$species))
penguins$species <- as.factor(penguins$species)
penguins$island <- as.factor(penguins$island)
penguins$flipper_len <- as.integer(penguins$flipper_len)
penguins$body_mass <- as.integer(penguins$body_mass)
penguins$sex <- tolower(penguins$sex)
penguins$sex <- as.factor(penguins$sex)
penguins$year <- regmatches(penguins$year,  
                            regexpr("\\d{4}", penguins$year))
penguins$year <- as.integer(penguins$year)

save(penguins, file = "./src/library/datasets/data/penguins.rda")

# Check identical with version palmerpenguins package
# rm(penguins)
# load("./src/library/datasets/data/penguins.rda")
# old_nms <- sub("len", "length_mm",
#                sub("dep","depth_mm",
#                    sub("mass", "mass_g", colnames(penguins))))
# colnames(penguins) <- old_nms
# identical(penguins, palmerpenguins:::penguins_df)
