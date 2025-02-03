\name{penguins_raw}
\encoding{UTF-8}
\docType{data}
\alias{penguins_raw}
\title{Penguin Size, Clutch, and Blood Isotope Data for Foraging Adults near Palmer Station, Antarctica}
\description{
  Includes nesting observations, penguin size data, and isotope measurements from blood samples for adult \enc{Adélie}{Adelie}, Chinstrap, and Gentoo penguins.
}
\usage{penguins_raw}
\format{
  A tibble with 344 rows and 17 variables:
  \describe{
    \item{studyName}{Sampling expedition from which data were collected, generated, etc.}
    \item{Sample Number}{an integer denoting the continuous numbering sequence for each sample}
    \item{Species}{a character string denoting the penguin species}
    \item{Region}{a character string denoting the region of Palmer LTER sampling grid}
    \item{Island}{a character string denoting the island near Palmer Station where samples were collected}
    \item{Stage}{a character string denoting reproductive stage at sampling}
    \item{Individual ID}{a character string denoting the unique ID for each individual in dataset}
    \item{Clutch Completion}{a character string denoting if the study nest observed with a full clutch, i.e., 2 eggs}
    \item{Date Egg}{a date denoting the date study nest observed with 1 egg (sampled)}
    \item{Culmen Length}{a number denoting the length of the dorsal ridge of a bird's bill (millimeters)}
    \item{Culmen Depth}{a number denoting the depth of the dorsal ridge of a bird's bill (millimeters)}
    \item{Flipper Length}{an integer denoting the length penguin flipper (millimeters)}
    \item{Body Mass}{an integer denoting the penguin body mass (grams)}
    \item{Sex}{a character string denoting the sex of an animal}
    \item{Delta 15 N}{a number denoting the measure of the ratio of stable isotopes 15N:14N}
    \item{Delta 13 C}{a number denoting the measure of the ratio of stable isotopes 13C:12C}
    \item{Comments}{a character string with text providing additional relevant information for data}
    \item{Sample}{a character string denoting whether the bird featured in the test or train set (or neither) in the original analysis (see References).}
  }
}
\source{
\enc{Adélie}{Adelie} penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female \enc{Adélie}{Adelie} penguins (Pygoscelis adeliae) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative, \doi{10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f}.

Gentoo penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Gentoo penguin (Pygoscelis papua) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative, \doi{10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689}.

Chinstrap penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Chinstrap penguin (Pygoscelis antarcticus) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 6. Environmental Data Initiative, \doi{10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e}.
}
\references{
Gorman, K. B., Williams, T. D. and Fraser, W. R. (2014) Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus Pygoscelis). PLoS ONE \bold{9}, 3, e90081. doi:10.1371/journal.pone.0090081.
}
\note{
This data is also available in the \CRANpkg{palmerpenguins} package. See also \url{https://allisonhorst.github.io/palmerpenguins/} for further details and resources.
}
\keyword{datasets}