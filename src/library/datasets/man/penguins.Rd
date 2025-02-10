\name{penguins}
\encoding{UTF-8}
\docType{data}
\title{Size Measurements for Penguins near Palmer Station, Antarctica}
\alias{penguins}
\alias{penguins_raw}
\description{
  The data set of size measurements for three adult foraging penguin
  species on three islands in the Palmer Archipelago, Antarctica, including their
  size (flipper length, body mass, bill dimensions), and sex.

  These columns of \code{penguins} are a subset of the more extensive
  \code{penguins_raw} data frame which results from reading the original
  data, containing only numeric (double precision), character and Date variables.
  \code{penguins_raw} additionally contains nesting observations and blood isotope data. 
}
\usage{
penguins
penguins_raw
}
\format{
  \code{penguins} is a data frame with 344 rows and 8 variables:
  \describe{
    \item{species}{a factor denoting penguin species (Adelie, Chinstrap and Gentoo)}
    \item{island}{a factor denoting island in Palmer Archipelago, Antarctica (Biscoe, Dream or Torgersen)}
    \item{bill_len}{a number denoting bill length (millimeters)}
    \item{bill_dep}{a number denoting bill depth (millimeters)}
    \item{flipper_len}{an integer denoting flipper length (millimeters)}
    \item{body_mass}{an integer denoting body mass (grams)}
    \item{sex}{a factor denoting penguin sex (female, male)}
    \item{year}{an integer denoting the study year (2007, 2008, or 2009)}
  }

  \code{penguins_raw} is a data frame with 344 rows and 18 variables.

  8 columns correspond to columns in \code{penguins}, though with different variable names
  and/or classes:
  \describe{
    \item{Species}{a character string}
    \item{Island}{a character string}
    \item{Culmen Length (mm)}{a number denoting bill length}
    \item{Culmen Depth (mm)}{a number denoting bill depth}
    \item{Flipper Length (mm)}{an number denoting flipper length}
    \item{Body Mass (g)}{a number denoting body mass}
    \item{Sex}{a character string}
    \item{Date Egg}{a Date denoting when study nest observed with 1 egg (sampled). 
                    The year component of this date is the `year` column in \code{penguins}}
  }

  There are 10 further columns:
  \describe{
    \item{studyName}{a character string denoting the sampling expedition from which data were collected, generated, etc.}
    \item{Sample Number}{a number denoting the continuous numbering sequence for each sample}
    \item{Region}{a character string denoting the region of Palmer LTER sampling grid}
    \item{Stage}{a character string denoting reproductive stage at sampling}
    \item{Individual ID}{a character string denoting the unique ID for each individual in dataset}
    \item{Clutch Completion}{a character string denoting if the study nest observed with a full clutch, i.e., 2 eggs}
    \item{Delta 15 N (o/oo)}{a number denoting the measure of the ratio of stable isotopes 15N:14N}
    \item{Delta 13 C (o/oo)}{a number denoting the measure of the ratio of stable isotopes 13C:12C}
    \item{Comments}{a character string with text providing additional relevant information for data}
    \item{Set}{a character string denoting whether the bird featured in the test or train set (or \code{NA} for neither) in the original analysis (see References).}
  }  
}
\source{
  \describe{
    \item{\enc{Adélie}{Adelie} penguins:}{Palmer Station Antarctica LTER and K. Gorman (2020).
      Structural size measurements and isotopic signatures of foraging
      among adult male and female \enc{Adélie}{Adelie} penguins (Pygoscelis adeliae)
      nesting along the Palmer Archipelago near Palmer Station, 2007-2009
      ver 5. Environmental Data Initiative, \doi{10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f}.}

    \item{Gentoo penguins:}{Palmer Station Antarctica LTER and K. Gorman (2020).
      \doi{10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689}.}

    \item{Chinstrap penguins:}{Palmer Station Antarctica LTER and K. Gorman. 2020.
      \doi{10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e}.}
  }
  
  The title naming convension for the source for the Gentoo and Chinstrap data is that same as for \enc{Adélie}{Adelie} penguins. 
}
\references{
  Gorman, K. B., Williams, T. D. and Fraser, W. R. (2014)
  Ecological Sexual Dimorphism and Environmental Variability within a
  Community of Antarctic Penguins (Genus Pygoscelis).
  \emph{PLoS ONE} \bold{9}, 3, e90081; \doi{10.1371/journal.pone.0090081}.
}
\note{
  This data is also available in the \CRANpkg{palmerpenguins} package.  See
  also \url{https://allisonhorst.github.io/palmerpenguins/} for further
  details and resources.
}
\examples{
mosaicplot( ~ species + island,       data = penguins)
mosaicplot( ~ species + island + sex, data = penguins)

## Produce the long variable names as from {palmerpenguins} pkg:
old_nms <- sub("len", "length_mm",
               sub("dep","depth_mm",
                   sub("mass", "mass_g", colnames(penguins))))
## compare old and current:
noquote(rbind(old_nms, nms = colnames(penguins)))

\dontrun{ # << not in this example, keeping shorter 'penguins' names:
    colnames(penguins) <- old_nms
}
}
\keyword{datasets}
