#' Recode Polity IV scores
#'
#' Calculates XPOLITY (Vreeland 2008) scores and corrected interregnum scores
#' (Pluemper and Neumayer 2010) for a current Polity IV dataset.
#'
#'
#' @param file Path or URL to Polity IV data frame. Defaults to the URL for the current
#' (2013) Polity IV .sav file.
#' @param option Takes the values \code{all}, \code{xpolity}, \code{inter}, or \code{combined}.
#' Default is \code{all}. \code{all} calculates both XPOLITY and corrected interregnum
#' scores. \code{xpolity} only calculates XPOLITY scores; \code{inter} only calculates
#' interregnum scores. \code{combined} calculates XPOLITY scores based on the corrected
#' interrengum scores.
#'
#' @details Depending on which \code{option} the user provides different columns
#' are added to the dataset given by the \code{file} argument.
#'
#' \code{all} adds the columns \code{xrcomp_xpol}, \code{xropen_xpol}, \code{xconst_xpol},
#' \code{parreg_xpol}, \code{parcomp_xpol}, \code{xpolity}. The first five columns are the weighted components of
#' Polity IV, calculated according to Polity IV coding manual and Pluemper's (2008)
#' \code{Stata} code. \code{xpolity} represents the combined components without {parreg_xpol}
#' and \code{parcomp_xpol}. The \code{all} option also adds the columns \code{polity2min},
#' \code{polity2max}, and \code{polity2inter}, each representing Pluemper and
#' Neumayer's (2010) different options of recoding the polity2 intrregnum periods (for details
#' see Pluemper and Neumayer 2010).
#'
#' The option \code{xpolity} only adds the XPOLITY columns; the option {inter} only
#' adds the interregnum colums.
#'
#' The option \code{combined} adds columns with the suffixes \code{_min}, \code{_max},
#' \code{_inter} to the Polity IV subcomponents as well as the final \code{xpolity} score.
#' This provides three different versions of the XPOLITY score, dependent on the
#' different recalculations (\code{min}, \code{max}, \code{inter}) from Pluemper
#' and Neumayer.
#'
#' @export
#'
#' @return Polity IV data frame with additional columns. Number & names of columns
#' depend on user options (see above).
#'
#' @examples
#' \dontrun{
#' # basic usage; assigns the recoded Polity dataset to the data frame 'polity'
#' polity <- recodepolity()
#' }
#'
#' @references
#' Marshall, Monty G., Keith Jaggers, and Ted Robert Gurr. 2010. "Polity IV
#' project. Political regime characteristics and transitions, 1800-2010. Dataset
#' user's manual." \url{http://www.systemicpeace.org/inscr/p4manualv2010.pdf.}
#'
#' Pluemper, Thomas, and Eric Neumayer. 2010. "The level of democracy during
#' interregnum periods: Recoding the polity2 score." \emph{Political Analysis}
#' 18(2): 206-226. \url{http://pan.oxfordjournals.org/content/18/2/206}
#'
#' Vreeland, James Raymond. 2008. "The effect of political regime on civil war.
#' Unpacking anocracy." \emph{Journal of Conflict Resolution} 52(3): 401-425.
#' \url{http://jcr.sagepub.com/content/52/3/401}
#'


recodepolity <- function(file = "http://www.systemicpeace.org/inscr/p4v2015.sav", option = "all") {


    # Print downloading message because it might take some time
    if(grepl("http", file)) {
      writeLines(paste0("Downloading Polity IV data from ", file))
    }

    polity <- foreign::read.spss(file, to.data.frame = TRUE)
    # polity <- read.csv(file, sep = "|")
    polity$country <- trim.trailing(polity$country) # trim trailing blank space

    # arrange by country-year
    polity <- dplyr::arrange(polity, ccode, year)

    if(option == "all") {

    # Vreeland / xpolity recoding
    polity <- plyr::ddply(polity, "ccode", xpolity)

    # Pluemper and Neumayer recoding
    polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2min = recode_polity2min(x)$polity2min))
    polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2max = recode_polity2max(x)$polity2max))
    polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2inter = recode_polity2inter(x)$polity2inter))

    return(polity)
  }

  if(option == "xpolity") {

    # Vreeland / xpolity recoding
    polity <- plyr::ddply(polity, "ccode", xpolity)

    return(polity)
  }

  if(option == "inter") {

    # Pluemper and Neumayer recoding
    polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2min = recode_polity2min(x)$polity2min))
    polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2max = recode_polity2max(x)$polity2max))
    polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2inter = recode_polity2inter(x)$polity2inter))


    return(polity)
  }

  if(option == "combined") {
    # Vreeland / xpolity recoding
    polity <- plyr::ddply(polity, "ccode", xpolity)

    colnames_xpolity <- c("xrcomp_xpol", "xropen_xpol", "xconst_xpol", "parreg_xpol", "parcomp_xpol", "xpolity")

    # since the recode_polity subfunctions all take "polity" as column to transform
    # it's easier to simply rename all xpolity columns, make the transformation and rename
    # back.
    polity <- plyr::rename(polity, c("polity" = "old_polity"))

    for(col_name in colnames_xpolity) {

      names(polity)[names(polity) == col_name] <- "polity"

      polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2min = recode_polity2min(x)$polity2min))
      polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2max = recode_polity2max(x)$polity2max))
      polity <- plyr::ddply(polity, "ccode", function(x) data.frame(x, polity2inter = recode_polity2inter(x)$polity2inter))

      polity <- plyr::rename(polity, c("polity2min" = paste0(col_name, "_min")))
      polity <- plyr::rename(polity, c("polity2max" = paste0(col_name, "_max")))
      polity <- plyr::rename(polity, c("polity2inter" = paste0(col_name, "_inter")))
      polity <- plyr::rename(polity, c("polity" = paste0(col_name)))
    }

    return(polity)

  }


}
