xpolity <- function(x) {

  # Map XRCOMP (Competitiveness of Executive Recruitment)
  x$xrcomp_xpol <- x$xrcomp
  x$xrcomp_xpol <- ifelse(x$xrcomp == 1, -2, x$xrcomp_xpol) # if COMP == -2 (selection) weigh by -2
  x$xrcomp_xpol <- ifelse(x$xrcomp == 2, 1, x$xrcomp_xpol) # if COMP == 2 (transition) weigh by 1
  x$xrcomp_xpol <- ifelse(x$xrcomp == 3, 2, x$xrcomp_xpol)
  # remaining category fro XRCOMP is 0, which is only coded when XRREG is "1"

  # Map XROPEN (Openness of Executive Recruitment)
  x$xropen_xpol <- x$xropen
  x$xropen_xpol <- ifelse(x$xropen == 1 & x$xrcomp == 1, - 1, x$xropen_xpol)
  x$xropen_xpol <- ifelse(x$xropen == 1 & x$xrcomp != 1, 0, x$xropen_xpol)
  x$xropen_xpol <- ifelse(x$xropen == 2 & x$xrcomp == 1, -1, x$xropen_xpol)
  x$xropen_xpol <- ifelse(x$xropen == 2 & x$xrcomp != 1, 0, x$xropen_xpol)
  x$xropen_xpol <- ifelse(x$xropen == 3 & x$xrcomp > 1, 1, x$xropen_xpol)
  x$xropen_xpol <- ifelse(x$xropen == 3 & x$xrcomp < 2 & x$xrcomp > -20, 0, x$xropen_xpol) # >-20 is to prevent -66/-77/-88 values from being evaluated
  x$xropen_xpol <- ifelse(x$xropen == 4 & x$xrcomp > 1, 1, x$xropen_xpol)
  x$xropen_xpol <- ifelse(x$xropen == 4 & x$xrcomp < 2 & x$xrcomp > -20, 0, x$xropen_xpol)

  # Map XCONST (Executive Constraints)
  x$xconst_xpol <- x$xconst
  x$xconst_xpol <- ifelse(x$xconst == 1, -3, x$xconst_xpol)
  x$xconst_xpol <- ifelse(x$xconst == 2, -2, x$xconst_xpol)
  x$xconst_xpol <- ifelse(x$xconst == 3, -1, x$xconst_xpol)
  x$xconst_xpol <- ifelse(x$xconst == 4, 1, x$xconst_xpol)
  x$xconst_xpol <- ifelse(x$xconst == 5, 2, x$xconst_xpol)
  x$xconst_xpol <- ifelse(x$xconst == 6, 3, x$xconst_xpol)
  x$xconst_xpol <- ifelse(x$xconst == 7, 4, x$xconst_xpol)

  # Map PARREG
  x$parreg_xpol <- x$parreg
  x$parreg_xpol <- ifelse(x$parreg_xpol == 1, 0, x$parreg_xpol)
  x$parreg_xpol <- ifelse(x$parreg_xpol == 2, 0, x$parreg_xpol)
  x$parreg_xpol <- ifelse(x$parreg_xpol == 3, -1, x$parreg_xpol)
  x$parreg_xpol <- ifelse(x$parreg_xpol == 4, -2, x$parreg_xpol)
  x$parreg_xpol <- ifelse(x$parreg_xpol == 5, 0, x$parreg_xpol)

  # Map PARCOMP
  x$parcomp_xpol <- x$parcomp
  x$parcomp_xpol <- ifelse(x$parcomp_xpol == 1, -2, x$parcomp_xpol)
  x$parcomp_xpol <- ifelse(x$parcomp_xpol == 2, -1, x$parcomp_xpol)
  x$parcomp_xpol <- ifelse(x$parcomp_xpol == 3, 1, x$parcomp_xpol)
  x$parcomp_xpol <- ifelse(x$parcomp_xpol == 4, 2, x$parcomp_xpol)
  x$parcomp_xpol <- ifelse(x$parcomp_xpol == 5, 3, x$parcomp_xpol)
  x$parcomp_xpol <- ifelse(x$parcomp_xpol == 0, 0, x$parcomp_xpol)

  # calculate polity for comparison
  #   x$polity_new <- apply(x[, c("xrcomp_xpol", "xropen_xpol", "xconst_xpol", "parreg_xpol", "parcomp_xpol")],
  #                         1,
  #                         function(x) {
  #                           if(any(c(-66, -77, -88) %in% x)) {
  #                             return(x[1]) } else {
  #                               sum(x)
  #                             }
  #                         })

  # calculate xpolity, i.e. polity without parreg & parcomp components
  x$xpolity <- apply(x[, c("xrcomp_xpol", "xropen_xpol", "xconst_xpol")],
                     1,
                     function(x) {
                       if(any(c(-66, -77, -88) %in% x)) {
                         return(x[1]) } else { # return -77/-88/-66 if in row
                           sum(x)  # sum over rows
                         }
                     })


  return(x)

}
