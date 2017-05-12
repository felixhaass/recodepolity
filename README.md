# recodepolity
This R package applies the corrections to the Polity coding according to Vreeland (2008) and Plümper &amp; Neumayer  (2010)

You can install it through

``` {.r}
install.packages("devtools")
library(devtools)
install_github("felixhaass/recodepolity", dependencies = TRUE)
library(recodepolity)
```
# Basic Use

``` {.r}
output_df <- recodepolity(file = "http://www.systemicpeace.org/inscr/p4v2015.sav")
```

`output_df` contains all the recoded variables, for variable names see `help(recodepolity)`


