# TennisBT
Tennis Bradley-Terry predictions for SC1 and SM1 group project. The code provides additional functionality to the `BradleyTerry2` package to predict WTA Tennis Matches.

## How to install
To install the package, use the 'install_github' function in R. This requires installation of the `devtools` package.

`install_github("oijbaker/TennisBT")`

After installing, use `library(TennisBT)` in the source code.
Ocassionally you may get an error: `tibble() not found`. To solve this, install and use the library `tidyverse` (`install_packages("tidyverse")`, then `library(tidyverse)`). 


## Information about each folder
`-data` Contains all WTA tour matches played in each year from 2013 to 2023, sourced from Jeff Sackman (<https://github.com/JeffSackmann/tennis_wta>). It also contains data splitting matches by the surface they were played on. Every `.rda` file in this folder is accessible in R by simply using the filename as a variable name. For example, 'wta_matches_2016.csv' is accessible using the identifier `wta_matches_2016` in R. 


`-man` Contains documentation for the functions provided by the package.

`-R` Contains the R code in `main.R`.
