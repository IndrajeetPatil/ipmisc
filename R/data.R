#' @title Edgar Anderson's Iris Data in long format.
#' @name iris_long
#' @details This famous (Fisher's or Anderson's) iris data set gives the
#'   measurements in centimeters of the variables sepal length and width and
#'   petal length and width, respectively, for 50 flowers from each of 3 species
#'   of iris. The species are Iris setosa, versicolor, and virginica.
#'
#' This is a modified dataset from `datasets` package.
#'
#' @format A data frame with 600 rows and 5 variables
#' \itemize{
#'   \item id. Dummy identity number for each flower (150 flowers in total).
#'   \item Species.	The species are *Iris setosa*, *versicolor*, and
#'   *virginica*.
#'   \item condition. Factor giving a detailed description of the attribute
#'   (Four levels: `"Petal.Length"`, `"Petal.Width"`,  `"Sepal.Length"`,
#'   `"Sepal.Width"`).
#'   \item attribute.	What attribute is being measured (`"Sepal"` or `"Pepal"`).
#'   \item measure.	What aspect of the attribute is being measured (`"Length"`
#'   or `"Width"`).
#'   \item value.	Value of the measurement.
#' }
#'
#' @source
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html}
#'
#' @examples
#' dim(iris_long)
#' head(iris_long)
#' dplyr::glimpse(iris_long)
"iris_long"


#' @title Tidy version of the "Bugs" dataset.
#' @name bugs_long
#' @details This data set, "Bugs", provides the extent to which men and women
#'   want to kill arthropods that vary in freighteningness (low, high) and
#'   disgustingness (low, high). Each participant rates their attitudes towards
#'   all anthropods. Subset of the data reported by Ryan et al. (2013).
#'
#' @format A data frame with 372 rows and 6 variables
#' \itemize{
#'   \item subject. Dummy identity number for each participant.
#'   \item gender. Participant's gender (Female, Male).
#'   \item region. Region of the world the participant was from.
#'   \item education. Level of education.
#'   \item condition. Condition of the experiment the participant gave rating
#'   for (**LDLF**: low freighteningness and low disgustingness; **LFHD**: low
#'   freighteningness and high disgustingness; **HFHD**: high freighteningness
#'   and low disgustingness; **HFHD**: high freighteningness and high
#'   disgustingness).
#'   \item desire. The desire to kill an arthropod was indicated on a scale from
#'   0 to 10.
#' }
#'
#' @source
#' \url{https://www.sciencedirect.com/science/article/pii/S0747563213000277}
#'
#' @examples
#' dim(bugs_long)
#' head(bugs_long)
#' dplyr::glimpse(bugs_long)
"bugs_long"
