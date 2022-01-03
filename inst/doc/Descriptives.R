## ---- include=FALSE-----------------------------------------------------------
library(magrittr)
library(dplyr)
library(Gmisc)

# Style all the table output using htmlTable's theme handler
htmlTable::setHtmlTableTheme(css.rgroup = "")
set.seed(123)

## -----------------------------------------------------------------------------
library(Gmisc)
data("mtcars")
mtcars <- mtcars %>% 
  mutate(am = factor(am, levels = 0:1, labels = c("Automatic", "Manual")),
         gear = factor(gear),
         # Make up some data for making it slightly more interesting
         col = factor(sample(c("red", "black", "silver"),
                             size = NROW(mtcars), 
                             replace = TRUE))) %>% 
  set_column_labels(mpg = "Gas",
                    wt = "Weight",
                    am = "Transmission",
                    gear = "Gears",
                    col = "Car color") %>% 
  set_column_units(mpg = "Miles/(US) gallon",
                   wt = "10<sup>3</sup> lbs")

## -----------------------------------------------------------------------------
mtcars %>% 
  getDescriptionStatsBy(mpg, 
                        wt,
                        am,
                        gear,
                        col,
                        by = am)

## -----------------------------------------------------------------------------
mtcars %>% 
  getDescriptionStatsBy(mpg, 
                        wt,
                        am,
                        gear,
                        col,
                        by = am,
                        continuous_fn = describeMedian)

## -----------------------------------------------------------------------------
mtcars %>% 
  getDescriptionStatsBy(mpg, 
                        `Weight&dagger;` = wt,
                        am,
                        gear,
                        col,
                        by = am) %>% 
  htmlTable(caption  = "Basic descriptive statistics from the mtcars dataset",
            tfoot = "&dagger; The weight is in 10<sup>3</sup> kg")

## -----------------------------------------------------------------------------
mtcars %>% 
  getDescriptionStatsBy(mpg, 
                        `Weight&dagger;` = wt,
                        am,
                        gear,
                        col,
                        by = am,
                        digits = 0,
                        add_total_col = TRUE,
                        use_units = "name") %>% 
  addHtmlTableStyle(pos.caption = "bottom") %>% 
  htmlTable(caption  = "Basic descriptive statistics from the mtcars dataset",
            tfoot = "&dagger; The weight is in 10<sup>3</sup> kg")

## ---- warning=FALSE-----------------------------------------------------------
mtcars %>% 
  getDescriptionStatsBy(mpg, 
                        wt,
                        am,
                        gear,
                        col,
                        by = am,
                        continuous_fn = describeMedian,
                        digits = 0,
                        header_count = TRUE,
                        statistics = TRUE) %>% 
  htmlTable(caption  = "Basic descriptive statistics from the mtcars dataset")

## ---- warning=FALSE-----------------------------------------------------------
mtcars %>% 
  getDescriptionStatsBy(mpg, 
                        wt,
                        am,
                        gear,
                        col,
                        by = am,
                        continuous_fn = describeMedian,
                        digits = 0,
                        header_count = TRUE,
                        statistics = list(continuous = getPvalChiSq, 
                                          factor = getPvalChiSq, 
                                          proportion = getPvalFisher)) %>% 
  addHtmlTableStyle(pos.caption = "bottom") %>% 
  htmlTable(caption  = "P-values generated from a custom set of values")

## -----------------------------------------------------------------------------
getTable1Stats <- function(x, digits = 0, ...){
  getDescriptionStatsBy(x = x, 
                        by = mtcars$am,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,
                        ...)
  
}

t1 <- list()
t1[["Gas"]] <-
  getTable1Stats(mtcars$mpg)
  
t1[["Weight&dagger;"]] <-
  getTable1Stats(mtcars$wt)

t1[["Color"]] <- 
  getTable1Stats(mtcars$col)

# If we want to use the labels set in the beginning
# we add an element without a name
t1 <- c(t1,
        list(getTable1Stats(mtcars$gear)))

mergeDesc(t1,
          htmlTable_args = list(caption  = "Basic descriptive statistics from the mtcars dataset",
                                tfoot = "&dagger; The weight is in 10<sup>3</sup> kg"))

