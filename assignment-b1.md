---
output: 
  html_document:
    keep_md: true
---

# Assignment B-1: Making a function

*\~Juliana Vieira*\
2023-11-03

**Total Points**: 100

This assignment covers making a function in R, documenting it, and testing it.

## Setup

1.  Go to canvas to get your invitation to create a GitHub repository for this project. You can find this in the description of Assignment B-1. Name your repo as you wish, so long as it's informative.

2.  Make a new Rmd file containing all of your code. Be sure to use some dialogue between code chunks.

## Tidy Submission (15 points)

Follow these steps to submit your work. Be sure to familiarize yourself with the rubric for a tidy submission below, before doing these steps.

1.  Make a README file for your repository. It should be a brief document letting a visitor know what's in this repository (at a high level) and some key things they should know about how to use the files in the repository.
2.  Tag a release in your GitHub repository corresponding to your submission before the deadline.
3.  Grab the URL corresponding to your tagged release, and submit that to canvas. Make sure the TAs and Lucy can see your repository! Either it should be public or private with TAs and Lucy added as collaborators.

**Rubric**:

-   The above steps were followed.
-   Your work must be reproducible from beginning to end, error-free.
-   Code should adopt a consistent and easy-to-read style -- ideally, the [tidyverse style](https://style.tidyverse.org/), although we're certainly not looking for strict adherence.
-   You use proper English, spelling, and grammar, and write concisely. If there's any uncertainty in determining a grade here, the [UBC MDS writing rubric](https://github.com/UBC-MDS/public/blob/master/rubric/rubric_writing.md) will be referred to.
-   If there's any further uncertainty in determining a grade for this tidy submission portion, the [UBC MDS mechanics rubric](https://github.com/UBC-MDS/public/blob/master/rubric/rubric_mech.md) will be referred to.

## Exercise 1: Make a Function (25 points)

In this exercise, you'll be making a function and fortifying it. The function need not be complicated. The function need not be "serious", but shouldn't be nonsense.

**Function Ideas**

-   Did you repeat any code for a data analysis in STAT 545A? If so, consider making a function for this action.
-   Consider bundling a specific `group_by() %>% summarise()` workflow.
-   Write a *wrapper* around an existing function.
    -   For example, perhaps accepting a narrower range of inputs (like not allowing logical vectors), or providing a different output.
    -   A specific example: the [`rqdist()` function](https://github.com/vincenzocoia/rqdist/blob/master/R/rqdist.R#L1) is a wrapper around `quantreg::rq()`, narrowing its functionality.
    -   It's usually better to narrow a function's focus than to broaden, so that a function doesn't end up doing too much.
-   Make a special plot that you'd want to repeat when exploring your data.
-   ...

**Guidelines**

-   Your function should not rely on anything from your working environment.
-   Your function should not rely on "magic numbers" -- pre-selected numbers (or options) that appear inside the function that can't be accessed by a user of the function.
    -   For example, maybe `quantile(x, type = 1, ...)` appears in your function. The choice of 1 is arbitrary -- unless you're making a function like `quantile_type_1()`.
-   Your function should be reasonably flexible on its inputs. Here is an example of a function that is *not* reasonably flexible: a function called `count_missing_values_by_group()` that only works when the data frame has columns that are all of type fct.
-   The output is consistent -- for example, always gives a list. An example of inconsistent output: `sapply(1:3, seq_len)` gives a list, and `sapply(1:3, sqrt)` gives an (atomic) vector.
-   Your function includes appropriate arguments. (Do you handle NA's appropriately? Are you using the ellipsis properly? etc.)

## Exercise 2: Document your Function (20 points)

In the same code chunk where you made your function, document the function using [roxygen2 tags](https://roxygen2.r-lib.org/articles/rd-formatting.html). Be sure to include:

1.  Title.
2.  Function description: In 1-2 brief sentences, describe what the function does.
3.  Document each argument with the `@param` tag, *making sure to justify why you named the parameter as you did*.
    -   (Justification for naming is not often needed, but we want to hear your reasoning.)
4.  What the function returns, using the `@return` tag.


```r
## Load relevant packages

suppressPackageStartupMessages(library(datateachr)) # <- data for example
suppressPackageStartupMessages(library(palmerpenguins))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(testthat))
```

> **Exercise 1 & 2 : Making a new function and documenting.**
>
> Function name: ***count_events***


```r
#' Count Events 
#'
#' This function takes a data frame or Tibble, and two variable names as arguments. 
#' It performs data validation, filtering out rows with missing values.
#' It counts the number of events in the data, grouped by the two specified variables, and returns the result as a data frame.
#'
#' @param data A data frame or Tibble containing the dataset to analyze, thus data.
#' @param var1 The first variable used for grouping and event counting, thus var1.
#' @param var2 The second variable used for grouping and event counting, thus var2.
#'
#' @return A data frame containing counts of events for each unique combination of
#'   the specified variables.
#'
#' @examples
#' # Example usage:
#' my_result <- count_events(data = your_data, var1 = variable1, var2 = variable2)
#'
#'
count_events <- function(data, var1, var2) {

   # Check if data is a data frame or Tibble
  if (!is.data.frame(data) && !is(data, "tbl_df")) {
    stop("Input 'data' must be a data frame or Tibble.")
  }

   # Check if var1 is present in data
  if (!(quo_name(enquo(var1)) %in% names(data))) {
    stop("Variables 'var1' must exist in the input data.")
  }
   # Check if var2 is present in data
  if (!(quo_name(enquo(var2)) %in% names(data))) {
    stop("Variables 'var2' must exist in the input data.")
  }
  
   # Filters out any NA values
   filtered_data <- drop_na(data)
   

 #function to group by variables and return the count as a new column
    result <- filtered_data %>%
    group_by({{var1}}, {{var2}}) %>%
    summarise(count = n()) %>%
    ungroup()
    return(result)
}
```

## Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or more new code chunks, describing what you're doing.

Note: If you want to deliberately show an error, you can use `error = TRUE` in your code chunk option.

> **Examples to demonstrate de function "count_events"**
>
> *Using the penguins data*


```r
# How many penguins of each species per island in the data??

specie_by_island <- count_events(penguins, species, island)
```

```
## `summarise()` has grouped output by 'species'. You can override using the
## `.groups` argument.
```

```r
print(specie_by_island)
```

```
## # A tibble: 5 × 3
##   species   island    count
##   <fct>     <fct>     <int>
## 1 Adelie    Biscoe       44
## 2 Adelie    Dream        55
## 3 Adelie    Torgersen    47
## 4 Chinstrap Dream        68
## 5 Gentoo    Biscoe      119
```

> *Using the vancouver_tree data*


```r
# How many trees of each species by the different genus names in the data?

tress_by_species <- count_events(vancouver_trees, genus_name, species_name)
```

```
## `summarise()` has grouped output by 'genus_name'. You can override using the
## `.groups` argument.
```

```r
print(tress_by_species)
```

```
## # A tibble: 95 × 3
##    genus_name species_name   count
##    <chr>      <chr>          <int>
##  1 ACER       CAMPESTRE       1104
##  2 ACER       CAPPADOCICUM     205
##  3 ACER       FREEMANI   X    3015
##  4 ACER       NIGRUM            69
##  5 ACER       PALMATUM         480
##  6 ACER       PLATANOIDES     4298
##  7 ACER       PSEUDOPLATANUS    16
##  8 ACER       RUBRUM          3517
##  9 ACER       TEGMENTOSUM       11
## 10 ACER       TRUNCATUM       1469
## # ℹ 85 more rows
```

> *Using the parking_meters data*


```r
# How many meter head types per localization in the data?

headtype_by_location <- count_events(parking_meters, geo_local_area, meter_head)
```

```
## `summarise()` has grouped output by 'geo_local_area'. You can override using
## the `.groups` argument.
```

```r
print(headtype_by_location)
```

```
## # A tibble: 48 × 3
##    geo_local_area     meter_head          count
##    <chr>              <chr>               <int>
##  1 Arbutus-Ridge      Single Motorbike        1
##  2 Downtown           Pay Station            16
##  3 Downtown           Single                  7
##  4 Downtown           Single / Disability     1
##  5 Downtown           Single Motorbike       57
##  6 Downtown           Twin                   17
##  7 Fairview           Pay Station             2
##  8 Fairview           Single Motorbike       13
##  9 Fairview           Twin                    5
## 10 Grandview-Woodland Pay Station            19
## # ℹ 38 more rows
```

## Exercise 4: Test the Function (25 points)

Running examples is a good way of checking by-eye whether your function is working as expected. But, having a formal "yes or no" check is useful when you move on to other parts of your analysis.

Write formal tests for your function. You should use at least three non-redundant uses of an `expect_()` function from the `testthat` package, and they should be contained in a `test_that()` function (or more than one). They should all pass.

Example of non-redundant inputs:

-   Vector with no NA's
-   Vector that has NA's
-   Vector of a different type (if relevant)
-   Vector of length 0, like `numeric(0)`.

Example of redundant inputs:

-   Providing a different number (unless one of these numbers have some significance, like an extreme point -- just tell us if that's the case)

> **Write formal tests for my function "count_events" using the expect\_() functions.**
>
> ***Test 1.*** I will check if my function only accepts data frame or Tibble


```r
test_that("count_events only accepts data frame or Tibble", {
  
#make a vector to use as a test
n_vector_test <- c(1, 2, 3, 4, 5)
#make a variable to use as a test
var_test <- "not a dataset"

  
  expect_error(count_events(n_vector_test, species, island), "Input 'data' must be a data frame or Tibble.") # A vector was used instead of a data frame or tibble, thus an error is expected
  expect_error(count_events(var_test, species, island), "Input 'data' must be a data frame or Tibble.") # A single variable was used instead of a data frame or tibble, thus an error is expected
})
```

```
## Test passed 🎉
```

> ***Test 2.*** I will check if my function handles invalid inputs for var1 and var2


```r
test_that("count_events function handles invalid input", {
  
#make a variable not present in the dataset
var3 <- c(1, 2, 3, 4, 5) 

#using penguins as dataset

  
  expect_error(count_events(penguins, var3, island), "Variables 'var1' must exist in the input data.") # var3 is not in the dataset, thus an error is expected for var1
  expect_error(count_events(penguins, species, var3), "Variables 'var2' must exist in the input data.") # var3 is not in the dataset, thus an error is expected for var2
})
```

```
## Test passed 🌈
```

> ***Test 3.*** I will check if the function correctly counts events by 'var1' and 'var2'


```r
test_that("count_events function correctly counts events", {
  
  # Make a sample dataset
  data <- data.frame(var1 = c("A", "A", "B", "B", "A", "B"),
                     var2 = c("X", "Y", "X", "X", "Y", "Y"))
  
  # Call the count_events function
  result <- count_events(data, var1, var2)

  # Check if the result contains the expected counts
  expect_equal(result$count, c(1, 2, 2, 1))  #expect X:A = 1; X:B = 2; expect Y:A = 2; Y:B = 1;
})
```

```
## Test passed 🎊
```

> ***Test 4.*** I will check if the function handles NA values correctly


```r
test_that("count_events function NA test", {
  
  # Make a sample dataset
  data <- data.frame(var1 = c(NA, "A", "B", "B", "A", "B"), #added an NA in the place of "A"
                     var2 = c("X", "Y", "X", "X", NA, "Y")) #added an NA in the place of "Y"
  
  # Call the count_events function
  result <- count_events(data, var1, var2)

  # Check if the result contains the expected counts
  expect_equal(result$count, c(1, 2, 1))  #expect minus one row and minus one count for "Y"
})
```

```
## Test passed 🌈
```
