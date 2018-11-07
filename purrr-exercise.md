---
title: "Data Wrangling Wrapup"
author: Shreeram Senthivasan
output: github_document
---



# Intro

For this assignment, we will be using the `purrr` package to more efficiently wrangle data. More specifically we were asked to choose two of six tasks to complete... but I was only really interested in one of the six tasks. So, I chose to break form and do a deeper dive into a single task. While I recognize this might just seem like laziness, I hope by the end of this report you will agree it was still done in the spirit of the assignment.

# Task 2: Writing Functions

## Background

In my research I will be dealing with messy animal behaviour and performance metrics that could depend on any number of other factors. While the goal is of course to design efficient experiments that address targeted questions, it can be a little overwhelming to make targetted hypotheses in the first place. Luckily, I've got access to some large datasets that include some of the variables I am interested in. Accordingly, I've gotten into using AIC scores and weights to fit many different models to these datasets to help plan my future experiments.

"But now hold on a minute," you might be thinking. "That sounds a whole lot like data dredging!" And you would be right, my hypothetical strawman audience, you. But the important thing is that I only plan to use this data dredging to generate hypotheses, not to make formal inferences. Once I've got a sense of explanatory variables that might be interesting, I can design targetted experiments to test my smaller set of hypotheses.

But now that I've got that disclaimer out of the way, I imagine some of you might be wondering, "What's data dredging, and what's so bad about it anyway?" Unfortunately getting into that might be a bit beyond the scope of this assignment, so I'll just point you towards [this](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1124898/) great primer and epidemeology case study by George Smith and Shah Ebrahim.

Anyway, I thought I would make a couple functions that make this sort of analysis easier to do since I find myself doing it to every new dataset I get a hold of.

## The Functions

The first thing I like to do is build a named list of formulas to analyse. However, I don't see an obvious way of automating the construction since I basically never want to look at every possible combination of explanatory variables. So at least for now, I will start at the next step which is building model objects. But even before that, let's build some utility functions that will be useful later.

### Utility Functions

So it turns out a lot of the functions I wrote have an eclectic mix of dependencies, so rather than checking them every time and writing a relevant error message, let's functionize it!


```r
# Attempt to load a given package and return an error message if not installed
check_dep <- function(package_name){
  tryCatch(
    library(package_name, character.only = T),
    error = function(e)
      paste0("This function requires the `", package_name, "` package to be installed. Please install it with: `install.packages('", package_name, "')` and try again!") %>%
      stop()
    )
}
```

This function takes the package name as an character argument and attempts to load it. The function also returns a more friendly error in case the package is not available. Unforunately, I couldn't find a way to intercept the error thrown by `library` for a missing package without using a `tryCatch` statement, which is pretty clunky. Notice that we don't need any error checking beyond this since this function isn't intended to be used by the end user, so we have full control of what gets passed in.

Also, I also end up with a number of named lists that I would like to print. While the `print` function handles named lists pretty well, I would like to change the surrounding padding of empty lines and prefer the markdown-style header tags for separating entries. So let's build a function for that too!


```r
print_list <- function(list_to_print){
  map2(list_to_print, names(list_to_print), function(x,y){
    cat("## ")
    cat(y)
    print(x)
    cat("\n")
    }
  )
  return()
}
```

Another thing I was surprised to find myself doing a lot was double checking that the user input a valid list of model objects into many of my functions. Accordingly, I built a function to check this return a friendly message. Unfortunately, I can't simply check the class of the input since each type of model has its own class name. Accordingly, I once again took to the clunky solution of `tryCatch` functions.


```r
check_models <- function(code){
  tryCatch(
    result <- code,
    error = function(e)
      stop("Something went wrong! Are you sure you provided a list of valid model objects?")
  )

  return(result)
}
```

### Build Models

Now let's get right to it! Let's build a list of models given a list of formulas.


```r
build_model_objects <- function(formulas, data, model = lm, ...){
  # Check that `purrr` is installed and loaded for the `map` family of functions
  check_dep('purrr')

  # Check that both the arguments without defaults were provided
  if(missing(formulas) | missing(data))
    stop("Please provide a list of formulas and a dataset")

  # Check that every element of the formula list is a valid formula
  if(!all(map_chr(formulas, class) == "formula"))
    stop("You didn't enter a valid list of formulas.")

  # Unfortunately I also don't know how to check if the formulas are specifically valid for the given dataset. I use `tryCatch` to handle this and other problems
  result <- tryCatch(
    map(formulas, model, ..., data = data),# Create and return the list of model objects
    error = function(e){ # Return an error if the model cannot be constructed
      print(e) # Print the specific error
      stop("One or more of the formulas, dataset, and model are incompatible. Please double check that:\n
            - there are no typos in the formula
            - the correct dataset was provided
            - the model function returns a model object
            - the package with the model was loaded
            - all the mandatory arguments are provided for the model")
    }
  )

  return(result)
}
```

Importantly, the function takes the list of formulas and the dataset with no defaults to build the models. By default, it will fit a linear model with `lm`, but the function should work with most functions that return a model object, such as generalized linear models with `glm` and mixed effect models with `lmerTest::lmer`. Since different models take different arguments, I use the elipses to generalize the model creation.

The function makes considerable use of the `map` family of functions, so I use the `library` function with the `return.logical` argument to ensure it is installed and loaded. I also do some other basic error checking on the arguments passed into the function.

### Summarize Models

Great! Now that we've built our models, one of the first things I like to do is get a quick tabular run down of *p*-values, parameter estimates, and confidence intervals for the parameters. Let's streamline this as well.


```r
summarize_models <- function(models, ...){
  check_dep("purrr")
  check_dep("car") # I like the `Anova` function for the choice of Sum of Square types
  check_dep("broom") # for tidy outputs in tables
  check_dep("knitr") # for pretty tables

  check_models({
    # Print out a nice table of anova outputs
    cat("# Anova tables: \n")
    models %>%
      map(Anova, ...) %>%
      map(tidy) %>%
      map(kable) %>%
      print_list

    # Print out a nice table of confidence intervals
    cat("# Confidence Intervals for Coefficients: \n")
    models %>%
      map(confint) %>%
      map(kable) %>%
      print_list
  })

  # Don't return anything at the end
  invisible()
}
```

Note the use of `check_dep` again. It was definitely worth functionizing! I also make use of `print_list` for the first time here to clean up the the lists of tables that are produced. The actual generation of the tables are also wrapped in a call of `check_model` in case the provided list of models is not valid. Finally, I use `invisible` at the end of the function since I do not want anything to be returned by this function.

### Predictions

Next, how about a tidied tibble of predicted variables? We can bind the rows together with an added column naming the model it comes from to easily plot the predictions with `ggplot2`.


```r
tidy_predictions <- function(models){
  check_dep("broom") # for `augment` function
  check_dep("purrr")
  check_dep("dplyr") # for bind_rows

  check_models(
    models %>%
      map(augment) %>%
      map2(names(.), ~ mutate(.x, model_name = .y)) %>%
      bind_rows()
  )
}
```

Turns out I need the `tryCatch` statements to check my input variables a lot. I probably should have made a function of that as well. Or perhaps that would be overkill? Also, nocice that I have to pass both the list of augmented tibbles and the names of this list to the `map2` function in order to create a column of model names. This helps me distinguish when model ends and the next begins after I bind the list of tibbles together in the next step. For a more concrete example of why this is more useful than a list of tibbles, see my worked example below.

### AIC

Now we're at the meat of the problem: AIC scores. Afterall, what I'm hoping to get out of this whole exercise is a sense of which explanatory variables are worth exploring in more detail.


```r
summarize_aic <- function(models){
  check_models(
    tibble(
      model = names(models),
      aic = map_dbl(models, AIC),
      delta_aic = aic - min(aic),
      likelihood = exp(-0.5 * delta_aic),
      aic_weight = likelihood / sum(likelihood)
      )
  )
}
```

This is a pretty straight forward



```r
example_formula <- list(pop = lifeExp ~ pop,
                gdp = lifeExp ~ gdpPercap,
                year = lifeExp ~ year)

models <- build_model_objects(example_formula, gapminder)

summarize_models(models)
```

# Anova tables: 
## pop

|term      |      sumsq|   df| statistic|   p.value|
|:---------|----------:|----:|---------:|---------:|
|pop       |   1198.879|    1|  7.211505| 0.0073141|
|Residuals | 282949.505| 1702|        NA|        NA|

## gdp

|term      |     sumsq|   df| statistic| p.value|
|:---------|---------:|----:|---------:|-------:|
|gdpPercap |  96813.03|    1|  879.5766|       0|
|Residuals | 187335.35| 1702|        NA|      NA|

## year

|term      |     sumsq|   df| statistic| p.value|
|:---------|---------:|----:|---------:|-------:|
|year      |  53919.18|    1|  398.6047|       0|
|Residuals | 230229.20| 1702|        NA|      NA|

# Confidence Intervals for Coefficients: 
## pop

|            |    2.5 %|   97.5 %|
|:-----------|--------:|--------:|
|(Intercept) | 58.60447| 59.87649|
|pop         |  0.00000|  0.00000|

## gdp

|            |      2.5 %|     97.5 %|
|:-----------|----------:|----------:|
|(Intercept) | 53.3377428| 54.5733790|
|gdpPercap   |  0.0007143|  0.0008155|

## year

|            |        2.5 %|       97.5 %|
|:-----------|------------:|------------:|
|(Intercept) | -649.0314652| -522.2729096|
|year        |    0.2938872|    0.3579204|

```r
tidy_predictions(models)
```

# A tibble: 5,112 x 12
   lifeExp    pop .fitted .se.fit .resid    .hat .sigma .cooksd .std.resid
     <dbl>  <int>   <dbl>   <dbl>  <dbl>   <dbl>  <dbl>   <dbl>      <dbl>
 1    28.8 8.43e6    59.3   0.319  -30.5 6.10e-4   12.9 1.71e-3      -2.37
 2    30.3 9.24e6    59.3   0.318  -29.0 6.08e-4   12.9 1.54e-3      -2.25
 3    32.0 1.03e7    59.3   0.317  -27.3 6.06e-4   12.9 1.36e-3      -2.12
 4    34.0 1.15e7    59.3   0.317  -25.3 6.04e-4   12.9 1.16e-3      -1.96
 5    36.1 1.31e7    59.3   0.316  -23.3 6.01e-4   12.9 9.79e-4      -1.80
 6    38.4 1.49e7    59.4   0.315  -20.9 5.98e-4   12.9 7.88e-4      -1.62
 7    39.9 1.29e7    59.3   0.316  -19.5 6.01e-4   12.9 6.88e-4      -1.51
 8    40.8 1.39e7    59.4   0.316  -18.5 6.00e-4   12.9 6.20e-4      -1.44
 9    41.7 1.63e7    59.4   0.315  -17.7 5.96e-4   12.9 5.62e-4      -1.37
10    41.8 2.22e7    59.4   0.313  -17.7 5.90e-4   12.9 5.53e-4      -1.37
# ... with 5,102 more rows, and 3 more variables: model_name <chr>,
#   gdpPercap <dbl>, year <int>

```r
summarize_aic(models)
```

# A tibble: 3 x 5
  model    aic delta_aic likelihood aic_weight
  <chr>  <dbl>     <dbl>      <dbl>      <dbl>
1 pop   13553.      703.  2.61e-153  2.61e-153
2 gdp   12850.        0   1.00e+  0  1.00e+  0
3 year  13202.      351.  5.14e- 77  5.14e- 77
