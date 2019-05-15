library(rlang)
library(glue)

setwd('..')

solution <- new.env()
source('tests/solution.R', local = solution)

user <- new.env()
source('download.R', local = user)

parsed <- parse_exprs(file('download.R'))

for (line in parsed) {
  arg_list <- call_args(line)
  
  if(is_call(line, '<-') && arg_list[[1]] == 'p') {
    right <- arg_list[[2]]
    right_call <- call_args(arg_list[[2]])
    
    if(is_call(right, 'ggplot', 0) && length(right_call) == 0) {
      ggplot_called_zero <- TRUE
    }
    
    if(length(right) >= 3) {
      if(right[[1]] == '+' && is_call(right[[2]], 'ggplot') && is_call(right[[3]], 'geom_point', 1)){
        ggplot_args <- call_args(right[[2]])
        ggplot_named_args <- call_standardise(right[[2]])
        if(is_call(ggplot_named_args$mapping, 'aes')) {
          aes_args <- call_standardise(ggplot_named_args$mapping)
        }
        geom_point_args <- call_standardise(right[[3]])
        ggplot_called_zero <- TRUE
      }
    }
  }
  
  plot_arg <- ifelse(is_call(line, 'plot', 1), arg_list, '')
}

parse_function <- function(func) {
  expr <- parse_expr(deparse(body(func)))
  for (cc in seq_along(expr)) {
    print(expr[[cc]])
  }
}
  

context('Module 01')
test_that('Find Twain\'s Works @find-twains-works', {
  expect('twain' %in% ls(envir = user), 'Does the `twain` data frame exist in `download.R`?')
  expect(isTRUE(all_equal(user$twain, solution$twain)), 'The `twain` data frame does not contain the correct data or columns.')
})

test_that('Select Relevant Columns @select-relevant-columns', {
  expect('twain_refined' %in% ls(envir = user), 'Does the `twain_refined` data frame exist in `download.R`?')
  expect(isTRUE(all_equal(user$twain_refined, solution$twain_refined)), 'The `twain_refined` data frame does not contain the correct data or columns.')
})

test_that('Arrange Books by Download @arrange-by-download', {
  expect('twain_by_download' %in% ls(envir = user), 'Does the `twain_by_download` data frame exist in `download.R`?')
  expect(isTRUE(all_equal(user$twain_by_download, solution$twain_by_download)), 'The `twain_by_download` data frame does not contain the correct data or columns.')
})

test_that('Create a Function @create-function', {
  expect(is_function(user$unique_books), 'Have you defined a function called `unique_books`?')
  defined_args <- formals(user$unique_books)
  expect(is_symbol(defined_args$data), 'Is the first parameter in the `unique_books` function definition named `data`?')
  expect(is_character(defined_args$column), 'Is the second parameter in the `unique_books` function definition named `column`?')
  expect(defined_args$column == 'title', 'Is the second parameter `column` set to `\'title\'`?')
})

test_that('Pull the Correct Column @pull', {
  expect(is_function(user$unique_books), 'Have you defined a function called `unique_books`?')
  print(parse_function(user$unique_books))
})

test_that('Create an Empty List @empty-list', {
  expect(is_function(user$unique_books), 'Have you defined a function called `unique_books`?')
  
})

test_that(' Create a For Loop @for-loop', {
  expect(is_function(user$unique_books), 'Have you defined a function called `unique_books`?')
  
})

test_that('Fuzzy Matching @fuzzy-matching', {
  expect(is_function(user$unique_books), 'Have you defined a function called `unique_books`?')
  
})

test_that('Add Elements to a List @add-list', {
  expect(is_function(user$unique_books), 'Have you defined a function called `unique_books`?')
  
})

test_that('Remove Duplicates @remove-duplicates', {
  expect(is_function(user$unique_books), 'Have you defined a function called `unique_books`?')
  
})

test_that('Call a Function @call-function', {
  
})

test_that('Initialize a Plot Object @initialize-plot', {
  expect(exists('ggplot_called_zero'), 'Was the variable `p` set to the result of a call to `ggplot()`')
  expect(plot_arg == 'p', 'Was the `plot()` function called and passed an argument of `p`?')
})

test_that('Adding a Component @add-component', {
  expect(exists('geom_point_args'), 'Was the `geom_point()` function added to `ggplot()`?')
})

test_that('Aesthetic Mappings @aesthetic-mappings', {
  expect(length(ggplot_args) != 0, 'Have you added the proper arguments to the `ggplot()` function?')
  expect(ggplot_named_args$data == 'twain_unique', 'Are you passing the `twain_unique` data frame to the `ggplot()` function?')
  expect(exists('aes_args'), 'Have you passed a call to the `aes()` function as the second argument to the `ggplot()` function?')
  expect(aes_args$x == 'sentences', 'Was the `sentences` column passed to the `aes()` function?')
  expect(aes_args$y == 'avg_words_per_sentence', 'Was the `avg_words_per_sentence` column passed to the `aes()` function?')
})

test_that('Geom Aesthetic Mappings @geom-aesthetic-mappings', {
})
