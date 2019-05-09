library(rlang)
library(glue)

setwd('..')

solution <- new.env()
source('tests/solution.R', local = solution)

user_source_file <- 'reading.R'

user <- new.env()
source(user_source_file, local = user)

parsed <- parse_exprs(file(user_source_file))

for (line in parsed) {
  if(line[[1]] == 'plot' && line[[2]] == 'p') {
    plot_call <- TRUE
  }
}

context('Module 01')
test_that('@arrange_by_download', {
})

test_that('@select_relevant_variables', {
  expect('titles' %in% ls(envir = user), glue('Does the `titles` data frame exist in `{user_source_file}`?'))
})

test_that('@top_ten_authors', {
})

test_that('@filtering', {
})

test_that('@mutate_flesch_reading_ease', {
})

test_that('@mutate_flesch_reading_grade_level', {
})

test_that('@grouping_by_author', {
})

test_that('@summarising_reading_statistics', {
})

test_that('@gather', {
})

test_that('@gather', {
})

test_that('@initialize_plot', {
})

test_that('@faceting_bar_plot', {
})

test_that('@customizing_plot', {
})