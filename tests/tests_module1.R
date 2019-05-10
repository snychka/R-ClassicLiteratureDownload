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
test_that('@find-twains-works', {
})

test_that('@select-relevant-columns', {
})

test_that('@arrange-by-download', {
})

test_that('@create-function', {
})

test_that('@pull', {
})

test_that('@empty-list', {
})

test_that('@for-loop', {
})

test_that('@fuzzy-matching', {
})

test_that('@add-list', {
})

test_that('@remove-duplicates', {
})

test_that('@call-function', {
})

test_that('@initialize-plot', {
})

test_that('@add-component', {
})

test_that('@aesthetic-mappings', {
})

test_that('@geom-aesthetic-mappings', {
})
