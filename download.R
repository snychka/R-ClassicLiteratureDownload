library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# https://swcarpentry.github.io/r-novice-inflammation/02-func-R/
unique_books <- function(data, column='title') {
  # a bit tough for me.  hints in test and task name (2.5)
  items <- pull(data, column)
  duplicates <- list()
  # https://stackoverflow.com/questions/26297024/how-to-loop-through-a-list-in-r
  for (item in items) {
    match <- agrep(item, items)
    last <- match[-1]
    if (length(last)) {
      duplicates[[last]] <- last
    }
  }
  
  # https://stackoverflow.com/questions/17094774/better-way-to-convert-list-to-vector
  # remove <- unique(unlist(duplicates, use.names = FALSE))
  remove <- unique(unlist(duplicates))
  # guessed data - remove , which was wrong
  # https://stackoverflow.com/questions/4835342/how-can-i-get-a-dataframe-with-columns-temporarily-removed-by-name
  data[-remove,]
}

books <- read_csv('data/books.csv')

# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/filter
twain <- filter(books, author == 'Twain, Mark')
# maybe https://stackoverflow.com/questions/22850026/filtering-row-which-contains-a-certain-string-using-dplyr
# twain <- filter(books, str_detect(author, 'Twain'))

# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8
twain_refined <- twain %>% select(title, author, downloads, avg_words_per_sentence, sentences)
# https://dplyr.tidyverse.org/reference/arrange.html
twain_by_download <- arrange(twain_refined, desc(downloads))

twain_unique <- unique_books(twain_by_download)

p <- ggplot(twain_unique, aes(sentences, avg_words_per_sentence)) + geom_point(aes(size=downloads))
plot(p)





