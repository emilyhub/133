stat_range <- function(x) {
  max(x) - min(x)
}

stat_centers <- function(x) {
  return (c(median(x), mean(x)))
}

stat_spreads <- function(x) {
  return (c(stat_range(x), IQR(x), sd(x)))
}

states <- rownames(USArrests)
num_chars <- nchar(states)
char_freqs <- table(num_chars)
char_freqs
plot(char_freqs)

tolower(states)
toupper(states)
casefold(states, upper=TRUE)
casefold(states, upper=FALSE)

paste(head(states), head(num_chars), sep = " ")
paste(head(states), collapse = "")

substr(states, 1, 3)
substr(states, -1, 3) #Unsure how to get last 3
paste(substr(states, 1, 1), substr(states, -1, 3))

challenge <- function(n) {
  states_list <- c()
  if (n %% 2 == 0) {
    for (i in states) {
      if (len(i) == n) {
        states_list <- c(states_list, toupper(i))
      }
    }
  } else {
    for (i in states) {
      if (len(i) == n) {
        states_list <- c(states_list, i)
      }
    }
  }
}

temp_convert1 <- function(x = 1, to = "celsius") {
  switch(to,
         "celsius" = to_celsius(x),
         "kelvin" = to_kelvin(x),
         "reaumur" = to_reaumur(x),
         "rankine" = to_rankine(x))
}

temp_convert <- function(x, to) {
  return (temp_convert1(x, tolower(to)))
}

#Names of files
way1 <- c("file1.csv", "file2.csv", "file3.csv", "file4.csv", "file5.csv", "file6.csv",
          "file7.csv","file8.csv", "file9.csv", "file10.csv")
way2 <- c("sheet1.csv", "sheet2.csv", "sheet3.csv", "sheet4.csv", "sheet5.csv", "sheet6.csv",
          "sheet7.csv","sheet8.csv", "sheet9.csv", "sheet10.csv")
way3 <- c("doc1.csv", "doc2.csv", "doc3.csv", "doc4.csv", "doc5.csv", "doc6.csv",
          "doc7.csv","doc8.csv", "doc9.csv", "doc10.csv")

rename <- function(x) {
  replace(x, x=='file', 'dataset')
}

# name of output file
outfile <- "output.txt"

# writing to 'outfile.txt'
cat("title: Modify", file = outfile)
cat("\n", file = outfile, append = TRUE)
cat("author: Emily Lan", file = outfile, append = TRUE)
cat("\n", file = outfile, append = TRUE)
cat("Date: 3/27/2018", file = outfile, append = TRUE)
cat("\n", file = outfile, append = TRUE)
cat("Output: html_document", file = outfile, append = TRUE)
cat("\n", file = outfile, append = TRUE)
cat("This is the first line", file = outfile, append = TRUE)
# insert new line
cat("\n", file = outfile, append = TRUE)
cat("A 2nd line", file = "output.txt", append = TRUE)
# insert 2 new lines
cat("\n\n", file = outfile, append = TRUE)
cat("\nThe quick brown fox jumps over the lazy dog\n",
    file = outfile, append = TRUE)


is_color <- function(x) {
  return (is.element(x, colors()))
}
is_color('yellow')  # TRUE

is_color('blu')     # FALSE

is_color('turkuoise') # FALSE

colplot <- function(col) {
  if (is_color(col)) {
    plot(x <- rnorm(100, 2, 3))
  } else {
    stop(paste("invalid color", col))
  }
}


set.seed(1)
letrs <- sample(letters, size = 100, replace = TRUE)
dt <- table(letrs)
dt[c(1, 5, 9, 15, 21)]
dt[c(2, 3, 4, 6, 7, 8, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 22, 23, 24, 25, 26)]

count_letters <- function(letrs) {
  dt <- table(letrs)
  print(len(letrs))
  print(sum(dt[c(1, 5, 9, 15, 21)]))
  print(sum(dt[c(2, 3, 4, 6, 7, 8, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 22, 23, 24, 25, 26)]))
}