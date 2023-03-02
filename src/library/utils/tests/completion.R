
## test some typical completion attempts

testLine <- function(line, cursor = nchar(line))
{
    str(utils:::.win32consoleCompletion(line, cursor))
}

testLine("")

testLine("lib")
testLine("data(")
testLine("data(US")
testLine("data(US", 3)

testLine("?INS")

testLine("utils::data")
testLine("utils:::.show_help_on_topic_")
testLine("utils::.show_help_on_topic_")

testLine("update(")

testLine("version$m")
testLine("nchar(version[")



testLine("method?coe")
testLine("?coe")
testLine("?\"coerce,AN")
testLine("method?\"coerce,AN")


## testLine("")
## testLine("")
## testLine("")

odd_named_list <- list(a = 1, b = 2, `b c` = 3, 4, 5)
names(odd_named_list)[4L] <- "`\\`"
checkDollarSuggestions <- function(text, completions) {
  spl <- utils:::specialOpLocs(text)
  prefix <- sub("[$][^$]*$", "$", text)
  actual <- utils:::specialCompletions(text, spl)
  stopifnot(startsWith(actual, prefix))
  actual <- substring(actual, nchar(prefix) + 1L)
  stopifnot(identical(actual, completions))
}
checkDollarSuggestions("odd_named_list$a", "a")
checkDollarSuggestions("odd_named_list$b", c("b", "`b c`"))
checkDollarSuggestions("odd_named_list$`", R"(`\`\\\``)")
checkDollarSuggestions("odd_named_list$", c("a", "b", "`b c`", R"(`\`\\\``)", ""))

