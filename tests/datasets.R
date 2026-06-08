#### Simple integrity tests of the system datasets

options(useFancyQuotes=FALSE)
env <- as.environment("package:datasets")
d <- setdiff(ls(env), "sunspot.month") # don't want .names, nor "changing" one
for(f in d) {
    cat("\n** structure of dataset ", f, "\n", sep="")
    str(get(f, envir=env, inherits=FALSE))
}
