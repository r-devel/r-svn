## A parse error after a line directive must retain its filename during GC.
local({
    old <- gctorture2(1L)
    on.exit(gctorture2(old))
    msg <- tryCatch(str2expression("#line 1 \"srcfile.R\"\n("),
                    error = conditionMessage)
    stopifnot(startsWith(msg, "srcfile.R:2:0:"))
})
