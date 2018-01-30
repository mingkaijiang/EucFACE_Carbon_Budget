# function to count number of days in between the dates
count_ndays <- function(d) {
    
    # d: list of dates in the input dataframe
    # f: number of days difference from the first date
    # b: number of days in between the dates
    
    f <- c()
    
    for (i in seq_along(d))
    {
        f[i] <- d[i] - d[1]
    }
    
    b <- c(0, diff(d))
    
    return(b)
}
