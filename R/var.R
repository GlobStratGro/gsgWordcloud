# functions used but not maintained
myBind <- function(x, left = 1, right = length(x), type = "df.row") {
    # execute helper function from top of data
    if (type == "vec") {
        return(.myBindHelper(x, left = left, right = right, type = type))
    }

    return(data.frame(.myBindHelper(x, left = left,
                                    right = right,
                                    type = type)))
}

.cleanEncoding <- function(txt) {
    txt <- gsub("[^[:print:]]", "", txt)
    txt <- stringr::str_replace_all(txt, "[^[:graph:]]", " ")
    return(txt)
}

.natoempty <- function(test) {
    if (is.character(test) | sum(is.na(test)) == length(test)) {
        return(ifelse(is.na(test), "", test))
    } else {
        return(test)
    }
}


.myBindHelper <- function(x, left, right, type) {
    if (type == "vec") {
        binder <- c
        if (!is.vector(x[[1]])) {
            if  (1 %in% dim(x[[1]]) & length(dim(x[[1]])) <= 2) {
                x <- lapply(x, function(y) {
                    if (dim(x[[1]])[1] == 1) {
                        y <- as.vector(y)
                        names(y) <- NULL
                        return(y)
                    } else {
                        y <- as.vector(t(y))
                        names(y) <- NULL
                        return(y)
                    }

                })
            } else {
                stop("Not a vector and not coercable to a vector.")
            }

        }
    } else if (type == "df.row") {
        binder <- rbind
    } else if (type == "df.col") {
        binder <- cbind
    } else {
        stop("Improper binding method.")
    }

    return(.recHelp(x, left, right, binder))
}

.recHelp <- function(x, left, right, binder) {
    if (left == right) {
        return(x[[left]])
    }

    mid <- floor((left + right) / 2)
    return(binder(.recHelp(x, left, mid, binder),
                  .recHelp(x, mid + 1, right, binder)))
}


myBindVec <- function(x, left = 1, right = length(x)) {
    # recursive function: implements "divide and conquer" merge algorithm
    myBind(x, left, right, "vec")
}

facToNum <- function(x) {
    as.numeric(as.character(x))
}

`%!in%` <- function(LHS, RHS) {
    return(!(LHS %in% RHS))
}

findLastWord <- function(string) {
    y <- trimws(string)
    ll <- gregexpr(" ", y)
    accum <- mapply(function(ll, y) {
        lastSpace <- ll[length(ll)]
        if (!(lastSpace %in%  c(0, -1))) {
            substr(y, start = lastSpace + 1, stop = nchar(y))
        } else {
            y
        }
    },
    ll,
    y)

    # accum is a matrix; diagonal is where both indeces are the same
    return(accum)
}



capitalize <- function(string, all = F) {
    # capitalize all words
    if (all) {
        # apply helper
        sapply(string, .help, USE.NAMES = F)

    } else { # capitalize first word (default)
        substr(string, 1, 1) <- toupper(substr(string, 1, 1))
        string
    }
}


.help <- function(string) {
    string.split <- unlist(strsplit(string, split = " "))
    capitalize(string.split) %>%
        paste(collapse = " ")
}


collapseWS <- function(str) {
    if (FALSE %!in% (gsub("  ", " ", str) == str)) {
        return(str)
    }
    return(collapseWS(gsub("  ", " ", str)))
}



nwordsfun <- function(str) {
    str <- trimws(str) %>% collapseWS()
    if (str == "") {
        return(0)
    }

    n <- gregexpr("\\s", str)[[1]]

    if (n[1] == -1) {
        return(1)
    }

    length(n) + 1

}

prev <- function() {
    return(list(keep = c("dont"),
                replace = c("don't")))
}


read.csv3 <- function(...) {
    temp <- read.csv(...)
    for (col in colnames(temp)) {
        temp[,col] <- .natoempty(temp[,col])
    }
    return(temp)
}
