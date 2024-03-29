replacePunct <- function(txt) {
    txt <- gsub("/|,", " / ", txt)
    txt <- gsub("\\(|\\)", " ", txt)
    txt <- .cleanEncoding(txt)
    out <- removePunctuation(txt,
                             preserve_intra_word_contractions = TRUE,
                             preserve_intra_word_dashes = TRUE)
    return(collapseWS(out) %>% trimws())
}

punctReplace <- function(txt) {
    replacePunct(txt)
}

.generateMatchPattern <- function(word) {
    paste0("\\b",
           word,
           "(?!'|-|[[:alnum:]])\\b")
}

.replaceHelper <- function(txt, word, replacement) {
    return(gsub(.generateMatchPattern(word), replacement, txt, perl = T))
}

spellReplaceHelper <- function(txt, bad, good,
                               keepWords = NULL,
                               replaceWords = NULL,
                               replacements) {
    if (length(keepWords) != length(replaceWords)) {
        stop(paste0("ERROR: The forced keep words",
                    " and forced replace words must",
                    " be the same length."))
    }

    if (length(bad) == 0) { # base case
        return (list(text = txt,
                     replacements = replacements))
    } else {
        if (bad[length(bad)] %in% keepWords) {
            #txt[txt == bad[length(bad)]] <- replaceWords[keepWords == bad[length(bad)]]
            txt <- .replaceHelper(txt,
                                  word = bad[length(bad)],
                                  replacement = replaceWords[keepWords == bad[length(bad)]])
            if (bad[length(bad)] != replaceWords[keepWords == bad[length(bad)]]) {
                replacements[dim(replacements)[1] + 1,] <- c(bad[length(bad)], replaceWords[keepWords == bad[length(bad)]])
            }
        } else {
            if (!is.na(good[length(bad)])) {
                txt <- .replaceHelper(txt,
                                      word = bad[length(bad)],
                                      replacement = good[length(bad)])
                #txt[txt == bad[length(bad)]] <- good[length(bad)]
                replacements[dim(replacements)[1] + 1,] <- c(bad[length(bad)], good[length(bad)])
            }
        }


        return(spellReplaceHelper(txt,
                                  bad[-length(bad)],
                                  good,
                                  keepWords = keepWords,
                                  replaceWords = replaceWords,
                                  replacements = replacements))
    }
}

customReplace <- function(txt, keepWords, replaceWords, replacements) {
    idx <- sapply(keepWords, function(w) {
        return(sum(grepl(.generateMatchPattern(w),
                         txt,
                         perl = T),
                   na.rm = T) > 0)
        }, USE.NAMES = F)
    if (sum(idx) > 0) {
        keepWords <- keepWords[idx]
        replaceWords <- replaceWords[idx]
        for (i in 1:length(keepWords)) {
            #txt[txt == keepWords[i]] <- replaceWords[i]
            txt <- .replaceHelper(txt,
                                  word = keepWords[i],
                                  replacement = replaceWords[i])
            replacements[dim(replacements)[1] + 1, ] <- c(keepWords[i], replaceWords[i])
        }
    }
    return(list(text = txt, replacements = replacements))
}

#' Replace all misspelled words with either the suggested replacement or the specified replacement
#'
#' @param txt a character or character vector to be spell checked
#' @param keepWords a character vector of words to not replace with the inferred best word
#' @param replaceWords a character vector of the same length as \code{keepWords} of the user-defined replacements for the given words
#' @param spellcheck logical, defaults to \code{TRUE}. If \code{TRUE}, does an automatic spell check with hunspell. If \code{FALSE}, makes only the user entered replacements.
#' @return A list with two fields: \code{text}, a character or character vector with only single spaces, and \code{replacements}, a dataframe documenting each word replaced and what it was replaced by.
#' @note This will sometimes remove some/all punctuation and other times may add punctuation.
spellReplace <- function(txt, keepWords = NULL, replaceWords = NULL, spellcheck = TRUE) {
    replacements <- data.frame(word = character(0),
                               replacement = character(0),
                               stringsAsFactors = F)

    txt <- .cleanEncoding(txt)
    keepWords <- .cleanEncoding(keepWords)
    replaceWords <- .cleanEncoding(replaceWords)

    #txt <- na.omit(txt)
    txt <- tolower(txt)
    keepWords <- tolower(keepWords)
    replaceWords <- tolower(replaceWords)

    d <- duplicated(keepWords, fromLast = TRUE)
    if (sum(d) > 0) {
        warning(paste0("Found ",
                       sum(d),
                       " duplication(s) in keepWords -- using latest entries"))
        keepWords <- keepWords[!d]
        replaceWords <- replaceWords[!d]
    }

    txt <- replacePunct(txt)
    #txt <- paste(na.omit(txt), collapse = " ")

    if (spellcheck) {
        bad <- hunspell::hunspell(txt) %>% unlist() %>% unique()
        good <- hunspell::hunspell_suggest(bad)
        good <- sapply(good, function(x) {x[1]}, USE.NAMES = F)
    } else {
        bad <- c()
        good <- c()
    }

    #txt <- strsplit(txt, split = "\\s") %>% unlist()

    ngroups <- ceiling(length(bad) / 200)

    idx <- prev()$keep %!in% keepWords
    keepWords <- c(keepWords, prev()$keep[idx])
    replaceWords <- c(replaceWords, prev()$replace[idx])

    out <- list(text = txt, replacements = replacements)
    if (length(bad) > 0) {
        for (i in 0:(ngroups - 1)) {
            out <- spellReplaceHelper(out$text,
                                      bad[1:length(bad) %% ngroups == i],
                                      good[1:length(good) %% ngroups == i],
                                      keepWords = keepWords,
                                      replaceWords = replaceWords,
                                      replacements = out$replacements)
            if (ngroups > 1) {
                cat(paste0(round(100 * i / ngroups), "%", "\n"))
            }
        }
    }

    out <- customReplace(out$text, keepWords, replaceWords, out$replacements)
    #out$text <- paste(out$text, collapse = " ") %>% tolower()
    return(out)
}
