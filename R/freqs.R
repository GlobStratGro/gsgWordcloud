# no stem corpus (stopwords kept)
noStemCorp <- function(txt) {
    return(Corpus(VectorSource(txt)))
}

# no stem corpus (stopwords removed)
noStemCorpClean <- function(txt) {
    corp.nostem <- noStemCorp(txt)
    return(suppressWarnings(tm_map(corp.nostem, removeNumbers) %>%
                                tm_map(removeWords, stopwords("english")) %>%
                                tm_map(removePunctuation,
                                       preserve_intra_word_contractions = TRUE,
                                       preserve_intra_word_dashes = TRUE) %>%
                                tm_map(stripWhitespace)))
}

# unstemmed frequencies
noStemFreq <- function(txt) {
    corp.nostem1 <- noStemCorp(txt)

    dtm.nostem <- TermDocumentMatrix(corp.nostem1) %>% as.matrix()
    agg <- sort(rowSums(dtm.nostem), decreasing = T)

    freqs.nostem <- data.frame(words = names(agg),
                               freq = agg)
    freqs.nostem$words <- as.character(freqs.nostem$words)
    return(freqs.nostem)
}

stemCorp <- function(txt) {
    return(suppressWarnings(tm_map(noStemCorpClean(txt), stemDocument)))
}

# creates frequencies of stemmed words
freqStems <- function(txt) {
    # create stemmed version
    corp.stem <- stemCorp(txt)

    # now, construct our frequency table with stemming
    dtm.stem <- TermDocumentMatrix(corp.stem)
    dtm.stem <- as.matrix(dtm.stem)

    # sum total uses and convert to data frame
    agg <- sort(rowSums(dtm.stem), decreasing = T)
    return(data.frame(words = names(agg),
                      freq = agg,
                      stringsAsFactors = F))
}

stemMap <- function(txt) {
    txt.nostem <- sapply(noStemCorpClean(txt), function(x) x[[1]])
    txt.stem <- sapply(stemCorp(txt), function(x) x[[1]])

    # now, we want to match the stemmed words to where they came from
    words.nostem <- trimws(txt.nostem) %>% strsplit(split = " ") %>% unlist()
    words.stem <- trimws(txt.stem) %>% strsplit(split = " ") %>% unlist()

    # matches each word to its stem
    stemMap <- data.frame(word = tolower(words.nostem),
                          stem = tolower(words.stem))

    # remove duplicated words (not stems)
    return(stemMap[!duplicated(stemMap$word),])
}


# helper
fun <- function(...) {
    paste(..., sep = ",", collapse = ",") %>% tolower()
}

# now we have a list of stems along with all the words they came from


stemToAllWords <- function(txt) {
    stem.map <- stemMap(txt)
    stem.allwords <- aggregate(stem.map$word, by = list(stem.map$stem), FUN = fun)
    colnames(stem.allwords) <- c("stem", "sources")
    stem.allwords$stem <- as.character(stem.allwords$stem)
    stem.allwords$sources <- as.character(stem.allwords$sources)
    return(stem.allwords)
}


# now we can go down our list of stems and find the best source word for each!
findBestWords <- function(txt) {
    freqs.nostem <- noStemFreq(txt)
    stem.allwords <- stemToAllWords(txt)
    stem.allwords$best.source <-
        sapply(stem.allwords$sources,
               function(words) {
                   check <- strsplit(words, split = ",") %>% unlist() %>% tolower()
                   d <- freqs.nostem[match(check, tolower(freqs.nostem$words)),]
                   d <- d[order(d$freq, decreasing = T),]
                   d$words[1]
               },
               USE.NAMES = F)
    return(stem.allwords)
}

aggFun <- function(x) {
    if (is.numeric(x)) {
        return(sum(x, na.rm = T))
    } else {
        out <- paste(x, sep = ",", collapse = ",")
        while(grepl(",,", out)) {
            out <- gsub(",,", ",", out)
        }
        return(out)
    }
}

freqForWordCloud <- function(txt,
                             mergekeep = NULL, mergereplace = NULL,
                             bestkeep = NULL, bestreplace = NULL) {
    if (length(mergekeep) != length(mergereplace)) {
        stop(paste0("ERROR: The forced merge keep words",
                    " and forced merge replace words must",
                    " be the same length."))
    }

    if (length(bestkeep) != length(bestreplace)) {
        stop(paste0("ERROR: The forced best source keep words",
                    " and forced best source replace words must",
                    " be the same length."))
    }

    for (word in mergekeep) {
        pattern <- paste0("\\b", word, "\\b")
        replacement <- paste0("donotmergeword", word, "donotmergeword")
        txt <- gsub(pattern, replacement, txt)
    }

    df <- freqStems(txt)
    stem.allwords <- findBestWords(txt)
    df <- merge(df[,c("words", "freq")], stem.allwords,
                by.x = "words", by.y = "stem", all.x = T)
    df$best.source[is.na(df$best.source)] <- df$words[is.na(df$best.source)]
    colnames(df)[colnames(df) == "words"] <- "stem"

    df$stem <- gsub("donotmergeword", "", df$stem)
    df$sources <- gsub("donotmergeword", "", df$sources)
    df$best.source <- gsub("donotmergeword", "", df$best.source)

    for (word in mergekeep) {
        df$best.source[df$sources == word] <- mergereplace[mergekeep == word]
    }

    df <- aggregate(df[,colnames(df) != "best.source"],
                    by = list(best.source = df$best.source),
                    FUN = aggFun)[,c(2:4, 1)]

    for (word in bestkeep) {
        df$best.source[df$best.source == word] <- bestreplace[bestkeep == word]
    }

    df <- aggregate(df[,colnames(df) != "best.source"],
                    by = list(best.source = df$best.source),
                    FUN = aggFun)[,c(2:4, 1)]

    return(df)
}

