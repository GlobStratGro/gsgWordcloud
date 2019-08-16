generateWordCloudData <- function(txt,
                                  spellcheck = TRUE,
                                  combine = TRUE,
                                  name = "wordcloud_text",
                                  location = getwd(),
                                  keepWords = NULL,
                                  replaceWords = NULL,
                                  mergekeep = NULL,
                                  mergereplace = NULL,
                                  bestkeep = NULL,
                                  bestreplace = NULL) {
    txt <- spellReplace(txt, keepWords = keepWords, replaceWords = replaceWords, spellcheck)
    df <- freqForWordCloud(txt$text,
                           bestkeep = bestkeep,
                           bestreplace = bestreplace,
                           mergekeep = mergekeep,
                           mergereplace = mergereplace)


    out <- df[!is.na(df$best.source),]
    out <- paste(as.character(out$best.source), out$freq, sep = ":")

    if (grepl("/$", location)) {
        location <- substr(location, start = 1, stop = nchar(location) - 1)
    }

    dir <- paste0(location, "/", name)

    if (name %!in% list.files(location)) {
        dir.create(dir)
    }

    # write file to input to wordle
    writeLines(out, con = paste0(dir,"/cleanedOEs.txt"))

    # write file to view spelling changes
    #if (spellcheck) {
        write.csv(txt$replacements[tolower(txt$replacements$word) != tolower(txt$replacements$replacement),],
                  file = paste0(dir, "/made_spelling_replacements.csv"),
                  row.names = F)
    # } else {
    #     write.csv(data.frame(word = character(0),
    #                          replacement = character(0)),
    #               file = paste0(dir, "/made_spelling_replacements.csv"),
    #               row.names = F)
    # }

    if (!is.null(keepWords) & !is.null(replaceWords)) {
        write.csv(data.frame(keep = keepWords,
                             replace = replaceWords),
                  row.names = F,
                  file = paste0(dir, "/custom_spelling_replacements.csv"))
    } else {
        write.csv(data.frame(keep = character(0),
                             replace = character(0)),
                  row.names = F,
                  file = paste0(dir, "/custom_spelling_replacements.csv"))
    }

    # write file to view frequencies
    write.csv(df,
              file = paste0(dir, "/stem_map_with_freqs.csv"),
              row.names = F)

    # write file to view custom merges
    if (!is.null(mergekeep) & !is.null(mergereplace)) {
        write.csv(data.frame(keep = mergekeep,
                             replace = mergereplace),
                  row.names = F,
                  file = paste0(dir, "/custom_merges.csv"))
    } else {
        write.csv(data.frame(keep = character(0),
                             replace = character(0)),
                  row.names = F,
                  file = paste0(dir, "/custom_merges.csv"))
    }

    # write file to view forced best source changes
    if (!is.null(bestkeep) & !is.null(bestreplace)) {
        write.csv(data.frame(keep = bestkeep,
                             replace = bestreplace),
                  row.names = F,
                  file = paste0(dir, "/custom_best_sources.csv"))
    } else {
        write.csv(data.frame(keep = character(0),
                             replace = character(0)),
                  row.names = F,
                  file = paste0(dir, "/custom_best_sources.csv"))
    }
}
