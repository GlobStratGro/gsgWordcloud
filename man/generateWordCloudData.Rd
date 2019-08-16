\name{generateWordCloudData}
\alias{generateWordCloudData}
\title{Generate Data for Wordcloud Use}
\usage{
generateWordCloudData(txt, spellcheck = TRUE, combine = TRUE, name = "wordcloud_text",
  location = getwd(), keepWords = NULL, replaceWords = NULL)
}
\arguments{
\item{txt}{a character or character vector, the text data you wish to use}

\item{spellcheck}{logical, whether or not to spellcheck the input. Defaults to \code{TRUE}. Custom-entered replacements will be made regardless.}
\item{combine}{logical, whether or not to stem and combine the input. Defautls to \code{TRUE}}

\item{name}{a character, the name of the folder to output the data to. Defaults to \code{wordcloud_text}}

\item{location}{a character, the file directory in which to place the folder. Defaults to the current working directory.}
\item{keepWords}{a character vector, the words to replace with a custom replacement}
\item{replaceWords}{a character vector, the replacements}

\item{mergekeep}{a character vector of words to force merge into specific groups}
\item{mergereplace}{a character vector of destinations for the force-merged words}
\item{bestkeep}{a character vector of identified best sources to force to a new name}
\item{bestreplace}{a character vector of new names to send the specified sources}
}

\value{
No output value is created. Several files are written to the specified location under the specified folder name:
\item{cleanedOEs.txt}{A text file containing each cleaned, stemmed, and origin-matched word repeated the same number of times as its sources appear in the input text. This file is intended to be easily copied and pasted into Wordle or any other word cloud generator.}
\item{stem_map_with_freqs.csv}{A csv file, which can be openned in Microsoft Excel, that contains each outputted matched word, the stem used to match to it, all the source words found in the cleaned input that stemmed to it, and combined frequency of all the sources.}
\item{made_spelling_replacements.csv}{If \code{spellcheck} is \code{TRUE}, a csv file is generated indicating every word that is replaced during spellchecking and the correct word it is replaced with.}
\item{custom_spelling_replacements.csv}{The custom replacements provided.}
}
\description{
Generates cleaned and combined data for wordcloud use. Has functionality to combine words by word stem into the most common full English word appearing in the input, as well as to spell check the inputted text data.
}
