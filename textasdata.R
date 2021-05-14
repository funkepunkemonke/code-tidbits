# load libraries
library(devtools)
devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)
library(tidytext)
library(plyr)
library(tidyverse)
library(quanteda)

philosophers_stone_corpus <- corpus(philosophers_stone)
philosophers_stone_summary <- summary(philosophers_stone_corpus) 
philosophers_stone_summary

# check for metadata; shouldn't see any
docvars(philosophers_stone_corpus)

# add an indicator for the book; this will be useful later when we add 
# all the books together into a single corpus
philosophers_stone_summary$book <- "Philosopher's Stone"

# create a chapter indicator
philosophers_stone_summary$chapter <- as.numeric(str_extract(philosophers_stone_summary$Text, "[0-9]+"))

#Now we can assign these to the corpus as document-level metadata as follows
docvars(philosophers_stone_corpus) <- philosophers_stone_summary
docvars(philosophers_stone_corpus)

#look at only chapters with fewer than 5,000 tokens AKA WORDS.
small_corpus <- corpus_subset(philosophers_stone_corpus, Tokens < 5000)
summary(small_corpus)

# the number of documents (chapters) in our small corpus
ndoc(small_corpus)

# the command to reshape our corpus to the sentence level
small_corpus_sentences <- corpus_reshape(small_corpus, to = "sentences")

# the number of documents (sentences) in our reshaped corpus
ndoc(small_corpus_sentences)

# a summary of the first 5 texts in the sentence-level corpus
summary(small_corpus_sentences, n=5)

# the default breaks on white space
philosophers_stone_tokens <- tokens(philosophers_stone_corpus)
print(philosophers_stone_tokens)

# you can also drop punctuation
philosophers_stone_tokens <- tokens(philosophers_stone_corpus, 
                                    remove_punct = T)
print(philosophers_stone_tokens)

# as well as numbers
philosophers_stone_tokens <- tokens(philosophers_stone_corpus, 
                                    remove_punct = T,
                                    remove_numbers = T)
print(philosophers_stone_tokens)

# check the use of "dumbledore"
kwic_dumbledore <- kwic(philosophers_stone_tokens, 
                        pattern = c("dumbledore"))

# look at the first few uses
head(kwic_dumbledore)

# now look at a broader window of terms around "dumbledore"
kwic_dumbledore <- kwic(philosophers_stone_tokens, 
                        pattern = c("dumbledore"),
                        window = 10)

# look at the first few uses
head(kwic_dumbledore)

# if you are more interested in phrases, then you can do that too using phrase()
kwic_phrase <- kwic(philosophers_stone_tokens,
                    pattern = phrase("daily prophet"))
head(kwic_phrase)



# list out the object (book) names that we need
myBooks <- c("philosophers_stone", "chamber_of_secrets", "prisoner_of_azkaban", "goblet_of_fire", "order_of_the_phoenix", 
             "half_blood_prince", "deathly_hallows")

# create loop.
for (i in 1:length(myBooks)){
  
  # create corpora
  corpusCall <- paste(myBooks[i],"_corpus <- corpus(",myBooks[i],")", sep = "")
  eval(parse(text=corpusCall))
  
  # change document names for each chapter to include the book title. If you don't do this, the document names will be duplicated and you'll get an error.
  namesCall <- paste("tmpNames <- docnames(",myBooks[i],"_corpus)", sep = "")
  eval(parse(text=namesCall))
  bindCall <- paste("docnames(",myBooks[i],"_corpus) <- paste(\"",myBooks[i],"\", tmpNames, sep = \"-\")", sep = "")
  eval(parse(text=bindCall))
  
  # create summary data
  summaryCall <- paste(myBooks[i],"_summary <- summary(",myBooks[i],"_corpus)", sep = "")
  eval(parse(text=summaryCall))
  
  # add indicator
  bookCall <- paste(myBooks[i],"_summary$book <- \"",myBooks[i],"\"", sep = "")
  eval(parse(text=bookCall))
  
  # add chapter indicator
  chapterCall <- paste(myBooks[i],"_summary$chapter <- as.numeric(str_extract(",myBooks[i],"_summary$Text, \"[0-9]+\"))", sep = "")
  eval(parse(text=chapterCall))
  
  # add meta data to each corpus
  metaCall <- paste("docvars(",myBooks[i],"_corpus) <- ",myBooks[i],"_summary", sep = "")
  eval(parse(text=metaCall))
  
}

# once the loop finishes up, check to make sure you've created what you want
docvars(deathly_hallows_corpus)

# create combined corpora of the first 7 Harry Potter books.
harry_potter_corpus <- c(philosophers_stone_corpus, chamber_of_secrets_corpus, prisoner_of_azkaban_corpus, goblet_of_fire_corpus, order_of_the_phoenix_corpus, 
                         half_blood_prince_corpus, deathly_hallows_corpus)
summary(harry_potter_corpus)

# check the number of documents (here, total chapters in the 7 books)
ndoc(harry_potter_corpus)

# check the total length of the text (i.e., the total word count)
sum(ntoken(harry_potter_corpus))

# check the total length of the text (i.e., the total word count)
max(ntoken(harry_potter_corpus))

# check the use of "hagrid"
kwic_hagrid <- kwic(philosophers_stone_tokens, 
                        pattern = c("hagrid"))

# look at the first few uses
head(kwic_hagrid)

# now look at a broader window of terms around "hagrid"
kwic_hagrid <- kwic(philosophers_stone_tokens, 
                        pattern = c("hagrid"),
                        window = 10)

# look at the first few uses
head(kwic_hagrid)

hist(ntoken(harry_potter_corpus))
