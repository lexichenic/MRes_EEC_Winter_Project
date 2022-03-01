# Setting working directory
setwd("C:/Users/Lexi C/Desktop/Winter Project/Seleciton of publication")

# Loading packages
library(remotes)
library(litsearchr)
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)

#########################
# Loading Naive Results #
#########################
a <- import_results(file = "1.ris")
a <- subset(a, select = c(title,abstract,keywords))# getting the "title","abstract","keywords" column from the original datasheet
a <- a[c(1), ]# getting the first column of the datasheet

b <- import_results(file = "2.ris")
b <- subset(b, select = c(title,abstract,keywords))
b <- b[c(1), ]

c <- import_results(file = "3.ris")
c <- subset(c, select = c(title,abstract,keywords))
c <- c[c(1), ]

d <- import_results(file = "4.ris")
d <- subset(d, select = c(title,abstract,keywords))
d <- d[c(1), ]

e <- import_results(file = "5.ris")
e <- subset(e, select = c(title,abstract,keywords))
e <- e[c(1), ]

f <- import_results(file = "6.ris")
f <- subset(f, select = c(title,abstract,keywords))
f <- f[c(1), ]

g <- import_results(file = "7.ris")
g <- subset(g, select = c(title,abstract,keywords))
g <- g[c(1), ]

h <- import_results(file = "8.ris")
h <- subset(h, select = c(title,abstract,keywords))
h <- h[c(1), ]

j <- import_results(file = "10.ris")
j <- subset(j, select = c(title,abstract,keywords))
j <- j[c(1), ]

k <- import_results(file = "11.ris")
k <- subset(k, select = c(title,abstract,keywords))
k <- k[c(1), ]

l <- import_results(file = "12.ris")
l <- subset(l, select = c(title,abstract,keywords))
l <- l[c(1), ]

m <- import_results(file = "13.ris")
m <- subset(m, select = c(title,abstract,keywords))
m <- m[c(1), ]

n <- import_results(file = "14.ris")
n <- subset(n, select = c(title,abstract,keywords))
n <- n[c(1), ]

o <- import_results(file = "15.ris")
o <- subset(o, select = c(title,abstract,keywords))
o <- o[c(1), ]

p <- import_results(file = "16.ris")
p <- subset(p, select = c(title,abstract,keywords))
p <- p[c(1), ]

s <- import_results(file = "19.ris")
s <- subset(s, select = c(title,abstract,keywords))
s <- s[c(1), ]

t <- import_results(file = "20.ris")
t <- subset(t, select = c(title,abstract,keywords))
t <- t[c(1), ]

u <- import_results(file = "21.ris")
u <- subset(u, select = c(title,abstract,keywords))
u <- u[c(1), ]

search_results <- rbind(a,b,c,d,e,f,g,h,j,k,l,m,n,o,p,s,t,u)# combining the information of the 18 papers into one datasheet

##################################
# Getting potential search terms #
##################################
keywords <- extract_terms(keywords=search_results[, "keywords"], method="tagged", min_n=1) # gather the keywords from this column of our search results, min_n=1 only get keywords that consist of at least one word
extract_terms(text=search_results[, "title"], method="fakerake", min_freq=2, min_n=1) # look for potential keywords from the titles, min_freq=2, only get keywords that appear at least twice in the full set of results
# Some of the phrases that appear in a title are general science or data analysis terms and are not related to the specific topic of the article. In language analysis, such frequently-occurring but uninformative words are often called 'stopwords'.
all_stopwords <- get_stopwords("English") # getting the set of stopwords provided by litsearchr
# Getting potential search terms from the titles and excluding the stopwords
title_terms <- extract_terms(
  text=search_results[, "title"],
  method="fakerake", # a quick implementation similar to Rapid Automatic Keyword Extraction
  min_freq=2, min_n=1,
  stopwords=all_stopwords
)
terms <- unique(c(keywords, title_terms)) # Let's finish by adding together the search terms we got from the article titles and those we got from the keywords earlier, removing duplicates.

####################
# Network analysis #
####################
docs <- paste(search_results[, "title"], search_results[, "abstract"]) # join the title of each article to its abstract
# create a matrix that records which terms appear in which articles
dfm <- create_dfm(elements=docs, features=terms)# 'DFM' stands for 'document-feature matrix'. The elements argument is the list of documents. The features argument is the list of terms whose relationships we want to analyze within that set of documents.
# The rows of our matrix represent the articles (their titles and abstracts), and the columns represent the search terms. Each entry in the matrix records how many times that article contains that term.
View(dfm)
# We can then turn this matrix into a network of linked search terms, using the litsearchr function create_network().
g <- create_network(dfm, min_studies=2) # min_studies = excludes terms that occur in fewer than a given number of articles.
# Create a picture of our network of terms to get a better idea of its structure using ggplot
ggraph(g, layout="stress") +
  coord_fixed() + # A fixed scale coordinate system forces a specified ratio between the physical representation of data units on the axes.
  expand_limits(x=c(-3, 3)) + # ensure limits include a single value, for all panels or all plots
  geom_edge_link(aes(alpha=weight)) + # add lines linking the terms
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE)+ #add some labels showing what the actual terms are; show just an arbitrary subset of them that do not overlap on the figure
  guides(edge_alpha= none)

###########
# Pruning #
###########
# Use the network to rank our search terms by importance, with the aim of pruning away some of the least important ones.
strengths <- strength(g) # The 'strength' of each term in the network is the number of other terms that it appears together with.
data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(strength) ->term_strengths # arrange the terms in ascending order of strength we see those that might be the least important
term_strengths
# To discard some of the terms that only rarely occur together with the others. First, we can visualize the strength of the terms
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)
cutoff_fig
# Pruning away the search terms least closely linked to the others through retain 80% of the total strength of the network of search terms
cutoff_cum <- find_cutoff(g, method="cumulative", percent=0.8) 
# Visualising it on a graph
cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")
# Once we have found a cutoff value, the reduce_graph() function applies it and prunes away the terms with low strength. The arguments are the original network and the cutoff. The get_keywords() function then gets the remaining terms from the reduced network.
cumulatively<-get_keywords(reduce_graph(g, cutoff_cum))
# Manually add in some terms that must be relevant and include their alternative spelling as well
extra_terms<-c("Trans-generational","Transgenerational","cross-generational","aging","ageing","epigenet*","old","natural","life history","life-history","wild")
cumulatively <- c(cumulatively, extra_terms)
write.csv(cumulatively,file="cumulativelyfinal1.csv")

############
# Grouping #
############
cumulatively
# Manually review the full list of suggested keywords (exported csv file) and group them into concept categories
grouped_terms <-list(
  Age =cumulatively[c(1,6,7,8,12,28,29,30,31,32,34)],
  Bird =cumulatively[c(2,35,38)],
  Effect =cumulatively[c(4,11,13,20,26,27,33,36,37)]
)
grouped_terms

########################
# Writing a new search #
########################
# The write_search() function takes our list of grouped search terms and writes the text of a new search.
write_search(
  grouped_terms,
  languages="English", # provides a list of languages to translate the search into, in case we want to get articles in multiple languages.
  exactphrase=TRUE, # controls whether terms that consist of more than one word should be matched exactly rather than as two separate words. If we have phrases that are only relevant as a whole phrase, then we should set this to TRUE
  stemming=FALSE, # controls whether words are stripped down to the smallest meaningful part of the word (its 'stem') so that we make sure to catch all variants of the word, for example catching both behavior and behavioral.
  closure="left", #  controls whether partial matches are matched at the left end of a word ("left"), at the right ("right"), only as exact matches ("full") or as any word containing a term ("none").
  writesearch=TRUE # controls whether we would like to write the search text to a file.
)
# Result string: 
# (age OR "maternal age" OR "parental age" OR "paternal age" OR senescence OR Trans-generational OR Transgenerational OR cross-generational OR aging OR old) AND (bird OR natural OR wild) AND (fitness OR "reproductive success" OR survival OR offspring OR telomere OR epigenet* OR "life history" OR life-history)

###########################
# Checking the new search #
###########################
# Conduct new search using the string above in 3 different databases
# Import search results from WoS (3400 papers)
list1 <- import_results(file="final1.ris")
nrow(list1) 
list1 <- select (list1, "author","title","year")

list2<- import_results(file="final2.ris")
nrow(list2)
list2 <- select (list2, "author","title","year")

list3<- import_results(file="final3.ris")
nrow(list3)
list3 <- select (list3, "author","title","year")

list4<- import_results(file="final4.ris")
nrow(list4)
list4 <- select (list4, "author","title","year")

new_results <- rbind(list1, list2, list3, list4)
nrow(new_results)
# Import search results from Scopus (687 papers)
Scopus <- read.csv("scopus3.csv")
nrow(Scopus)
colnames(Scopus) <- c("author","author ID","title","year","source.title","volume","issue","article no.","page.start","page.end","page.count","cited.by","DOI","link","abstract","document.type","publication.stage","open.access","source","EID") # rename the column names
Scopus <- select (Scopus, "author","title","year")

# Import search results from PubMed (5365 papers)
PubMed <- read.csv("PubMed5365.csv")
PubMed <- select(PubMed,"Authors","Title","Publication.Year")
colnames(PubMed) <- c("author","title","year")

# Combine all search results from 3 databases (9452 papers)
combine <- rbind(new_results, PubMed, Scopus)
nrow(combine)

# Remove duplication in titles from databases (8768 papers)
combine <- data.frame(combine[!duplicated(combine$title),])
nrow(combine)

# Compare the combined results with the naive result papers
important_titles <- c(
  "Trans-generational effects on ageing in a wild bird population",
  "Reduced fitness in progeny from old parents in a natural population",
  "Interactive effects of parental age on offspring fitness and age-assortative mating in a wild bird",
  "Evolvability of an avian life history trait declines with father's age",
  "Reduced telomere length in offspring of old fathers in a long-lived seabird",
  "Parental age influences offspring telomere loss",
  "Longitudinal evidence that older parents produce offspring with longer telomeres in a wild social bird",
  "Offspring telomere length in the long lived Alpine swift is negatively related to the age of their biological father and foster mother",
  "Paternal but not maternal age influences early-life performance of offspring in a long-lived seabird",
  "Sex-specific pathways of parental age effects on offspring lifetime reproductive success in a long-lived seabird",
  "Maternal Age-Related Depletion of Offspring Genetic Variance in Immune Response to Phytohaemagglutinin in the Blue Tit (Cyanistes caeruleus)",
  "Epigenetic inheritance of telomere length in wild birds",
  "Parental Age and Lifespan Influence Offspring Recruitment: A Long-Term Study in a Seabird",
  "Maternal and genetic factors determine early life telomere length",
  "Parent age, lifespan and offspring survival: structured variation in life history in a wild population",
  "Senescence of Maternal Effects: Aging Influences Egg Quality and Rearing Capacities of a Long-Lived Bird",
  "Paternal age and offspring growth: separating the intrinsic quality of young from rearing effects",
  "Does parental age affect offspring performance through differences in egg quality?"
)
comparison <- data.frame(check_recall(important_titles, combine[, "title"]))
# similarity = 1 means there is an exact match in our new search results compare to the naive search papers
# all papers can be found in our new search results!

###########################################
# Export the papers for systematic review #
###########################################
write.csv(combine,file="FinalSystematicReview.csv")










