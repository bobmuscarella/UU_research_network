#############################
####### LOAD PACKAGES #######
#############################

library(scholar)
library(tm)
library(igraph)

###############################
####### PREP INPUT DATA #######
###############################

# Define the id for someone (must look manually at each person's Google Scholar page)
# Google Scholar URLs looks like: https://scholar.google.com/citations?user=SuKDQfoAAAAJ&hl=en&oi=ao
# The letters between 'user=' and '&hl' (i.e., SuKDQfoAAAAJ from above) is the Google Scholar ID.

# Make an input .csv file with columns:
#   - name
#   - program
#   - dept
#   - section
#   - id

############################################
####### DOWNLOAD FROM GOOGLE SCHOLAR #######
############################################

# Set the path to your local path
path <- "/Users/au529793/Projects/GIT/UU_research_network/Data/"

# Read in your prepared data
df <- read.csv(paste0(path, "IEG_Google_Scholar_information.csv"))

# Now it's time to get a list of publications for each person

# A better way to do this (than below) is:
# d <- lapply(as.list(as.character(df$id)), get_publications)

# HOWEVER:
# Google prevents scraping from bots and may block you in the middle of the process.
# To try and reduce the issue, I added a 10 second delay between calls but this may not be enough.
# Alternatively, this may need to be done in several sessions.
# During initial trials for IEG people, I did it over a few sessions.

d <- list()
for(i in 1:nrow(df)){
  print(i)
  Sys.sleep(10)
  d[[i]] <- get_publications(df$id[i])
  names(d)[i] <- df$name[i]
}

# Save the publication data
saveRDS(d, file="/Users/au529793/Projects/GIT/UU_research_network/Data/IEG_PubsData_2019-09-03.RDS")


########################################################
####### DO TEXT PROCESSING OF PUBLICATION TITLES #######
########################################################

# Reload your saved names and publication data (if you're starting from here)
d <- readRDS("/Users/au529793/Projects/GIT/UU_research_network/Data/IEG_PubsData_2019-09-03.RDS")
df <- read.csv(paste0(path, "IEG_Google_Scholar_information.csv"))

# First get all words used in titles from each person
keywords <- lapply(d, function(x) unlist(strsplit(as.character(x$title),' ')))

# Remove punctuation
keywords <- lapply(keywords, removePunctuation, preserve_intra_word_dashes=T)

# Remove blanks
keywords <- lapply(keywords, function(x) x[!x %in% ''])

# Remove 'stopwords' (e.g., and, or, the)
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')
keywords <- lapply(keywords, function(x) tolower(x)[!tolower(x) %in% stopwords('en')])

# Now we need a list of words to build a similarity matrix.
# Now you will make some choices.

# You might take the e.g., 15 most commonly used words for each author:
words <- unique(unlist(lapply(keywords, function(x) names(sort(table(x), decreasing = T)[1:15]))))

# Alternatively, you might want just a list of all unique words used by each author
# words <- unique(unlist(keywords))

# You might want to look at the list and manually remove some (non-informative) words
removes <- c('can','new','common','role', 'tree-fig','puerto','two', 'great','across')
words <- words[!words %in% removes]

# You may want to remove 1 letter "words"
words <- words[nchar(words)>1]

# Now match the individual author keywords to the shared words in a matrix
keywordmat <- do.call(rbind, lapply(keywords, function(x) table(x)[match(words, names(table(x)))]))
rownames(keywordmat) <- df$name
colnames(keywordmat) <- words
keywordmat[is.na(keywordmat)] <- 0

# You might want to use a threshold to and only consider words used a certain number of times
keywordmat <- keywordmat[,colSums(keywordmat>1) > 10]

# You might want to use the frequencies of words per author
# Or you might want to convert this to a 'presence/absence' dataset
keywordmat[keywordmat>0] <- 1


####################################################
####### MAKE AND VISUALIZE THE NETWORK GRAPH #######
####################################################

net <- graph_from_incidence_matrix(keywordmat)

# Color nodes (people) based on their program
V(net)$color[!V(net)$type] <- NA
V(net)$color[!V(net)$type][df$program %in% 'plant'] <- 'forest green'
V(net)$color[!V(net)$type][df$program %in% 'limno'] <- 'steel blue'
V(net)$color[!V(net)$type][df$program %in% 'animal'] <- 'red'
V(net)$color[!V(net)$type][df$program %in% 'evobio'] <- 'orange'

# People nodes will have name labels, shared words will not.
V(net)$label[V(net)$type==F] <- rownames(keywordmat)[V(net)$type==F]

# Set some other plotting parameters for nodes and edges
V(net)$label.cex=.6
V(net)$label.font=2

# Make the width of edges proportional to commonness of word usage 
E(net)$weight <- as.vector(keywordmat)[as.vector(keywordmat)>0]
E(net)$weight <- E(net)$weight/10

# You might want to export as PDF
# pdf("mynetwork.pdf", width=8, height=8)

par(mar=c(0,0,0,0))

plot(net, vertex.shape="none", 
     vertex.label=names(V(net)),
     vertex.label.color=V(net)$color, 
     vertex.label.font=(!V(net)$type)+1, 
     vertex.label.cex=ifelse(V(net)$type, 0.4, 0.75),
     edge.color='light grey',
     edge.width=E(net)$weight)

legend('bottomleft',legend=c('Plant Eco/Evo','Limnology','Animal Ecology','Evolutionary Biology'), 
       bty='n', col=c('forest green','steel blue','red','orange'), 
       pch=16, pt.cex=2)

# dev.off()


