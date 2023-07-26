library(tidyverse)

Surge <- read.csv("/Users/mateuszkazimierczak/Downloads/Social Media Toxicity Dataset.csv")
# Remove URL's and not alphanumeric characters
Surge$text = gsub("\n", " ", Surge$text)
SurgeRefined <- Surge %>% transmute(text = trimws(gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", gsub("[^A-Za-z0-9 ]","",text))), toxic = (Is.this.text.toxic. == "Toxic"), dataset = "Surge")


Paper <- read.csv("/Users/mateuszkazimierczak/Downloads/labeled_data.csv")
# Remove URL's, not alphanumeric characters
Paper$tweet = gsub("\n", " ", Paper$tweet)
PaperRefined <- Paper %>% transmute(text = trimws(gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", gsub("[^A-Za-z0-9 ]","",tweet))), toxic = neither / count < 0.75)
# Remove RT and words starting with @
PaperRefined <- PaperRefined %>% mutate(text = gsub("@\\w+ *", "", gsub("RT", "", text)), dataset = "Paper")

KaggleTrain <- read.csv("/Users/mateuszkazimierczak/Downloads/jigsaw-toxic-comment-classification-challenge/train.csv")
KaggleTestText <- read.csv("/Users/mateuszkazimierczak/Downloads/jigsaw-toxic-comment-classification-challenge/test.csv")
KaggleTestLabels <- read.csv("/Users/mateuszkazimierczak/Downloads/jigsaw-toxic-comment-classification-challenge/test_labels.csv")
KaggleTest <- merge(KaggleTestText, KaggleTestLabels, by = "id")
Kaggle <- rbind(KaggleTrain, KaggleTest)
# Remove URL's, not alphanumeric characters
Kaggle$comment_text = gsub("\n", " ", Kaggle$comment_text)
KaggleRefined <- Kaggle %>%  transmute(text = trimws(gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", gsub("[^A-Za-z0-9 ]","",comment_text))), toxic = (toxic != 0 | severe_toxic != 0 | obscene != 0 | threat != 0 | insult != 0 | identity_hate != 0), dataset = "Kaggle")


FinalWithDuplicates <- rbind(KaggleRefined, PaperRefined, SurgeRefined)

# Remove duplicate text
Final <- FinalWithDuplicates[!duplicated(FinalWithDuplicates$text), ]

shuffled_data= Final[sample(1:nrow(Final)), ]

write.csv(shuffled_data, "/Users/mateuszkazimierczak/Downloads/FinalDataset.csv", row.names=FALSE)
