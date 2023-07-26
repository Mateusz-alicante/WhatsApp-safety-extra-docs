Ds <- read.csv("/Users/mateuszkazimierczak/Downloads/FinalDataset.csv")

dsn <- Ds %>% group_by(toxic) %>% summarize(n = n())

ggplot(Ds, aes(x = toxic, fill=dataset)) + geom_bar(size = 1) + labs(title="Number of Toxic and Non-toxic entries in the dataset") +
  scale_color_manual(values=c("lightgreen", "red")) + guides(color="none")

Ds2 <- Ds %>% mutate(len = nchar(text))


ggplot(Ds2, aes(x = dataset, y = len, fill = dataset)) +
  geom_violin() +
  geom_jitter(height = 0, width = 0.1) +
  theme(legend.position = "none") + 
  labs(title="Distribution of text lengths by Datasets", x = "Dataset", y = "Lenght of text")
