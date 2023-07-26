Results <- read.csv("/Users/mateuszkazimierczak/Downloads/results6442.csv")

DS <- read.csv("/Users/mateuszkazimierczak/Downloads/maliciousURL.csv")

joinedRaw <- Results %>%  select(url, predicted_harmless, predicted_malicious, predicted_suspicious, predicted_undetected, predicted_timeout) %>% left_join(DS, by="url")

joiendSimple <- joinedRaw %>% mutate(truth = type != "benign", pmp=predicted_malicious/(predicted_harmless + predicted_malicious + predicted_suspicious + predicted_undetected + predicted_timeout))

ggplot(data = joiendSimple, mapping = aes(x = pmp, y = truth)) + 
  geom_boxplot() + 
  labs(title = "Distribution of prediction metrics for mallicious and benign URLs", y= "Is the URL mallicious?", x = "% of reports that classify the URL as mallicious")

df <- data.frame(matrix(ncol = 5, nrow = 0))

colnames(df) <- c('i', 'a', "p", "r", "f1")

for (i in seq(0, 1, by=0.01)) {
  a <- sum((joiendSimple$pmp >= i & joiendSimple$truth == TRUE) | (joiendSimple$pmp < i & joiendSimple$truth == FALSE)) / nrow(joiendSimple)
  p <- sum((joiendSimple$pmp >= i & joiendSimple$truth == TRUE))/(sum(joiendSimple$pmp >= i & joiendSimple$truth == TRUE) + sum(joiendSimple$pmp >= i & joiendSimple$truth == FALSE))
  if (is.nan(p)) {
    p <- 1
  }
  r <- sum((joiendSimple$pmp >= i & joiendSimple$truth == TRUE))/(sum(joiendSimple$pmp >= i & joiendSimple$truth == TRUE) + sum(joiendSimple$pmp <= i & joiendSimple$truth == TRUE))
  f1 <- 2 * (p * r) / (p + r)
  df <- df %>% add_row(i=i, a=a,p=p, r=r, f1=f1)
}

df2 <- gather(df, key = measure, value = Rate, 
             c("a", "p", "r", "f1"))

ggplot(df2, aes(x=i, y = Rate, group = measure, colour = measure)) + 
  geom_line() +
  scale_color_hue(labels=c('Accuracy', 'F1 Score', "Precision", "Recall")) + 
  theme(legend.position="top") + 
  labs(title = "Performance evaluation of URL detection system", x= "threshold (% of evaluations as 'mallicious')", y = "Value of measure") +
  xlim(0, 0.2)
