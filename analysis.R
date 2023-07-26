library(tidyverse)

Results <- read.csv("/Users/mateuszkazimierczak/Desktop/filter_backend/results.csv")%>% drop_na()
Ds <- read.csv("/Users/mateuszkazimierczak/Downloads/FinalDataset.csv")

final <- Results %>% left_join(Ds, by = "text") %>% select(-toxic, -text) %>% drop_na()

summ <- final %>% group_by(dataset) %>% summarize(tp = sum(truth == prediction & prediction == "TRUE"),
                                          tn = sum(truth == prediction & prediction == "FALSE"),
                                          fp = sum(truth != prediction & prediction == "TRUE"),
                                          fn = sum(truth != prediction & prediction == "FALSE"),
                                          Accuracy = (tp + tn)/(tp + tn + fp + fn),
                                          Precision = tp/(tp + fp),
                                          Recall =  tp/(tp + fn),
                                          f1 = 2 * (Precision * Recall)/(Precision + Recall),
                                          n = n()
) %>% gather(key = measure, value = Rate,
        c("Accuracy", "Precision", "Recall", "f1"))

summTotal <- final %>% summarize(tp = sum(truth == prediction & prediction == "TRUE"),
                                                  tn = sum(truth == prediction & prediction == "FALSE"),
                                                  fp = sum(truth != prediction & prediction == "TRUE"),
                                                  fn = sum(truth != prediction & prediction == "FALSE"),
                                                  Accuracy = (tp + tn)/(tp + tn + fp + fn),
                                                  Precision = tp/(tp + fp),
                                                  Recall =  tp/(tp + fn),
                                                  f1 = 2 * (Precision * Recall)/(Precision + Recall),
                                                  n = n()
) %>% gather(key = measure, value = Rate,
             c("Accuracy", "Precision", "Recall", "f1"))

summTotal$dataset = "Total"

final <- bind_rows(summ, summTotal)

ggplot(final, aes(x = measure, y = Rate, fill=dataset)) + 
  geom_col(position="dodge") + 
  labs(title="Performance of system on data from different datasets", x="statistical measure", y="measure value", x="value")


tp <- nrow(Results[Results$truth == Results$prediction & Results$prediction == "TRUE",])
tn <- nrow(Results[Results$truth == Results$prediction & Results$prediction == "FALSE",])

fp <- nrow(Results[Results$truth != Results$prediction & Results$prediction == "TRUE",])
fn <- nrow(Results[Results$truth != Results$prediction & Results$prediction == "FALSE",])

a <- (tp + tn)/(tp + tn + fp + fn)

p <- tp/(tp + fp)

r <-  tp/(tp + fn)

f1 <- 2 * (p * r)/(p + r)


