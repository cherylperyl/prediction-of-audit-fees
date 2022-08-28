# install required package
# install.packages("naivebayes")
# install.packages("superml", dependencies=TRUE) # to install all dependencies at once

# import the necessary packages
library(readr)
library(dplyr)
library(naivebayes)
library(superml)
library(grepl)
library(ggplot2)
# library(stringr)

# import data
setwd("~/Documents/01 Stats Prog/Project")
data = read_csv("data/Team Project AuditFees version.csv")
summary(data)

# remove entries with is.na(busdesc)
sum(is.na(data$busdesc)) # 5 NAs
data = filter(data, !is.na(busdesc))
sum(is.na(data$busdesc)) # 0 NAs

# select the columns needed
data_text = select(data, c(busdesc, AUDIT_FEES))
summary(data_text)

# group by busdesc to remove duplicate records, take the mean AUDIT_FEES for duplicate records of busdesc
data_text1 = data_text %>% 
  group_by(busdesc) %>% 
  mutate(AUDIT_FEES = mean(AUDIT_FEES)) %>%
  filter(row_number()==1)
summary(data_text1)

# initialize count vectorizer class
cv = CountVectorizer$new(remove_stopwords = TRUE, 
                         lowercase = TRUE,
                         regex = "/^[A-Za-z]+$/",
                         max_df = 0.008)

# takes too long to run, used a python function to complete this step, just import the csv given
# cv_matrix = cv$fit_transform(c(data_text2$busdesc)) 
cv_matrix = read_csv("data/word counts.csv")

# check summary stats on audit fees
summary(cv_matrix$AUDIT_FEES) # mean = 2613353

# use the mean to classify high and low audit fees
cv_matrix = cv_matrix %>% mutate(AUDIT_FEES = ifelse( AUDIT_FEES > 2583090, "High", "Low"))

# use multinomial Naïve Bayes to find probabilities
X = select(cv_matrix, -AUDIT_FEES)
y = cv_matrix$AUDIT_FEES
mnb = multinomial_naive_bayes(X, y)

# transform result to dataframe to view
result = as.data.frame(mnb$params)
result = mutate(result, word = rownames(result))
rownames(result) = NULL
View(result)

# get the top 10 words with the highest probability for High Audit Fees
High_top_10 = result %>% 
  select(c(word, High, Low)) %>% 
  arrange(desc(High)) %>%  
  head(10)
# write.csv(High_top_10, "visualisations/data/High_top_10.csv")

# get the top 10 words with the highest probability for Low Audit Fees
Low_top_10 = result %>% 
  select(c(word, Low, High)) %>% 
  arrange(desc(Low)) %>%  
  head(10)
# write.csv(Low_top_10, "visualisations/data/Low_top_10.csv")

# FIND OUT WHICH INDUSTRIES THESE WORDS USUALLY APPEAR IN
data_industry = select(data, c(busdesc, FFI12_desc))

# get top 10 words for each category (high/low audit fees)
High_top_10_words = unique(High_top_10$word)
Low_top_10_words = unique(Low_top_10$word)

# if word is in busdesc, replace busdesc with 1
High_result = data_industry
for (word in High_top_10_words)
{
  High_result = High_result %>% 
    mutate(busdesc = ifelse(is.character(busdesc) & grepl(word, busdesc, ignore.case = T), 1, busdesc))
}
Low_result = data_industry
for (word in Low_top_10_words)
{
  Low_result = Low_result %>% 
    mutate(busdesc = ifelse(is.character(busdesc) & grepl(word, busdesc, ignore.case = T), 1, busdesc))
}

# filter rows with busdesc == 1 to get the FFI12_desc with the top 10 words of each category
High_result_final = filter(High_result, busdesc == 1)
Low_result_final = filter(Low_result, busdesc == 1)

# shows the number of times a FFI12_desc appears in the top 10 words of High Audit Fees
High_FFI12 = as.data.frame(table(High_result_final$FFI12_desc)) %>% arrange(desc(Freq))
View(High_FFI12)
# write.csv(High_FFI12, "visualisations/data/High_FFI12.csv")

# shows the number of times a FFI12_desc appears in the top 10 words of Low Audit Fees
Low_FFI12 = as.data.frame(table(Low_result_final$FFI12_desc))
View(Low_FFI12)
# write.csv(Low_FFI12, "visualisations/data/Low_FFI12.csv")

# VISUALISE RESULTS

# industry frequency in top 10 words for High Audit Fees
ggplot(High_result_final, aes(x = reorder(FFI12_desc,FFI12_desc,function(x)-length(x)))) + 
  geom_bar(fill = "#64D1CC") +
  xlab("FFI12_desc") +
  ylab("Count")

# industry frequency in top 10 words for Low Audit Fees
ggplot(Low_result_final, aes(x = reorder(FFI12_desc,FFI12_desc,function(x)-length(x)))) + 
  geom_bar(fill = "#CCAAEC") +
  xlab("FFI12_desc") +
  ylab("Count")

# Conditional Probabilty for each of the top 10 words for High Audit Fees
ggplot(data=High_top_10, aes(x = word, y = High)) + 
  geom_bar(fill = "#38A29D", stat="identity") +
  xlab("Word") +
  ylab("Probability")

# Conditional Probabilty for each of the top 10 words for Low Audit Fees
ggplot(data=Low_top_10, aes(x = word, y = Low)) + 
  geom_bar(fill = "#D495E9", stat="identity") +
  xlab("Word") +
  ylab("Probability")


# DATA VISUALISATIONS

# mean audit fees by industry
data1 = data %>% 
  select(AUDIT_FEES, FFI12_desc) %>%
  group_by(FFI12_desc) %>%
  mutate(AUDIT_FEES_mean = mean(AUDIT_FEES, na.rm=T)) %>%
  filter(row_number()==1) %>% 
  select(-AUDIT_FEES) %>%
  ungroup() %>%
  arrange(AUDIT_FEES_mean)
View(data1)

ggplot(data=data1, aes(x= FFI12_desc, y = AUDIT_FEES_mean)) +
  geom_bar(fill = "#D495E9", stat="identity") +
  xlab("FFI12_desc") +
  ylab("Mean Audit Fee")

