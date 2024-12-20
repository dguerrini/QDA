# Load required libraries
library(tidyverse)     # For data manipulation and visualization
library(tidytext)      # For text mining
library(tm)            # Text mining utilities
library(wordcloud)     # For visualizations
library(topicmodels)   # For topic modeling

# Define the path to the folder with text files
path_to_files <- "path/to/transcripts/"

# Read all text files into a single data frame
file_list <- list.files(path = path_to_files, pattern = "\\.txt$", full.names = TRUE)
transcripts <- tibble(
  file_name = basename(file_list),
  text = map_chr(file_list, ~ readLines(.x) %>% paste(collapse = " "))
)

# Preprocessing: Clean the text
cleaned_transcripts <- transcripts %>%
  unnest_tokens(word, text) %>%              # Tokenize text into words
  anti_join(stop_words, by = "word") %>%     # Remove common stop words
  filter(!word %in% c("uh", "um", "yeah")) %>% # Optional: Remove filler words
  filter(str_detect(word, "^[a-zA-Z]+$"))    # Keep only alphabetical tokens

# Word frequency analysis
word_counts <- cleaned_transcripts %>%
  count(word, sort = TRUE)

# Display the top 10 most frequent words
print(word_counts %>% slice_max(n, n = 10))

# Create a word cloud
wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  min.freq = 2,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

# Sentiment analysis (e.g., using Bing lexicon)
sentiment_analysis <- cleaned_transcripts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(file_name, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0)

# Visualize sentiment across files
sentiment_analysis %>%
  mutate(total = positive - negative) %>%
  ggplot(aes(x = file_name, y = total, fill = total > 0)) +
  geom_col() +
  coord_flip() +
  labs(title = "Sentiment Analysis by Transcript", y = "Sentiment Score", x = "Transcript")

# Topic modeling (LDA)
dtm <- cleaned_transcripts %>%
  count(file_name, word) %>%
  cast_dtm(file_name, word, n)

lda_model <- LDA(dtm, k = 3, control = list(seed = 1234))  # Adjust k (number of topics)
lda_topics <- tidy(lda_model, matrix = "beta")

# Top terms per topic
top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 5) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)

# Visualize top terms per topic
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic", x = "Terms", y = "Beta Value")
