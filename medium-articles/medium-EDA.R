library(tidyverse)
library(tidytext)
library(SnowballC)
library(gridExtra)
library(scales)

#gather data
url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-12-04/medium_datasci.csv'
articles <- read_csv(url)
data(stop_words)

#create article id, new tag for when articles have been tagged multiple times (i.e. gets a 1 if has
# tag for both ai and machine learning)
#select relevant columns
#order by claps, desc
articles <- articles %>%
  mutate(
    id = seq(1:nrow(articles)),
    tag_multiple = ifelse((tag_ai + tag_artificial_intelligence + tag_big_data + tag_data +
                             tag_data_science + tag_data_visualization + tag_deep_learning +
                             tag_machine_learning) > 1,1,0)
  ) %>%
  select(id,author,title,publication,claps,tag_ai,tag_artificial_intelligence,tag_big_data,
         tag_data,tag_data_science,tag_data_visualization,tag_deep_learning,tag_machine_learning) %>%
  arrange(desc(claps))

#get top articles
top_articles <- articles %>%
  filter(claps > quantile(articles$claps, 0.99))

mediocre_articles <- articles %>%
  filter(claps < quantile(articles$claps, 0.75) & claps > quantile(articles$claps, 0.25))

#write function to tidy text
tidy_my_text <- function(df, gram, freq) {
  if (gram=='bigram') {
    tidy_df <- df %>% 
      unnest_tokens(word,title,token = 'ngrams', n=2, to_lower = TRUE, drop = TRUE) %>%
      separate(word,c('word1','word2'),sep = ' ') %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      drop_na(word1) %>%
      drop_na(word2) %>%
      mutate(word_stem1 = wordStem(word1, language="english"),
             word_stem2 = wordStem(word2, language="english")) %>%
      unite(word, word_stem1, word_stem2, sep = ' ') %>%
      filter(!str_detect(word, "[:digit:]"))
  } else {
    tidy_df <- df %>% 
      unnest_tokens(word,title,token = 'ngrams', n = 1, to_lower = TRUE, drop = TRUE) %>%
      drop_na(word) %>%
      anti_join(stop_words) %>%
      mutate(word = wordStem(word, language="english")) %>%
      filter(!str_detect(word, "[:digit:]")) %>%
      filter(!str_detect(word, 'na')) %>%
      filter(!str_detect(word, 'de'))
  }
  if (freq == TRUE) {
    tidy_df <- tidy_df %>%
      count(word, id, author, claps, sort = TRUE)
  }
  return (tidy_df)
}

#function to group data
group_by_word <- function(df) {
  grouped_df <- df %>%
    group_by(word) %>%
    summarise(mean=mean(n),total=sum(n)) %>%
    arrange(desc(total))
  return (grouped_df)
}

#get tidy dfs
tidy_articles_med_unigram <- tidy_my_text(mediocre_articles, 'unigram', freq = FALSE)
tidy_articles_top_unigram <- tidy_my_text(top_articles, 'unigram', freq = FALSE)
tidy_articles_med_bigram <- tidy_my_text(mediocre_articles, 'bigram', freq = FALSE)
tidy_articles_top_bigram <- tidy_my_text(top_articles, 'bigram', freq = FALSE)

tidy_articles_med_unigram_counts <- tidy_my_text(mediocre_articles, 'unigram', freq = TRUE)
tidy_articles_top_unigram_counts <- tidy_my_text(top_articles, 'unigram', freq = TRUE)
tidy_articles_med_bigram_counts <- tidy_my_text(mediocre_articles, 'bigram', freq = TRUE)
tidy_articles_top_bigram_counts <- tidy_my_text(top_articles, 'bigram', freq = TRUE)

#group by word
grouped_med_unigram <- group_by_word(tidy_articles_med_unigram_counts)
grouped_top_unigram <- group_by_word(tidy_articles_top_unigram_counts)
grouped_med_bigram <- group_by_word(tidy_articles_med_bigram_counts)
grouped_top_bigram <- group_by_word(tidy_articles_top_bigram_counts)

#plot top 10 words for viral posts and all posts
freq_plot <- function(df, my_title, cutoff) {
  plot <- ggplot(aes(word,total),data = subset(df,total>=cutoff)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    theme_minimal() +
    labs(title = my_title) 
}

unigram_viral_plot <- freq_plot(grouped_top_unigram, 'Viral Posts: Unigram', grouped_top_unigram$total[10])
unigram_med_plot <- freq_plot(grouped_med_unigram, 'Avg. Posts: Unigram', grouped_med_unigram$total[10])
bigram_med_plot <- freq_plot(grouped_med_bigram, 'Avg. Posts: Bigram', grouped_med_bigram$total[10])
bigram_viral_plot <- freq_plot(grouped_top_bigram, 'Viral Posts: Bigram', grouped_top_bigram$total[10])


#look at how viral post word usage compares to other posts
#get unigram data
unigram_all <- bind_rows(mutate(tidy_articles_med_unigram, viral = 0),
                     mutate(tidy_articles_top_unigram, viral = 1)) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(viral, word) %>%
  group_by(viral)

viral_df_uni <- subset(unigram_all, viral==1) %>%
  rename(n_viral = n) %>%
  mutate(proportion_viral = n_viral / sum(n_viral))

mediocre_df_uni <- subset(unigram_all, viral==0) %>%
  rename(n_med = n) %>%
  mutate(proportion_med = n_med / sum(n_med))

merged_unigram <- inner_join(viral_df_uni, mediocre_df_uni, by='word')


#get unigram plot
unigram_scatter_plot <- ggplot(merged_unigram, aes(x = proportion_viral, y = proportion_med)) +
  geom_abline(intercept=0, slope=1,color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = 'Word Choice in Viral Posts vs Avg. Posts: Unigrams',
       x = 'Unigram Proportion, Viral',
       y = 'Unigram Proportion, Average')

#format bigram data
bigram_all <- bind_rows(mutate(tidy_articles_med_bigram, viral = 0),
                         mutate(tidy_articles_top_bigram, viral = 1)) %>%
  count(viral, word) %>%
  group_by(viral)

viral_df_bi <- subset(bigram_all, viral==1) %>%
  rename(n_viral = n) %>%
  mutate(proportion_viral = n_viral / sum(n_viral))
  
mediocre_df_bi <- subset(bigram_all, viral==0) %>%
  rename(n_med = n) %>%
  mutate(proportion_med = n_med / sum(n_med))

merged_bigram <- inner_join(viral_df_bi, mediocre_df_bi, by='word')

#get plot for bigram
bigram_scatter_plot <- ggplot(merged_bigram, aes(x = proportion_viral, y = proportion_med)) +
  geom_abline(intercept = 0, slope = 1, color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  theme_minimal() +
  theme(legend.position="none") +
  labs(title = 'Word Choice in Viral Posts vs Avg. Posts: Bigrams',
       x = 'Bigram Proportion, Viral',
       y = 'Bigram Proportion, Average')




#put all four plots on a single grid
ggsave('barplot.png',
       plot = grid.arrange(unigram_viral_plot, unigram_med_plot,bigram_viral_plot, bigram_med_plot, nrow = 2,ncol=2),
       width = 6,
       height = 6)

#save other plots
ggsave('unigram_scatter.png', 
       plot = unigram_scatter_plot,
       height = 4,
       width = 6)

ggsave('bigram_scatter.png',
       plot = bigram_scatter_plot,
       height = 4,
       width = 6)









