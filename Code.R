library(tidyverse)



# トピックの設定 -----------------------------------------------------------------

## 各トピックの単語を指定
# トピック1：色
topic_color <- c(
  "オレンジ", "もも", "ラベンダー", "レモン", "青緑", "白"
)

# トピック2：花
topic_flower <- c(
  "あやめ", "こぶし", "さくら", "つばき", "もも", "ラベンダー"
)

# トピック3：果物
topic_fruits <- c(
  "オレンジ", "ぶどう", "メロン", "もも", "りんご", "レモン"
)

# トピック4：人の名前
topic_name <- c(
  "あやか", "かりん", "さくら", "ほのか", "まなか", "もも"
)

topics <- c(topic_color, topic_flower, topic_fruits, topic_name) %>% unique()
topics
length(topics)

tpics_df <- data_frame(
  color = topic_color, 
  flower = topic_flower, 
  fruits = topic_fruits, 
  name = topic_name
)

knitr::kable(tpics_df, format = "markdown")


## 各トピックの単語の出現確率を指定
# トピック1：色
topic_color_prob <- c(
  0.16667, 0.16667, 0.16667, 0.16667, 0.16667, 0.16667
)

# トピック2：花
topic_flower_prob <- c(
  0.05, 0.1, 0.15, 0.2, 0.25, 0.25
)

# トピック3：果物
topic_fruits_prob <- c(
  0.1, 0.15, 0.2, 0.3, 0.15, 0.1
)

# トピック4：人の名前
topic_name_prob <- c(
  0.25, 0.25, 0.15, 0.15, 0.1, 0.1
)


# 最尤推定の原理 ------------------------------------------------------------------


# 各語彙の出現回数
N_v <- c(1, 3, 1, 2, 1, 1)
N <- sum(N_v)
v <- 3

df <- tibble(
  phi_v = seq(0, 1, by = 0.01), 
  phi_o = 1 - phi_v, 
  y = phi_v^N_v[v] * phi_o^(N - N_v[v])
)

ggplot(df, aes(phi_v, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), 
                     labels = seq(0, 1, by = 0.1))

N_v[v] / N


# 最尤推定 --------------------------------------------------------------------



?rand_seed
# サイコロを設定する
dice <- tibble(
  fruit = c("もも", "りんご", "レモン", "ぶどう", "メロン", "オレンジ"), 
  prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)
)

# サイコロを振る
result <- sample(dice$fruit, size = 10000, replace = TRUE, prob = dice$prob)

# 出目を集計する
result_table <- table(result)
result_table

# データフレームに変換する
result_df <- as_tibble(result_table)
colnames(result_df) <- c("Term", "Freq")
result_df2 <- mutate(result_df, Prob = Freq / sum(Freq))
result_df2

# 描画
ggplot(result_df2, aes(Term, Prob)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ylim(c(0, 1))



# MAP推定 -------------------------------------------------------------------

w_n <- c(0, 1, 1, 0, 0, 0, 0, 1, 0, 1)

phi_v <- 0.1

for(n in 1:N){
  phi_v <- phi_v^w_n[n] * (1 - phi_v)^(trunc(1 - w_n[n]))
  print(phi_v)
}
