library(tidyverse)
library(sparklyr)

spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

data(iris)
df1 <- as_tibble(iris)
df <- copy_to(sc, df1)
class(df1)
class(df)
names(df1)
names(df)

df %>% select(Sepal_Length, Species) %>% head(n = 6) %>% print
df %>% filter(Sepal_Length > 5.5) %>% head(n = 6) %>% print
df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head(n = 6) %>% print
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length),count = n()) %>% head %>% print
df2 %>% arrange(Species) %>% head(list(6)) %>% print()

