library(ggplot2)
library(dplyr)
library(tidyr)
library(RMySQL)
library(stringr)

## OPEN DATABASE CONNECTION ##
wordbank <- src_mysql(dbname='wordbank',host="54.200.225.86", 
                      user="wordbank",password="wordbank")

## NOW LOAD TABLES ##
admin.table <- tbl(wordbank,"common_administration")
child.table <- tbl(wordbank,"common_child")
wg.table <- tbl(wordbank,"instruments_wg")

wg.vocab.words <- as.data.frame(select(wg.table,
                                       basetable_ptr_id,
                                       col_baabaa:col_some)) %>%
  rename(id = basetable_ptr_id) %>% # Rename the id
  gather(word,produces,col_baabaa:col_some) %>% # Arrange in longform
  mutate(word = str_replace(word, "col_", "")) # Strip off col_ from words

wg.vocab.words <- as.tbl(wg.vocab.words)

admins <- admin.table %>%
  select(data_id,child_id,age,source_id) %>%
  rename(id = data_id, child.id = child_id,source.id = source_id) 
admins <- as.data.frame(admins)

# Get demographic variables for each child
demos <- select(child.table,id,gender,mom_ed,birth_order) %>%
  rename(child.id = id) # Rename id fields
demos <- as.data.frame(demos)

# Join age and demographics together
child.data <- as.tbl(left_join(admins,demos))

# filter down
wg.data <- left_join(wg.vocab.words, child.data) %>%
  filter(age >= 8, age <= 18) %>%
  select(-child.id,-source.id) #drop redundant columns

v1s <- wg.data %>% 
  group_by(id,age) %>% 
  summarise(v = sum(produces==2)) %>% 
  filter(v == 1)

v0s <- wg.data %>% 
  group_by(id,age) %>% 
  summarise(v = sum(produces==2)) %>% 
  filter(v == 0) %>%
  group_by() %>%
  summarise(age=mean(age))


wds <- wg.data %>% 
  filter(id %in% v1s$id, produces==2) %>%
  select(word,age) %>% 
  mutate(older = age >= median(age)) %>%
  group_by(word,older) %>%
  summarise(count = n()) 


ord <- wg.data %>% 
  filter(id %in% v1s$id, produces==2) %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 


wds$word <- factor(wds$word, levels=ord$word)

wds <- wds %>% 
  summarise(count = n())
  

quartz()
qplot(word, count, facets=.~older, data=wds, 
      geom="bar", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90,vjust=0.5))


### get words that they understand as well as produce
## look at birth order, momed, and gender




#### --------------------------
ws.table <- tbl(wordbank,"instruments_ws")

ws.vocab.words <- as.data.frame(select(ws.table,
                                       basetable_ptr_id,
                                       col_baabaa:col_connthen)) %>%
  rename(id = basetable_ptr_id) %>% # Rename the id
  gather(word,produces,col_baabaa:col_connthen) %>% # Arrange in longform
  mutate(word = str_replace(word, "col_", "")) # Strip off col_ from words

ws.vocab.words <- as.tbl(ws.vocab.words)

# filter down
ws.data <- left_join(ws.vocab.words, child.data) %>%
  filter(age >= 15, age <= 30) %>%
  select(-child.id,-source.id) #drop redundant columns

v1s <- ws.data %>% 
  group_by(id,age) %>% 
  summarise(v = sum(produces==2)) %>% 
  filter(v == 1)


ws.wds <- ws.data %>% 
  filter(id %in% v1s$id, produces==2) 