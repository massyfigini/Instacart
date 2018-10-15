# MARKET BASKET ANALYSIS WITH INSTANCART DATA
# By Massimiliano Figini
# www.massimilianofigini.com
# 2018-10-13

# load libreries
library(data.table)
library(dplyr)
library(arules)
library(stringr)
library(tidyr)


# load data
orders <- fread('C:/Users/figinim/Documents/Studies/Instacart/data/orders.csv')
products <- fread('C:/Users/figinim/Documents/Studies/Instacart/data/products.csv')
order_products <- fread('C:/Users/figinim/Documents/Studies/Instacart/data/order_products__train.csv')
order_products_prior <- fread('C:/Users/figinim/Documents/Studies/Instacart/data/order_products__prior.csv')
aisles <- fread('C:/Users/figinim/Documents/Studies/Instacart/data/aisles.csv')
departments <- fread('C:/Users/figinim/Documents/Studies/Instacart/data/departments.csv')

# clean data
products %>% filter(str_detect(product_name, ","))
products$product_name <- gsub(",", " ", products$product_name)
ordersessential <- select(order_products,order_id,product_id)
ordersessential <- select(inner_join(ordersessential, products, by = "product_id"), order_id, product_name)
ordersmatrix <- select(aggregate(product_name ~ order_id, data = ordersessential, paste, collapse = ","),product_name)

# crea CSV
dir.create(path = "tmp", showWarnings = FALSE)
write.csv(ordersmatrix, "./tmp/transactions.csv")

# eliminato manualmente colonna ID, riga intestazione e banana

# crea sparse matrix dal CSV
SM <- read.transactions("./tmp/transactions.csv", format="basket", sep = ",", quote="")
#summary(SM)

# grafico con top 10 acquisti
itemFrequencyPlot(SM, topN=10)
# dev.off()

# algoritmo apriori
rules <- apriori(SM, parameter = list(support = 0.0001, confidence = 0.20, minlen = 2))
# SUPPORT: percentuale di transazioni che coinvolgono la regola
# CONFIDENCE: percentuale di correttezza - nel confidence% di casi di acquisto di input è acquistato anche l'output

# inspect(rules)

# trasformo in data.frame
rules_df <- as(rules, "data.frame")

# considero solo 1 prodotto in input e 1 in output 
rules_df <- rules_df[!grepl(",", rules_df$rules),]

# separo input da output
rules_df <- rules_df %>% separate(rules, into = c('input', 'output'), extra = 'merge', sep = "=>")

# tolgo parentesi graffe
rules_df[] <- lapply(rules_df, gsub, pattern="\\{", replacement="")
rules_df[] <- lapply(rules_df, gsub, pattern="\\}", replacement="")

# converto in numerico colonne char
rules_df$support <- as.numeric(as.character(rules_df$support))
rules_df$confidence <- as.numeric(as.character(rules_df$confidence))
rules_df$lift <- as.numeric(as.character(rules_df$lift))
rules_df$count <- as.numeric(as.character(rules_df$count))

# NUOVA TABELLA CON 3 REGOLE PER OGNI PRODOTTO

# 1. Top 1
# prendo quella con confidence maggiore
# CONFIDENCE: quanto percentuale di persone che hanno comprato input hanno comprato anche output
Top1 <- rules_df[order(rules_df$confidence,decreasing=T),]
Top1 <- Top1[!duplicated(Top1$input),]

# 2. Second Top
Top2 <- anti_join(rules_df, Top1, by = c("input", "output"))
Top2 <- Top2[order(Top2$confidence,decreasing=T),]
Top2 <- Top2[!duplicated(Top2$input),]

# 3. Third Top
Top3 <- anti_join(rules_df, Top1, by = c("input", "output"))
Top3 <- anti_join(Top3, Top2, by = c("input", "output"))
Top3 <- Top3[order(Top3$confidence,decreasing=T),]
Top3 <- Top3[!duplicated(Top3$input),]

# Tabella con 3 output maggiori per ogni input
Final <- left_join(Top1, Top2, by="input")
Final <- left_join(Final, Top3, by="input")
colnames(Final) <- c("input","output1","support1","confidence1","lift1","count1",
                     "output2","support2","confidence2","lift2","count2",
                     "output3","support3","confidence3","lift3","count3")

# Clean
Final$input <- trimws(Final$input)
Final$output1 <- trimws(Final$output1)
Final$output2 <- trimws(Final$output2)
Final$output3 <- trimws(Final$output3)
Final$input <- tolower(Final$input)
Final$output1 <- tolower(Final$output1)
Final$output2 <- tolower(Final$output2)
Final$output3 <- tolower(Final$output3)
Final <- Final[order(Final$input),]

# salva in RData file
save(Final,file="C:/Users/figinim/Documents/Studies/Instacart/App/Final.RData")

# SEARCH!
Final %>% filter(input == "almond milk strawberry yogurt") %>% select(output1,output2,output3)