# Sun May 22 12:14:23 2022 ------------------------------
# Aves dos parques urbanos de São José dos Campos
# Trabalho de monografia Alessandra de Souza

#--- Carregando pacotes
library(tidyverse) # manipulação de dados
library(visdat) # checar missing data

#--- Load data
dir()

# Composição de espécies
composition <- read.csv2("sp_composition.csv")
head(composition)

# Dados de urbanização dos parques
abiotic <- read.csv2("abiotic_variables.csv")
head(abiotic)

# Dados de atributos funcionais para aves
traits <- read.csv2("traits_birds.csv")
View(traits)

#--- Data Transform

# Ajustando problemas de leitura do arquivo 
rownames(composition) <- composition[,1]
composition <- composition[,-c(1:2)]# erro na leitura, retirando coluna
# Trocando . por _ no nome das espécies para dar match com os traits
names(composition) <- gsub(".", "_", fixed=T, names(composition))
# Ordem alfabética para facilitar
composition <- composition[ , order(names(composition))]

#Conferindo quais espécies da lista possuem traits
# Lista para join
# Arrumando para linhas
birds_list <- composition %>%
  gather(key="sp",value="count", Accipiter_bicolor:Zonotrichia_capensis) %>% distinct(sp, .keep_all = TRUE)

nrow(birds_list) #327 espécies

# Conferindo espécies que saíram no short.traits (sp NA's)
# Anti join: espécies que estão no x, mas não estão no y (x,y)
birds_sp <- left_join(birds_list, traits, by="sp")
birds_sjc_traits <- birds_sp[,-2]

# checando missing data
vis_miss(birds_sjc_traits)

# Match lista de espécies e traits



#--- Data analysis
# Riqueza de cada parque e como isso está relacionado ao tamanho do parque?
# Birds richness - sum of each urban park
composition_rich <- composition %>%
  mutat(c(names(composition)), as.numeric) %>%
  mutate(Rich = rowSums(composition))

# RLQ - dados abióticos, traits e composição

#--- Data visualizing