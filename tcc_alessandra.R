# Sun May 22 12:14:23 2022 ------------------------------
# Aves dos parques urbanos de São José dos Campos
# Trabalho de monografia Alessandra de Souza

#--- Carregando pacotes
library(tidyverse)

#--- Load data
dir()

# Composição de espécies
composition <- read.csv2("sp_composition.csv")
# Ajustando problemas de leitura do arquivo 
rownames(composition) <- composition[,1]
composition <- composition[,-1:2]# erro na leitura, retirando coluna
head(composition)

# Dados de urbanização dos parques
abiotic <- read.csv2("abiotic_variables.csv")
head(abiotic)

# Dados de atributos funcionais para aves
traits <- read.csv2("traits_birds.csv")
View(traits)

#--- Data Transform
# Match lista de espécies e traits


#--- Data analysis
# Riqueza de cada parque e como isso está relacionado ao tamanho do parque?
# Birds richness - sum of each urban park
composition_rich <- composition %>%
  mutate_at(c(names(composition)), as.numeric) %>%
  mutate(Rich = rowSums(composition))

# RLQ - dados abióticos, traits e composição

#--- Data visualizing