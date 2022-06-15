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
composition <- composition[,-1]

# Dados de urbanização dos parques
abiotic <- read.csv2("abiotic_variables.csv")
head(abiotic)

# Dados de atributos funcionais para aves
traits <- read.csv2("traits_birds.csv")
View(traits)

#--- Data Transform
# Ajustando problemas de leitura do arquivo 
rownames(composition) <- composition[,1]
composition_ok <- composition[,-1]# erro na leitura, retirando coluna
# Trocando . por _ no nome das espécies para dar match com os traits
names(composition_ok) <- gsub(".", "_", fixed=T, names(composition_ok))
# Ordem alfabética para facilitar
composition_ok <- composition_ok[ , order(names(composition_ok))]

# Riqueza de cada parque e como isso está relacionado ao tamanho do parque?
# Birds richness - sum of each urban park
composition_rich <- composition_ok %>%
  mutate_at(c(names(composition_ok)), as.numeric) %>%
  mutate(Rich = rowSums(composition_ok))

# Ajustando dataset para incluir riqueza e dados abióticos
# transformar tamanho em variável numérica
abiotic$parque..m2. <- as.numeric(abiotic$parque..m2.)

# criar full data set para gráficos
localities <- cbind(composition, Rich = composition_rich$Rich)
abiotic_change <- rename(abiotic, localities = Parque.Urbano) %>% select(-richness,-impermeabilizado..m2.)

# New dataset 
all_data <- left_join(localities, abiotic_change, by="localities") 
all_data <- rename(all_data, permm2 = permebealizado..m2., size = parque..m2.) 

#Conferindo quais espécies da lista possuem traits
# Lista para join
# Arrumando para linhas
birds_list <- composition_ok %>%
  gather(key="sp",value="count", Accipiter_bicolor:Zonotrichia_capensis) %>% distinct(sp, .keep_all = TRUE)

nrow(birds_list) #327 espécies

# Conferindo espécies que saíram no short.traits (sp NA's)
# Anti join: espécies que estão no x, mas não estão no y (x,y)
birds_sp <- left_join(birds_list, traits, by="sp")
birds_sjc_traits <- birds_sp[,-2]

# checando missing data
write.csv(birds_sjc_traits, "traits_birds_sjc")
# Match lista de espécies e traits
dir()
traits_birds <- read.csv2("traits_birds2.csv" )
View(traits_birds)

# Composição de espécies
colnames(all_data)
localities # composição de espécies
View(localities)
# Aqui precisamos excluir Sporophila_falcirostris, Sporophila_frontalis porque não temos dados de massa corporal
parques <- localities[,-c(273:274)]
parques <- parques[,-1]
parques <- parques[,-326]

# Dados abióticos dos parques urbanos
# Handling name
abiotic_change # abiotic data
rownames(abiotic_change) <- abiotic_change[,1]#renomeando as linhas para os nomes dos parques
abiotic_change <- abiotic_change[,-1] # retirando a primeira coluna nome dos parques
abiotic_change <- abiotic_change %>% rename(agua=corpo.dagua, perm=permebealizado..m2., size=parque..m2. ) #facilitando o nome para chamar o objeto

glimpse(abiotic_change) # variaveis fora do padrão
abiotic_change$agua <- as.factor(abiotic_change$agua)
abiotic_change$size <- as.numeric(abiotic_change$size)
abiotic_change$NDRI <- as.numeric(abiotic_change$NDRI)

# Atributos funcionais
# Selecionando os traits de interesse e removendo missing data
traits_birds_sel <- traits_birds %>% select(sp:ForStrat.aerial) %>% remove_missing(vars="BodyMass.Value") %>% distinct(sp, .keep_all = TRUE)

traits_birds_sel$sp <- str_replace_all(traits_birds_sel$sp, "_", ".") #acertando a nomenclatura para dar match

# Renomeando as linhas com os nomes das espécies
row.names(traits_birds_sel) <- traits_birds_sel[,1]
traits_birds_sel  <- traits_birds_sel[,-1]

# Classificando variaveis
traits_birds_sel$Nocturnal <- as.factor(traits_birds_sel$Nocturnal)
traits_birds_sel$BodyMass.Value <- as.numeric(traits_birds_sel$BodyMass.Value)

nrow(traits_birds_sel) # 323 linhas
ncol(parques) # 325 colunas
View(parques)

# Quem são esses dois a mais?
parques_list <- parques %>%
  gather(key="sp",value="count", Accipiter.bicolor:Zonotrichia.capensis) %>% distinct(sp, .keep_all = TRUE)

nrow(parques_list)# sem espécie repetida nas colunas
anti_join(parques_list, traits_birds_sel, by="sp")
#Mionectes.chibum     
# Mionectes.rufiventris     
# Presentes na lista mas não nos traits, removendo
parques_final <- parques %>% select(-Mionectes.chibum,-Mionectes.rufiventris)
ncol(parques_final) #323
nrow(traits_birds_sel) #323

#--- Data analysis
# RLQ - dados abióticos, traits e composição
coa1 <- dudi.coa(parques_final, scannf = FALSE, nf = 2)

dudimil <- dudi.hillsmith(abiotic_change, scannf = FALSE, nf = 2, row.w = coa1$lw)

duditrait <- dudi.hillsmith(traits_birds_sel, scannf = FALSE, nf = 2, row.w = coa1$cw)

rlq1 <- rlq(dudimil, coa1, duditrait, scannf = FALSE, nf = 2)
plot(rlq1)

summary(rlq1)
randtest(rlq1)

fourthcorner.rlq(rlq1,type="Q.axes")
fourthcorner.rlq(rlq1,type="R.axes")

#--- Data visualizing
ggplot(all_data, aes(x=log10(size), y=Rich))+
  geom_point() +
  labs(x = "Park size (log10)" , y= "Species richness", 
       title = "Urban birds") +
  stat_smooth(method = lm)+
  theme_bw()
