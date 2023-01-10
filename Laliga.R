
# Leemos los datos

df_0910 <- read.csv("season-0910_csv.csv", stringsAsFactors = FALSE)
df_1011 <- read.csv("season-1011_csv.csv", stringsAsFactors = FALSE)
df_1112 <- read.csv("season-1112_csv.csv", stringsAsFactors = FALSE)
df_1213 <- read.csv("season-1213_csv.csv", stringsAsFactors = FALSE)
df_1314 <- read.csv("season-1314_csv.csv", stringsAsFactors = FALSE)
df_1415 <- read.csv("season-1415_csv.csv", stringsAsFactors = FALSE)
df_1516 <- read.csv("season-1516_csv.csv", stringsAsFactors = FALSE)
df_1617 <- read.csv("season-1617_csv.csv", stringsAsFactors = FALSE)
df_1718 <- read.csv("season-1718_csv.csv", stringsAsFactors = FALSE)
df_1819 <- read.csv("season-1819_csv.csv", stringsAsFactors = FALSE)

col_needed <- c("HomeTeam", "AwayTeam", "Date",
                "FTHG", "FTAG", "FTR",
                "HTHG", "HTAG", "HTR",
                "HS", "AS", "HST", "AST",
                "HF", "AF", "HC", "AC",
                "HY", "AY", "HR", "AR" ,
                "B365H", "B365D", "B365A",  ## bet365
                "BWH", "BWD", "BWA",        ## bet and win
                "WHH", "WHD", "WHA",        ## William Hill
                "VCH", "VCD", "VCA")        ## BetVictor

df_0910 <- data.frame(Season="2009-2010" , df_0910[,col_needed])
df_1011 <- data.frame(Season="2010-2011" , df_1011[,col_needed])
df_1112 <- data.frame(Season="2011-2012" , df_1112[,col_needed])
df_1213 <- data.frame(Season="2012-2013" , df_1213[,col_needed])
df_1314 <- data.frame(Season="2013-2014" , df_1314[,col_needed])
df_1415 <- data.frame(Season="2014-2015" , df_1415[,col_needed])
df_1516 <- data.frame(Season="2015-2016" , df_1516[,col_needed])
df_1617 <- data.frame(Season="2016-2017" , df_1617[,col_needed])
df_1718 <- data.frame(Season="2017-2018" , df_1718[,col_needed])
df_1819 <- data.frame(Season="2018-2019" , df_1819[,col_needed])

## Combine Datasets

df_raw <- rbind(df_0910,df_1011, df_1112, df_1213, df_1314,
                df_1415, df_1516, df_1617, df_1718, df_1819)


rm(df_0910,df_1011, df_1112, df_1213, df_1314,
   df_1415, df_1516, df_1617, df_1718, df_1819)

###############################################33
df_raw <- na.omit(df_raw)
df_raw$Season <- as.factor(df_raw$Season)
df_raw$HomeTeam <- as.factor(df_raw$HomeTeam)
df_raw$AwayTeam <- as.factor(df_raw$AwayTeam)
df_raw$FTR <- as.factor(df_raw$FTR)
df_raw$HTR <- as.factor(df_raw$HTR)
cambio <- df_raw$Date[381:length(df_raw$Date)]
dia <- substr(cambio,1,2)
mes <- substr(cambio, 4, 5)
año <- substr(cambio, 7,8)
año2 <- paste0("", sep="20", año)
salida1 <- paste0(año2, sep="-", mes)
salida <- paste0(salida1, sep="-", dia)
df_raw$Date[381:length(df_raw$Date)] <- salida
df_raw$Date <- as.Date(df_raw$Date)

str(df_raw)
View(df_raw)


#################Conjunto de entrenamiento y de test (PLAN B)
index <- df_raw$Season %in% "2018-2019"
test <- df_raw[index,]
train <- df_raw[-index,]


###############Conjunto entrenamiento y test (PLAN A)
partido_random <- test[1,]
partido_random_final <- partido_random
#partidos del equipo local
index_local<- df_raw$HomeTeam %in% partido_random$HomeTeam
df_Local <- df_raw[index_local,]
#partidos del equipo visitante
index_visitante<- df_raw$AwayTeam %in% partido_random$AwayTeam
df_visitante <- df_raw[index_visitante,]
#ultimos partidos local y visitante
index_partidos_anteriores_local <- df_Local$Date < partido_random$Date 
partidos_anteriores_local <- df_Local[index_partidos_anteriores_local,]
index_partidos_anteriores_visit <- df_visitante$Date < partido_random$Date 
partidos_anteriores_visit <- df_visitante[index_partidos_anteriores_visit,]

inicio_local <- nrow(partidos_anteriores_local)-3
fin_local <- nrow(partidos_anteriores_local) - 0
inicio_visit <- nrow(partidos_anteriores_visit)-3
fin_visit <- nrow(partidos_anteriores_visit) - 0

inicio_local_2 <- nrow(partidos_anteriores_local)/2 - 0
fin_local_2 <- nrow(partidos_anteriores_local) - 0
inicio_visit_2 <- nrow(partidos_anteriores_visit)/2 - 0
fin_visit_2 <- nrow(partidos_anteriores_visit) - 0
###Tiros
partido_random_final$HS <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HS))
partido_random_final$AS <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AS))
#Tiros a puerta
partido_random_final$HST <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HST))
partido_random_final$AST <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AST))
#Faltas
partido_random_final$HF <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HF))
partido_random_final$AF <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AF))
#Corner
partido_random_final$HC <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HF))
partido_random_final$AC <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AF))
#Tarjetas amarillas
partido_random_final$HY <- sample(partidos_anteriores_local[c(inicio_local_2:fin_local_2),]$HY,1)
partido_random_final$AY <- sample(partidos_anteriores_visit[c(inicio_visit_2:fin_visit_2),]$HY,1)
#Tarjetas rojas
partido_random_final$HR <- sample(partidos_anteriores_local[c(inicio_local_2:fin_local_2),]$HR,1)
partido_random_final$AR <- sample(partidos_anteriores_visit[c(inicio_visit_2:fin_visit_2),]$HR,1)

test_final <- test
###############BUCLE
for(i in 1:380){
  partido_random <- test[i,]
  partido_random_final <- partido_random
  #partidos del equipo local
  index_local<- df_raw$HomeTeam %in% partido_random$HomeTeam
  df_Local <- df_raw[index_local,]
  #partidos del equipo visitante
  index_visitante<- df_raw$AwayTeam %in% partido_random$AwayTeam
  df_visitante <- df_raw[index_visitante,]
  #ultimos partidos local y visitante
  index_partidos_anteriores_local <- df_Local$Date < partido_random$Date 
  partidos_anteriores_local <- df_Local[index_partidos_anteriores_local,]
  index_partidos_anteriores_visit <- df_visitante$Date < partido_random$Date 
  partidos_anteriores_visit <- df_visitante[index_partidos_anteriores_visit,]
  
  inicio_local <- nrow(partidos_anteriores_local)-3
  fin_local <- nrow(partidos_anteriores_local) - 0
  inicio_visit <- nrow(partidos_anteriores_visit)-3
  fin_visit <- nrow(partidos_anteriores_visit) - 0
  
  inicio_local_2 <- nrow(partidos_anteriores_local)/2 - 0
  fin_local_2 <- nrow(partidos_anteriores_local) - 0
  inicio_visit_2 <- nrow(partidos_anteriores_visit)/2 - 0
  fin_visit_2 <- nrow(partidos_anteriores_visit) - 0
  ###Tiros
  partido_random_final$HS <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HS))
  partido_random_final$AS <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AS))
  #Tiros a puerta
  partido_random_final$HST <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HST))
  partido_random_final$AST <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AST))
  #Faltas
  partido_random_final$HF <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HF))
  partido_random_final$AF <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AF))
  #Corner
  partido_random_final$HC <- round(mean(partidos_anteriores_local[c(inicio_local:fin_local),]$HF))
  partido_random_final$AC <- round(mean(partidos_anteriores_visit[c(inicio_visit:fin_visit),]$AF))
  #Tarjetas amarillas
  partido_random_final$HY <- sample(partidos_anteriores_local[c(inicio_local_2:fin_local_2),]$HY,1)
  partido_random_final$AY <- sample(partidos_anteriores_visit[c(inicio_visit_2:fin_visit_2),]$HY,1)
  #Tarjetas rojas
  partido_random_final$HR <- sample(partidos_anteriores_local[c(inicio_local_2:fin_local_2),]$HR,1)
  partido_random_final$AR <- sample(partidos_anteriores_visit[c(inicio_visit_2:fin_visit_2),]$HR,1)
  test_final[i,] <- partido_random_final
}
