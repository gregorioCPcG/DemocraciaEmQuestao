# 
# Limpar o Global Environment
rm(list = ls())
library(tidyverse) # 
library(sjPlot) # pacote para visualizar os modelos
library(marginaleffects) # pacote para valores preditos
library(see) # para deixar fins estéticos
library(scales)# para recodificações
library(performance)

df <- read_csv("df.csv")# base de dados
table(df$Amostra)# amostras (para experimento, para uso posterior)

# criar uma base para cada variavel independente ()
# antes porém, devemos recodificar as variáveis de controle e a(s) dependente(s) [voto e feeling]

# portanto, se inicia com essa recodificação das variáveis presentes em todos os modelos
library(memisc)
table(df$P62, useNA = "always")
df$voto <- memisc::recode(as.factor(df$P62), "Bolsonaro" <- c(4),
                          "Lula" <- c(6),
                          "Outros/Abstenção"<-c(1,2,3,5,7,8,9,10,11,12,13,14,15,NA))

df$voto <- relevel(df$voto, ref = "Outros/Abstenção")
table(df$voto, useNA = "always")
table(df$P12)#escolarid ok
df$Escolaridade <- df$P12
table(df$P6)
df$Mulher <- df$P6 == 2
summary(df$P8)
df$Idade <- df$P8
table(df$P60)
df$Renda <- df$P60
table(df$P7)
df$raca_branca <- df$P7 == 1

table(df$P57)
df$DESinteresse_ <- df$P57
df$DESinteresse_[df$DESinteresse_  == 5] <- NA
df$DESinteresse_[df$DESinteresse_  == 6] <- NA
-1* - df$DESinteresse_ -> df$interesse
table(df$DESinteresse_)
table(df$interesse)#Ok



# Feeling

table(df$P18)
# Verifica se df$P18 está entre 8 e 10 e atribui TRUE ou FALSE a df$GostaMuitoPT
df$GostaMuitoPT <- df$P18 >= 8 & df$P18 <= 10
# Converta os TRUEs e FALSEs em 1s e 0s, se preferir
df$GostaMuitoPT <- as.numeric(df$GostaMuitoPT)
table(df$GostaMuitoPT, useNA = "always")# 
table(df$P20)
# Verifica se df$P20 está entre 8 e 10 e atribui TRUE ou FALSE a df$GostaMuitoPL
df$GostaMuitoPL <- df$P20 >= 8 & df$P20 <= 10
# Converta os TRUEs e FALSEs em 1s e 0s, se preferir
df$GostaMuitoPL <- as.numeric(df$GostaMuitoPL)
table(df$GostaMuitoPL)# 

table(df$GostaMuitoPL, df$GostaMuitoPT)# excluir os 21 que Gostam muito dos dois, 21 casos
# 
df$GostaMuitoPT <- ifelse(df$GostaMuitoPL == 1 & df$GostaMuitoPT == 1, NA, df$GostaMuitoPT)
df$GostaMuitoPL <- ifelse(df$GostaMuitoPL == 1 & df$GostaMuitoPT == 1, NA, df$GostaMuitoPL)
table(df$GostaMuitoPT, useNA= "always")
table(df$GostaMuitoPL, useNA= "always")
table(df$GostaMuitoPL, df$GostaMuitoPT, useNA = "always")#conferir
# Criar uma nova variável "feeling" do tipo factor
df$feeling <- factor("Neutro", levels = c("Neutro", "Gosta Muito PL", "Gosta Muito PT"))
# Atualizar os valores de "feeling" de acordo com as condições
df$feeling[df$GostaMuitoPL == 1 & df$GostaMuitoPT == 0] <- "Gosta Muito PL"
df$feeling[df$GostaMuitoPT == 1 & df$GostaMuitoPL == 0] <- "Gosta Muito PT"
table(df$feeling)



#
#agora sim criar as três bases separadas
df1 <- subset(df, select=c(voto,feeling, Escolaridade,Mulher,
                           Idade, Renda, raca_branca, interesse,P50))
df2 <- subset(df, select=c(voto,feeling,Escolaridade, Idade, Renda, raca_branca, interesse, Mulher,
                           P51,P52,P53,P54))# arrumar mulher lá na recodificaç~~ao
df3 <- subset(df, select=c(voto,feeling,Mulher,
                           Escolaridade, Idade, Renda, raca_branca, interesse, P22,P24))
# Apoio Explicíto #########

#recodificar P50 (churchilliana)
table(df1$P50,useNA = "always")
# como recodificar?
# 1. Para pessoas como eu, tanto faz um regime democrático ou um não democrático OU 
# 2. A democracia é preferível a qualquer outra forma de governo OU
# 3. Em algumas circunstâncias, um governo autoritário pode ser preferível a um democrático?
# 4. (ESPONTÂNEA) Não sabe
# 5. (ESPONTÂNEA) Não respondeu 

#optei pela binarização (sendo 2 = 1 [TRUE], demais valores = 0 [FALSE], 
# aí não levei em consideração os NAS que são 4 e 5)

df1$dep1 <- df$P50 == 2
table(df1$dep1)
df1$dep1  <- as.numeric(df1$dep1)
table(df1$dep1, useNA = "always")

#modelos glm
#voto - categoria referência (Outros/Abstenção)

modelo1 <- glm(dep1 ~ voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
               data = df1, family=binomial(link=logit))#consulta apenas

tab_model(modelo1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoioexplicito1 <- plot_predictions(modelo1, condition=c("voto")) + theme_bw()+labs(x="Voto",
                                                                                    y= "Apoio Explicíto")
apoioexplicito1# objeto salvo_ verificar


apoioexplicito1+coord_flip()



modelo12 <- glm(dep1 ~ feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                data = df1, family=binomial(link=logit))#consulta apenas

tab_model(modelo12, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoioexplicito12 <- plot_predictions(modelo12, condition=c("feeling")) + theme_bw()+labs(x="Sentimento partidário",
                                                                                         y= "Apoio Explicíto")
apoioexplicito12# objeto salvo_ verificar


# tabela
mean(df1$dep1)
sd(df1$dep1)
by(df1$dep1, df1$voto, mean, na.rm=T)
by(df1$dep1, df1$voto, sd, na.rm=T)
by(df1$dep1, df1$feeling, mean, na.rm=T)
by(df1$dep1, df1$feeling, sd, na.rm=T)
table(df$voto)
table(df$feeling)
# apoio aos principios ########
summary(df2)
summary(df2[9:12])
df2[9:12][df2[9:12] > 10] <- NA # para todas que vão de 0 a 10


summary(df2[9:12])

#a ideia é recodificar de modo que os valores democráticos fiquem maiores

####
# P51
# onde 0 significa “nada justificável” e 10 “totalmente justificável”, o quanto o(a) Sr(a) acredita que quando o país está enfrentando dificuldades é justificável que o presidente da República feche o Congresso e governe sem o Congresso?
# inverter 
table(df2$P51, useNA="always")
df2$P51_invert <- -1*df2$P51 #primeiro inverter
df2$P51_invert <- scales::rescale(df2$P51_invert, to = c(0, 10)) # depois colocar entre 0 e 10
table(df2$P51_invert, useNA="always")# conferir se inverteu
df2 <- subset(df2, select=-c(P51))


#P53 - 
# 0 significa “desaprova fortemente” e 10 “aprova fortemente”, até que ponto o(a) Sr(a) aprova a participação de pessoas em manifestações permitidas por lei?
# já ta ok, não precisa recod
table(df2$P53, useNA = "always")


#P54
# Em uma escala que vai de 0 a 10, onde 0 significa “concordo totalmente que, para poder prender criminosos, autoridades podem agir sem respeitar a lei” e 10 “concordo totalmente que, para poder prender criminosos, elas devem sempre respeitar as leis”
# já ta ok, não precisa recod
table(df2$P54, useNA = "always")

#P52 
#Em uma escala de 0 a 10, onde 0 é “concordo fortemente que necessitamos de um líder forte que não seja eleito através do voto” e 10 é “concordo fortemente que a democracia eleitoral é o melhor”
#
table(df2$P52, useNA = "always")

# TRI e AFC
#TRI
# binarizar para mirt
#TRI
df4 <- df2[9:12]
summary(df4)
# Binarizar as variáveis P52, P53, P54 e P51_invert em df4
df4$P52_bin <- ifelse(df4$P52 >= 8, 1, 0)
df4$P53_bin <- ifelse(df4$P53 >= 8, 1, 0)
df4$P54_bin <- ifelse(df4$P54 >= 8, 1, 0)
df4$P51_invert_bin <- ifelse(df4$P51_invert >= 8, 1, 0)
df4 <- df4[5:8]


library(mirt)
tri1 <- mirt(df4, 1, itemtype = 'graded')#Iteration: 205, Log-Lik: -10533.948, Max-Change: 0.00006
summary(tri1)
#

df4 <- df2[9:12]
library(lavaan)
# Definir o modelo de análise fatorial confirmatória
AFCmodel <- '
  fator =~ P52 + P53 + P54 + P51_invert
'
# Realizar a análise fatorial confirmatória
cfa_result <- cfa(AFCmodel, data = df4)
# Visualizar os resultados da análise fatorial confirmatória
summary(cfa_result, standardized = TRUE)
library(semTools)
semTools::fitmeasures(cfa_result, c("tli", "cfi", "rmsea", "srmr"))
#
scores <- lavPredict(cfa_result)
hist(scores)
nrow(df4)#base cheia
nrow(scores)
1500-1339#161  casos perdidos
summary(scores)

# Realizar a análise fatorial confirmatória
cfa_result <- cfa(AFCmodel, data = df4, missing = "available.cases")# tornar NA e dar 1500

# Calcular os escores fatoriais
scores <- lavPredict(cfa_result)
summary(scores)# 161 NAS
hist(scores)

#scores
# Calcular os escores fatoriais e manter NA para observações sem scores

scores <- as.numeric(scores)
scores -> df2$dep2
summary(df2)

#modelos
modelo2 <- lm(dep2~voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
              data = df2)
modelo22 <-lm(dep2~feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
              data = df2)


tab_model(modelo2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoio_principios1 <- plot_predictions(modelo2, condition=c("voto")) + theme_bw()+labs(x="Voto",
                                                                                      y= "Apoio aos princípios - Democracia Liberal")
apoio_principios1# objeto salvo_ verificar


tab_model(modelo22, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoio_principios2 <- plot_predictions(modelo22, condition=c("feeling")) + theme_bw()+labs(x="Sentimento partidário",
                                                                                          y= "Apoio aos princípios - Democracia Liberal")
apoio_principios2# objeto salvo_ verificar


# robustez

modeloP51invert_1<- lm(P51_invert~voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                       data = df2)
modeloP51_2<- lm(P51_invert~feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modeloP52_1<- lm(P52~voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modeloP52_2<- lm(P52~feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modeloP53_1<- lm(P53~voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modeloP53_2<- lm(P53~feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modeloP54_1<- lm(P54~voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modeloP54_2<- lm(P54~feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)

tab_model(modeloP51invert_1,modeloP51_2,modeloP52_1, modeloP52_2,
          modeloP53_1,modeloP53_2,modeloP54_1, modeloP54_2,
          show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")



#tabela cruzada

summary(df2$dep2)
sd(df2$dep2, na.rm=T)
by(df2$dep2, df2$voto, mean,na.rm=T)
by(df2$dep2, df2$voto, quantile, probs = c(0.25, 0.75), na.rm = TRUE)
by(df2$dep2, df2$voto, min,na.rm=T)
by(df2$dep2, df2$voto, max,na.rm=T)
by(df2$dep2, df2$voto, sd,na.rm=T)

by(df2$dep2, df2$feeling, mean,na.rm=T)
by(df2$dep2, df2$feeling, quantile, probs = c(0.25, 0.75), na.rm = TRUE)
by(df2$dep2, df2$feeling, min,na.rm=T)
by(df2$dep2, df2$feeling, max,na.rm=T)
by(df2$dep2, df2$feeling, sd,na.rm=T)


# Tolerância ao rival ####################

df3
df3[9:10][df3[9:10] > 10] <- NA # para todas que vão de 0 a 10
summary(df3)

#voto
df3voto<- subset(df3, voto == "Lula" | voto == "Bolsonaro")
table(df3voto$feeling)
table(df3voto$voto)
df3voto$votoBolsonaro <- df3voto$voto=="Bolsonaro"
df3voto$votoLula <- df3voto$voto=="Lula"
df3voto$toleraPT <- df3voto$P22 >= 8 & df3voto$P22 <= 10
df3voto$toleraPL <- df3voto$P24 >= 8 & df3voto$P24 <= 10
table(df3voto$votoBolsonaro)
table(df3voto$votoLula)
table(df3voto$P22)
table(df3voto$toleraPT)
table(df3voto$P24)
table(df3voto$toleraPL)

df3votoBolsonaro <- subset(df3voto, voto == "Bolsonaro")
df3votoLula <- subset(df3voto, voto == "Lula")

df3votoLula$LulistaqueToleraRival <- ifelse(df3votoLula$votoLula & df3votoLula$toleraPL, 1, 0)
table(df3votoLula$LulistaqueToleraRival)
135+363 #=498
prop.table(table(df3votoLula$LulistaqueToleraRival))
df3votoBolsonaro$BolsonaristaqueToleraRival <- ifelse(df3votoBolsonaro$votoBolsonaro & df3votoBolsonaro$toleraPT, 1, 0)
table(df3votoBolsonaro$BolsonaristaqueToleraRival)
409+81 #490
prop.table(table(df3votoBolsonaro$BolsonaristaqueToleraRival))



df3voto$LulistaqueToleraRival <- ifelse(df3voto$votoLula & df3voto$toleraPL, 1, 0)
df3voto$BolsonaristaqueToleraRival <- ifelse(df3voto$votoBolsonaro & df3voto$toleraPT, 1, 0)
df3voto$ToleranteAoRival <- ifelse(df3voto$LulistaqueToleraRival | df3voto$BolsonaristaqueToleraRival, 1, 0) 
table(df3voto$ToleranteAoRival)
135+81
#deu certo
772+216
prop.table(table(df3voto$ToleranteAoRival))



#feeling
df3feeling<- subset(df3, feeling == "Gosta Muito PL" | feeling == "Gosta Muito PT")
table(df3feeling$feeling)
table(df3feeling$voto)
df3feeling$feelingPartidoLiberal <- df3feeling$feeling=="Gosta Muito PL"
df3feeling$feelingPT <- df3feeling$feeling=="Gosta Muito PT"
df3feeling$toleraPT <- df3feeling$P22 >= 8 & df3feeling$P22 <= 10
df3feeling$toleraPL <- df3feeling$P24 >= 8 & df3feeling$P24 <= 10
table(df3feeling$feelingPartidoLiberal)
table(df3feeling$feelingPT)
table(df3feeling$P22)
table(df3feeling$toleraPT)
table(df3feeling$P24)
table(df3feeling$toleraPL)

df3feelingPartidoLiberal <- subset(df3feeling, feeling == "Gosta Muito PL")
df3feelingPT <- subset(df3feeling, feeling == "Gosta Muito PT")

table(df3feelingPT$feelingPT)


df3feelingPT$LulistaqueToleraRival <- ifelse(df3feelingPT$feelingPT & df3feelingPT$toleraPL, 1, 0)
table(df3feelingPT$LulistaqueToleraRival)
346+96 #=442
prop.table(table(df3feelingPT$LulistaqueToleraRival))
df3feelingPartidoLiberal$BolsonaristaqueToleraRival <- ifelse(df3feelingPartidoLiberal$feelingPartidoLiberal & df3feelingPartidoLiberal$toleraPT, 1, 0)
table(df3feelingPartidoLiberal$BolsonaristaqueToleraRival)
279+28 #307 
prop.table(table(df3feelingPartidoLiberal$BolsonaristaqueToleraRival))



df3feeling$LulistaqueToleraRival <- ifelse(df3feeling$feelingPT & df3feeling$toleraPL, 1, 0)
df3feeling$BolsonaristaqueToleraRival <- ifelse(df3feeling$feelingPartidoLiberal & df3feeling$toleraPT, 1, 0)
df3feeling$ToleranteAoRival <- ifelse(df3feeling$LulistaqueToleraRival | df3feeling$BolsonaristaqueToleraRival, 1, 0) 
table(df3feeling$ToleranteAoRival)
28+96
#deu certo
625+124
prop.table(table(df3feeling$ToleranteAoRival))

#deps
table(df1$dep1)
prop.table(table(df1$dep1))#apoio explicíto

table(df3voto$ToleranteAoRival)
prop.table(table(df3voto$ToleranteAoRival))#Dep 3 : Tolerante ao rival - base de dados com votos em Lula ou Bolsonaro

table(df3feeling$ToleranteAoRival)
prop.table(table(df3feeling$ToleranteAoRival))#Dep 3 : Tolerante ao rival - base de dados com AQUELES QUE GOSTAM MUITO DO PL OU DO PT

# Modelos ##########

table(df3voto$voto)
table(df3feeling$feeling)
df3voto$voto<- droplevels(df3voto$voto)
df3feeling$feeling<- droplevels(df3feeling$feeling)
table(df3voto$voto)
table(df3feeling$feeling)
# categorias de referência Bolsonaro e Gosta Muito PL

#deps
table(df3voto$ToleranteAoRival)
table(df3feeling$ToleranteAoRival)

modelo3voto <- glm(ToleranteAoRival ~ voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                   data = df3voto, family=binomial(link=logit))

modelo3feeling <- glm(ToleranteAoRival ~ feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                      data = df3feeling, family=binomial(link=logit))

tab_model(modelo3voto, modelo3feeling, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoiotolerancia1 <- plot_predictions(modelo3voto, condition=c("voto")) + theme_bw()+labs(x="Voto",
                                                                                         y= "Tolerância ao direito de concorrer do partido Rival")
apoiotolerancia1# objeto salvo_ verificar

apoiotolerancia2 <- plot_predictions(modelo3feeling, condition=c("feeling")) + theme_bw()+labs(x="Sentimento partidário",
                                                                                               y= "Tolerância ao direito de concorrer do partido Rival")
apoiotolerancia2# objeto salvo_ verificar


# Modelos (nao usar esses)
tab_model(modelo1, modelo2, modelo3voto, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

tab_model(modelo12, modelo22, modelo3feeling, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
# plots (não usar esse apenas consulta)
plots(apoioexplicito1, apoio_principios1, apoiotolerancia1)
plots(apoioexplicito12, apoio_principios2, apoiotolerancia2)


#modelo 2 alternativo com factor scores do TRI
# obter factor scores de mirt
escores_fatoriaiscomTRI <- fscores(tri1)
df2$dep2TRI <- escores_fatoriaiscomTRI

hist(escores_fatoriaiscomTRI)

# e rodar modelos
modelo22_ <- lm(dep2TRI~voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                data = df2)
modelo32 <-lm(dep2TRI~feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
              data = df2)


tab_model(modelo22_, modelo32, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


# diagnóstico modelos
modelo1
modelo12 -> modelo11
modelo22 -> modelo12
modelo3voto -> modelo3
modelo3feeling -> modelo13

#modelos lm
modelo2
modelo12

check_heteroscedasticity(modelo2)
check_heteroscedasticity(modelo12)
check_model(modelo2)
check_model(modelo12)
model_performance(modelo2)
model_performance(modelo12)
plot(compare_performance(modelo2, modelo12, rank = TRUE, verbose = FALSE))

# Teste de Shapiro-Wilk para normalidade dos resíduos do modelo
shapiro.test(resid(modelo2))
shapiro.test(resid(modelo12))

# Gráfico QQ plot para verificar normalidade dos resíduos
qqnorm(resid(modelo2))
qqline(resid(modelo2), col = 2)  # Adiciona linha de referência
qqnorm(resid(modelo12))
qqline(resid(modelo12), col = 2)  # Adiciona linha de referência

library(olsrr)

ols_vif_tol(modelo2)
ols_vif_tol(modelo12)
ols_eigen_cindex(modelo2)
ols_eigen_cindex(modelo12)
model_performance(modelo2)
model_performance(modelo12)





#modelos glm
model_performance(modelo1)
model_performance(modelo11)
model_performance(modelo3)
model_performance(modelo13)
plot(compare_performance(modelo1, modelo11,
                         modelo3,modelo13, rank = TRUE, verbose = FALSE))


ols_vif_tol(modelo1)
ols_vif_tol(modelo11)
ols_vif_tol(modelo3)
ols_vif_tol(modelo13)


# modelos 22 e 32

model_performance(modelo22_)
model_performance(modelo32)
qqnorm(resid(modelo22_))
qqline(resid(modelo22_), col = 2)  # Adiciona linha de referência
qqnorm(resid(modelo32))
qqline(resid(modelo32), col = 2)  # Adiciona linha de referência
qqnorm(resid(modelo22_))
qqline(resid(modelo22_), col = 2)  # Adiciona linha de referência
qqnorm(resid(modelo32))
qqline(resid(modelo32), col = 2)  # Adiciona linha de referência
ols_vif_tol(modelo22_)
ols_vif_tol(modelo32)
ols_eigen_cindex(modelo22_)
ols_eigen_cindex(modelo32)




# novos modelos para dep 2 (padronizada 1 a 10) #######
df2

df2$dep2_padron <- scales::rescale(df2$dep2, to = c(0, 10)) # depois colocar entre 0 e 10



hist(df2$dep2_padron, breaks = seq(0, 10, by = 1))

#tabela cruzada

summary(df2$dep2_padron)
sd(df2$dep2_padron, na.rm=T)
by(df2$dep2_padron, df2$voto, mean,na.rm=T)
by(df2$dep2_padron, df2$voto, median,na.rm=T)
by(df2$dep2_padron, df2$voto, quantile, probs = c(0.25, 0.75), na.rm = TRUE)
by(df2$dep2_padron, df2$voto, min,na.rm=T)
by(df2$dep2_padron, df2$voto, max,na.rm=T)
by(df2$dep2_padron, df2$voto, sd,na.rm=T)

by(df2$dep2_padron, df2$feeling, mean,na.rm=T)
by(df2$dep2_padron, df2$feeling, median,na.rm=T)
by(df2$dep2_padron, df2$feeling, quantile, probs = c(0.25, 0.75), na.rm = TRUE)
by(df2$dep2_padron, df2$feeling, min,na.rm=T)
by(df2$dep2_padron, df2$feeling, max,na.rm=T)
by(df2$dep2_padron, df2$feeling, sd,na.rm=T)

# novos modelos agora

modelo2b <- lm(dep2_padron~voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
               data = df2)
modelo22b <-lm(dep2_padron~feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
               data = df2)




# postar de novo modelos #########
tab_model(modelo1, modelo2b, modelo3voto, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

tab_model(modelo12, modelo22b, modelo3feeling, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")


# valores preditos #########
apoio_principios1b <- plot_predictions(modelo2b, condition=c("voto")) + theme_bw()+labs(x="Voto",
                                                                                        y= "Apoio aos princípios - Democracia Liberal")
apoio_principios1b# objeto salvo_ verificar


tab_model(modelo22b, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoio_principios2b <- plot_predictions(modelo22b, condition=c("feeling")) + theme_bw()+labs(x="Sentimento partidário",
                                                                                            y= "Apoio aos princípios - Democracia Liberal")
apoio_principios2b# objeto salvo_ verificar

#plotar de novo valores preditos (de outra forma sugerida) ########

plots(apoioexplicito1, apoioexplicito12)
plots(apoio_principios1b, apoio_principios2b)
plots(apoiotolerancia1, apoiotolerancia2)

# diagnosis

model_performance(modelo22b)
model_performance(modelo2b)
qqnorm(resid(modelo22b))
qqline(resid(modelo22b), col = 2)  # Adiciona linha de referência
qqnorm(resid(modelo2b))
qqline(resid(modelo2b), col = 2)  # Adiciona linha de referência

ols_vif_tol(modelo22b)
ols_vif_tol(modelo2b)
ols_eigen_cindex(modelo22b)
ols_eigen_cindex(modelo2b)


# testes de média v#######################
#- dados da tabela cruzada mas com teste de média




#dep 1
# Calcular a média
media <- mean(df1$dep1, na.rm = TRUE)

# Calcular o desvio padrão
desvio_padrao <- sd(df1$dep1, na.rm = TRUE)

# Número de observações
n <- sum(!is.na(df1$dep1))

# Calcular o erro padrão da média
erro_padrao <- desvio_padrao / sqrt(n)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%)
erro_padrao_95 <- qt(0.975, df = n - 1) * erro_padrao
ic_inferior <- media - erro_padrao_95
ic_superior <- media + erro_padrao_95
# Exibir resultados
cat("dep1:", media, "\n")
cat("Intervalo de confiança (95%): [", ic_inferior, ", ", ic_superior, "]\n") 

table(df1$voto)
# Dividir o dataframe em dois baseado na categoria de voto
df_voto1 <- df1[df1$voto == "Bolsonaro", ]
df_voto2 <- df1[df1$voto == "Lula", ]

# Calcular as estatísticas para cada grupo
media_voto1 <- mean(df_voto1$dep1, na.rm = TRUE)
media_voto2 <- mean(df_voto2$dep1, na.rm = TRUE)

desvio_padrao_voto1 <- sd(df_voto1$dep1, na.rm = TRUE)
desvio_padrao_voto2 <- sd(df_voto2$dep1, na.rm = TRUE)

n_voto1 <- sum(!is.na(df_voto1$dep1))
n_voto2 <- sum(!is.na(df_voto2$dep1))

# Calcular o erro padrão da média para cada grupo
erro_padrao_voto1 <- desvio_padrao_voto1 / sqrt(n_voto1)
erro_padrao_voto2 <- desvio_padrao_voto2 / sqrt(n_voto2)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%) para cada grupo
erro_padrao_95_voto1 <- qt(0.975, df = n_voto1 - 1) * erro_padrao_voto1
ic_inferior_voto1 <- media_voto1 - erro_padrao_95_voto1
ic_superior_voto1 <- media_voto1 + erro_padrao_95_voto1

erro_padrao_95_voto2 <- qt(0.975, df = n_voto2 - 1) * erro_padrao_voto2
ic_inferior_voto2 <- media_voto2 - erro_padrao_95_voto2
ic_superior_voto2 <- media_voto2 + erro_padrao_95_voto2


# Exibir resultados
cat("Categoria 1 Bolsonaro (voto):", media_voto1, "\n")
cat("Intervalo de confiança (95%) para categoria 1: [", ic_inferior_voto1, ", ", ic_superior_voto1, "]\n\n")

cat("Categoria 2 Lula (voto):", media_voto2, "\n")
cat("Intervalo de confiança (95%) para categoria 2: [", ic_inferior_voto2, ", ", ic_superior_voto2, "]\n")



table(df1$feeling)
# Dividir o dataframe em dois baseado na categoria de feeling
df_feeling1 <- df1[df1$feeling == "Gosta Muito PL", ]
df_feeling2 <- df1[df1$feeling == "Gosta Muito PT", ]

# Calcular as estatísticas para cada grupo
media_feeling1 <- mean(df_feeling1$dep1, na.rm = TRUE)
media_feeling2 <- mean(df_feeling2$dep1, na.rm = TRUE)

desvio_padrao_feeling1 <- sd(df_feeling1$dep1, na.rm = TRUE)
desvio_padrao_feeling2 <- sd(df_feeling2$dep1, na.rm = TRUE)

n_feeling1 <- sum(!is.na(df_feeling1$dep1))
n_feeling2 <- sum(!is.na(df_feeling2$dep1))

# Calcular o erro padrão da média para cada grupo
erro_padrao_feeling1 <- desvio_padrao_feeling1 / sqrt(n_feeling1)
erro_padrao_feeling2 <- desvio_padrao_feeling2 / sqrt(n_feeling2)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%) para cada grupo
erro_padrao_95_feeling1 <- qt(0.975, df = n_feeling1 - 1) * erro_padrao_feeling1
ic_inferior_feeling1 <- media_feeling1 - erro_padrao_95_feeling1
ic_superior_feeling1 <- media_feeling1 + erro_padrao_95_feeling1

erro_padrao_95_feeling2 <- qt(0.975, df = n_feeling2 - 1) * erro_padrao_feeling2
ic_inferior_feeling2 <- media_feeling2 - erro_padrao_95_feeling2
ic_superior_feeling2 <- media_feeling2 + erro_padrao_95_feeling2

# Exibir resultados
cat("Categoria 1 (feeling):", media_feeling1, "\n")
cat("Intervalo de confiança (95%) para categoria 1: [", ic_inferior_feeling1, ", ", ic_superior_feeling1, "]\n\n")

cat("Categoria 2 (feeling):", media_feeling2, "\n")
cat("Intervalo de confiança (95%) para categoria 2: [", ic_inferior_feeling2, ", ", ic_superior_feeling2, "]\n")


X <- c("Média Amostra","Votou Bolsonaro","Votou Lula", "Gosta Muito PL", "Gosta Muito PT")
media <-c(0.7126667,0.6964286 ,0.7480769,0.6518987,  0.7242888 )
lower <- c(0.6897403,0.6561495,0.7106413,0.5990898,0.6831641)
upper <- c(0.735593,0.7367076,0.7855126,0.7047077,0.7654136 )
colorir <-c("1","2","3","2","3")
cores2 <- c("black","darkgreen", "darkred")
dadosmediadep1 <- data.frame(X,media,lower,upper,colorir)
dadosmediadep1 $X <- factor(dadosmediadep1$X,
                            levels = c("Média Amostra",
                                       "Votou Bolsonaro",
                                       "Votou Lula",
                                       "Gosta Muito PL",
                                       "Gosta Muito PT"))
dadosmediadep1
graficomediadep1 <- ggplot(dadosmediadep1, aes(x = X, y = media, color=colorir)) +
  geom_point(size = 3) +  # Adicionar pontos para as estimativas
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) +  # Adicionar barras de erro
  coord_flip() +  # Inverter os eixos x e y
  theme_minimal() +  # Aplicar um tema mínimo ao gráfico
  geom_text(aes(label = sprintf("%.2f", media)), hjust = 0.5, size = 3, vjust=-0.9) +
  labs(x = "Apoio Explícito",
       y = NULL,
       title = "")+
  theme(legend.position = "none")+
  scale_color_manual(values = cores2)#+ scale_y_continuous(limits = c(0.162, 0.634))
graficomediadep1


dadosmediadep1


#dep 2
df2$dep2_padron
# Calcular a média
media <- mean(df2$dep2_padron, na.rm = TRUE)

# Calcular o desvio padrão
desvio_padrao <- sd(df2$dep2_padron, na.rm = TRUE)

# Número de observações
n <- sum(!is.na(df2$dep2_padron))

# Calcular o erro padrão da média
erro_padrao <- desvio_padrao / sqrt(n)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%)
erro_padrao_95 <- qt(0.975, df = n - 1) * erro_padrao
ic_inferior <- media - erro_padrao_95
ic_superior <- media + erro_padrao_95
# Exibir resultados
cat("dep1:", media, "\n")
cat("Intervalo de confiança (95%): [", ic_inferior, ", ", ic_superior, "]\n") 

table(df2$voto)
# Dividir o dataframe em dois baseado na categoria de voto
df_voto1 <- df2[df2$voto == "Bolsonaro", ]
df_voto2 <- df2[df2$voto == "Lula", ]

# Calcular as estatísticas para cada grupo
media_voto1 <- mean(df_voto1$dep2_padron, na.rm = TRUE)
media_voto2 <- mean(df_voto2$dep2_padron, na.rm = TRUE)

desvio_padrao_voto1 <- sd(df_voto1$dep2_padron, na.rm = TRUE)
desvio_padrao_voto2 <- sd(df_voto2$dep2_padron, na.rm = TRUE)

n_voto1 <- sum(!is.na(df_voto1$dep2_padron))
n_voto2 <- sum(!is.na(df_voto2$dep2_padron))

# Calcular o erro padrão da média para cada grupo
erro_padrao_voto1 <- desvio_padrao_voto1 / sqrt(n_voto1)
erro_padrao_voto2 <- desvio_padrao_voto2 / sqrt(n_voto2)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%) para cada grupo
erro_padrao_95_voto1 <- qt(0.975, df = n_voto1 - 1) * erro_padrao_voto1
ic_inferior_voto1 <- media_voto1 - erro_padrao_95_voto1
ic_superior_voto1 <- media_voto1 + erro_padrao_95_voto1

erro_padrao_95_voto2 <- qt(0.975, df = n_voto2 - 1) * erro_padrao_voto2
ic_inferior_voto2 <- media_voto2 - erro_padrao_95_voto2
ic_superior_voto2 <- media_voto2 + erro_padrao_95_voto2


# Exibir resultados
cat("Categoria 1 Bolsonaro (voto):", media_voto1, "\n")
cat("Intervalo de confiança (95%) para categoria 1: [", ic_inferior_voto1, ", ", ic_superior_voto1, "]\n\n")

cat("Categoria 2 Lula (voto):", media_voto2, "\n")
cat("Intervalo de confiança (95%) para categoria 2: [", ic_inferior_voto2, ", ", ic_superior_voto2, "]\n")



table(df2$feeling)
# Dividir o dataframe em dois baseado na categoria de feeling
df_feeling1 <- df2[df2$feeling == "Gosta Muito PL", ]
df_feeling2 <- df2[df2$feeling == "Gosta Muito PT", ]

# Calcular as estatísticas para cada grupo
media_feeling1 <- mean(df_feeling1$dep2_padron, na.rm = TRUE)
media_feeling2 <- mean(df_feeling2$dep2_padron, na.rm = TRUE)

desvio_padrao_feeling1 <- sd(df_feeling1$dep2_padron, na.rm = TRUE)
desvio_padrao_feeling2 <- sd(df_feeling2$dep2_padron, na.rm = TRUE)

n_feeling1 <- sum(!is.na(df_feeling1$dep2_padron))
n_feeling2 <- sum(!is.na(df_feeling2$dep2_padron))

# Calcular o erro padrão da média para cada grupo
erro_padrao_feeling1 <- desvio_padrao_feeling1 / sqrt(n_feeling1)
erro_padrao_feeling2 <- desvio_padrao_feeling2 / sqrt(n_feeling2)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%) para cada grupo
erro_padrao_95_feeling1 <- qt(0.975, df = n_feeling1 - 1) * erro_padrao_feeling1
ic_inferior_feeling1 <- media_feeling1 - erro_padrao_95_feeling1
ic_superior_feeling1 <- media_feeling1 + erro_padrao_95_feeling1

erro_padrao_95_feeling2 <- qt(0.975, df = n_feeling2 - 1) * erro_padrao_feeling2
ic_inferior_feeling2 <- media_feeling2 - erro_padrao_95_feeling2
ic_superior_feeling2 <- media_feeling2 + erro_padrao_95_feeling2

# Exibir resultados
cat("Categoria 1 (feeling):", media_feeling1, "\n")
cat("Intervalo de confiança (95%) para categoria 1: [", ic_inferior_feeling1, ", ", ic_superior_feeling1, "]\n\n")

cat("Categoria 2 (feeling):", media_feeling2, "\n")
cat("Intervalo de confiança (95%) para categoria 2: [", ic_inferior_feeling2, ", ", ic_superior_feeling2, "]\n")


X <- c("Média Amostra","Votou Bolsonaro","Votou Lula", "Gosta Muito PL", "Gosta Muito PT")
media <-c(8.258151, 7.850808, 8.528171,7.909593, 8.377933 )
lower <- c(8.155027,7.660164,8.364443,7.667528,8.194167)
upper <- c(8.361275,8.041452,8.691899,8.151659,8.561699 )
colorir <-c("1","2","3","2","3")
cores2 <- c("black","darkgreen", "darkred")
dadosmediadep2 <- data.frame(X,media,lower,upper,colorir)
dadosmediadep2 $X <- factor(dadosmediadep2$X,
                            levels = c("Média Amostra",
                                       "Votou Bolsonaro",
                                       "Votou Lula",
                                       "Gosta Muito PL",
                                       "Gosta Muito PT"))
dadosmediadep2
graficomediadep2 <- ggplot(dadosmediadep2, aes(x = X, y = media, color=colorir)) +
  geom_point(size = 3) +  # Adicionar pontos para as estimativas
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) +  # Adicionar barras de erro
  coord_flip() +  # Inverter os eixos x e y
  theme_minimal() +  # Aplicar um tema mínimo ao gráfico
  geom_text(aes(label = sprintf("%.2f", media)), hjust = 0.5, size = 3, vjust=-0.9) +
  labs(x = "Apoio aos Princípios",
       y = NULL,
       title = "")+
  theme(legend.position = "none")+
  scale_color_manual(values = cores2)#+ scale_y_continuous(limits = c(0.162, 0.634))
graficomediadep2
#


# df3 - dep3
mean(df3voto$ToleranteAoRival, na.rm=T) #1 Média Amostra 1 (apenas aqueles que votaram Lula e Bolsonaro)
mean(df3votoBolsonaro$BolsonaristaqueToleraRival, na.rm = T) #2 Votou Bolsonaro
mean(df3votoLula$LulistaqueToleraRival,na.rm=T) #3 Votou Lula
mean(df3feeling$ToleranteAoRival, na.rm=T)#4 Média Amostra 2 (apenas aqueles que gostam muito do PT ou do PL)
mean(df3feelingPartidoLiberal$toleraPT, na.rm=T) # 5 Gosta muito do PL
mean(df3feelingPT$toleraPL, na.rm=T) # 6 Gosta muito do PT



# Calculando as médias para cada grupo
media_grupo1 <- mean(df3voto$ToleranteAoRival, na.rm = TRUE)
media_grupo2 <- mean(df3votoBolsonaro$BolsonaristaqueToleraRival, na.rm = TRUE)
media_grupo3 <- mean(df3votoLula$LulistaqueToleraRival, na.rm = TRUE)
media_grupo4 <- mean(df3feeling$ToleranteAoRival, na.rm = TRUE)
media_grupo5 <- mean(df3feelingPartidoLiberal$toleraPT, na.rm = TRUE)
media_grupo6 <- mean(df3feelingPT$toleraPL, na.rm = TRUE)

# Calcular os desvios padrão para cada grupo
desvio_padrao_grupo1 <- sd(df3voto$ToleranteAoRival, na.rm = TRUE)
desvio_padrao_grupo2 <- sd(df3votoBolsonaro$BolsonaristaqueToleraRival, na.rm = TRUE)
desvio_padrao_grupo3 <- sd(df3votoLula$LulistaqueToleraRival, na.rm = TRUE)
desvio_padrao_grupo4 <- sd(df3feeling$ToleranteAoRival, na.rm = TRUE)
desvio_padrao_grupo5 <- sd(df3feelingPartidoLiberal$toleraPT, na.rm = TRUE)
desvio_padrao_grupo6 <- sd(df3feelingPT$toleraPL, na.rm = TRUE)

# Número de observações em cada grupo
n_grupo1 <- sum(!is.na(df3voto$ToleranteAoRival))
n_grupo2 <- sum(!is.na(df3votoBolsonaro$BolsonaristaqueToleraRival))
n_grupo3 <- sum(!is.na(df3votoLula$LulistaqueToleraRival))
n_grupo4 <- sum(!is.na(df3feeling$ToleranteAoRival))
n_grupo5 <- sum(!is.na(df3feelingPartidoLiberal$toleraPT))
n_grupo6 <- sum(!is.na(df3feelingPT$toleraPL))

# Calcular o erro padrão da média para cada grupo
erro_padrao_grupo1 <- desvio_padrao_grupo1 / sqrt(n_grupo1)
erro_padrao_grupo2 <- desvio_padrao_grupo2 / sqrt(n_grupo2)
erro_padrao_grupo3 <- desvio_padrao_grupo3 / sqrt(n_grupo3)
erro_padrao_grupo4 <- desvio_padrao_grupo4 / sqrt(n_grupo4)
erro_padrao_grupo5 <- desvio_padrao_grupo5 / sqrt(n_grupo5)
erro_padrao_grupo6 <- desvio_padrao_grupo6 / sqrt(n_grupo6)

# Calcular o intervalo de confiança para cada grupo (assumindo um nível de confiança de 95%)
tamanho_ic_grupo1 <- qt(0.975, df = n_grupo1 - 1) * erro_padrao_grupo1
ic_inferior_grupo1 <- media_grupo1 - tamanho_ic_grupo1
ic_superior_grupo1 <- media_grupo1 + tamanho_ic_grupo1

tamanho_ic_grupo2 <- qt(0.975, df = n_grupo2 - 1) * erro_padrao_grupo2
ic_inferior_grupo2 <- media_grupo2 - tamanho_ic_grupo2
ic_superior_grupo2 <- media_grupo2 + tamanho_ic_grupo2

tamanho_ic_grupo3 <- qt(0.975, df = n_grupo3 - 1) * erro_padrao_grupo3
ic_inferior_grupo3 <- media_grupo3 - tamanho_ic_grupo3
ic_superior_grupo3 <- media_grupo3 + tamanho_ic_grupo3

tamanho_ic_grupo4 <- qt(0.975, df = n_grupo4 - 1) * erro_padrao_grupo4
ic_inferior_grupo4 <- media_grupo4 - tamanho_ic_grupo4
ic_superior_grupo4 <- media_grupo4 + tamanho_ic_grupo4

tamanho_ic_grupo5 <- qt(0.975, df = n_grupo5 - 1) * erro_padrao_grupo5
ic_inferior_grupo5 <- media_grupo5 - tamanho_ic_grupo5
ic_superior_grupo5 <- media_grupo5 + tamanho_ic_grupo5

tamanho_ic_grupo6 <- qt(0.975, df = n_grupo6 - 1) * erro_padrao_grupo6
ic_inferior_grupo6 <- media_grupo6 - tamanho_ic_grupo6
ic_superior_grupo6 <- media_grupo6 + tamanho_ic_grupo6

# Exibir resultados
cat("Grupo 1 (Apenas aqueles que votaram Lula e Bolsonaro):", media_grupo1, "\n")
cat("Intervalo de confiança (95%) para Grupo 1: [", ic_inferior_grupo1, ", ", ic_superior_grupo1, "]\n\n")

cat("Grupo 2 (Votou Bolsonaro):", media_grupo2, "\n")
cat("Intervalo de confiança (95%) para Grupo 2: [", ic_inferior_grupo2, ", ", ic_superior_grupo2, "]\n\n")

cat("Grupo 3 (Votou Lula):", media_grupo3, "\n")
cat("Intervalo de confiança (95%) para Grupo 3: [", ic_inferior_grupo3, ", ", ic_superior_grupo3, "]\n\n")

cat("Grupo 4 (Apenas aqueles que gostam muito do PT ou do PL):", media_grupo4, "\n")
cat("Intervalo de confiança (95%) para Grupo 4: [", ic_inferior_grupo4, ", ", ic_superior_grupo4, "]\n\n")

cat("Grupo 5 (Gosta muito do PL):", media_grupo5, "\n")
cat("Intervalo de confiança (95%) para Grupo 5: [", ic_inferior_grupo5, ", ", ic_superior_grupo5, "]\n\n")

cat("Grupo 6 (Gosta muito do PT):", media_grupo6, "\n")
cat("Intervalo de confiança (95%) para Grupo 6: [", ic_inferior_grupo6, ", ", ic_superior_grupo6, "]\n\n")


X <- c("Média Amostra 1*","Votou Bolsonaro","Votou Lula", "Média Amostra 2**",
       "Gosta Muito PL", "Gosta Muito PT")
media <-c(0.2138614 ,0.1653061,0.2596154,0.1623037,0.09120521,0.2100656   )
lower <- c(0.1885311,0.1323013,0.2218084,0.1360988,0.05881967,0.1725776)
upper <- c(0.2391916,0.198311,0.2974224,0.1885086,0.1235908, 0.2475537 )
colorir <-c("1","2","3","1","2","3")
cores2 <- c("black","darkgreen", "darkred")
dadosmediadep3 <- data.frame(X,media,lower,upper,colorir)
dadosmediadep3 $X <- factor(dadosmediadep3$X,
                            levels = c("Média Amostra 1*","Média Amostra 2**",
                                       "Votou Bolsonaro",
                                       "Votou Lula",
                                       "Gosta Muito PL",
                                       "Gosta Muito PT"))


dadosmediadep3
graficomediadep3 <- ggplot(dadosmediadep3, aes(x = X, y = media, color=colorir)) +
  geom_point(size = 3) +  # Adicionar pontos para as estimativas
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) +  # Adicionar barras de erro
  coord_flip() +  # Inverter os eixos x e y
  theme_minimal() +  # Aplicar um tema mínimo ao gráfico
  geom_text(aes(label = sprintf("%.2f", media)), hjust = 0.5, size = 3, vjust=-0.9) +
  labs(x = "Tolerância ao rival",
       y = NULL,
       title = "",
       caption="*Apenas aqueles que votaram Lula e Bolsonaro;
       **Apenas aqueles que gostam muito do PT ou do PL")+
  theme(legend.position = "none",
        plot.caption = element_text(size = 11)) +  # Ajustar o tamanho do caption
  scale_color_manual(values = cores2)
graficomediadep3

dadosmediadep3


# anova

df1$feeling <- as.factor(df1$feeling)
# Realizar a ANOVA
anova_resultado <- aov(dep1 ~ feeling, data = df1)

# Resumo da ANOVA
summary(anova_resultado)

df1$voto <- as.factor(df1$voto)
# Realizar a ANOVA
anova_resultado <- aov(dep1 ~ voto, data = df1)

# Resumo da ANOVA
summary(anova_resultado)



df2$feeling <- as.factor(df2$feeling)
# Realizar a ANOVA
anova_resultado <- aov(dep2 ~ feeling, data = df2)

# Resumo da ANOVA
summary(anova_resultado)

df2$voto <- as.factor(df2$voto)
# Realizar a ANOVA
anova_resultado <- aov(dep2 ~ voto, data = df2)
options(scipen=99)
# Resumo da ANOVA
summary(anova_resultado)


df3feeling$feeling <- as.factor(df3feeling$feeling)
# Realizar a ANOVA
anova_resultado <- aov(ToleranteAoRival ~ feeling, data = df3feeling)

# Resumo da ANOVA
summary(anova_resultado)

df3voto$voto <- as.factor(df3voto$voto)
# Realizar a ANOVA
anova_resultado <- aov(ToleranteAoRival ~ voto, data = df3voto)
options(scipen=99)
# Resumo da ANOVA
summary(anova_resultado)



#
summary(df1)
summary(df2[,9:12])
summary(df2[,15])

summary(df3feeling[,1:8])
summary(df3feeling[,15:17])

summary(df3voto[,1:8])
summary(df3voto[,15:17])


#
hist(df1$dep1)
hist(df2$dep2_padron)
hist(df3feeling$ToleranteAoRival)
hist(df3voto$ToleranteAoRival)
hist(df2$P51_invert)
hist(df2$P52)
hist(df2$P53)
hist(df2$P54)

#
df3
df3 <- subset(df, select=c(voto,feeling,Mulher,
                           Escolaridade, Idade, Renda, raca_branca, interesse, P22,P24))# refazer o 3 para não perder casos

#nova dep
# Criar a variável dep_robustez com base nas condições fornecidas
table(df3$P22, useNA = "always")
table(df3$P24, useNA= "always")
df3$toleraPT <- df3$P22 >= 8 & df3$P22 <= 10
df3$toleraPL <- df3$P24 >= 8 & df3$P24 <= 10
df3$toleraPL <- as.numeric(df3$toleraPL)
df3$toleraPT <- as.numeric(df3$toleraPT)
df3$dep_robustez <- ifelse((df3$P22 >= 8 & df3$P22 <= 10) & (df3$P24 >= 8 & df3$P24 <= 10), 1, 0)
table(df3$dep_robustez, useNA = "always")
table(df3$toleraPL, useNA = "always")
table(df3$toleraPT, useNA = "always")
summary(df3)



table(df3$toleraPL)
table(df3$toleraPT)
table(df3$dep_robustez)
table(df3$toleraPL, df3$voto)
table(df3$toleraPT, df3$voto)
table(df3$dep_robustez, df3$voto)
amost <- c("Amostra","Votou em Lula", "Votou em Bolsonaro", "Nem Lula e Nem Bolsonaro(Outros/Abstenção)")
ToleraPL <- c(637,135,329,173)
NãoToleraPL <-c(863,385,175,303)
ToleraPT <- c(741,452,81,208)
NãoToleraPT <-c(759,68,423,268)
TolerOsDois <-c(306,130,65,111)
NãoToleraOsDois <-c(1194,390,439,365)
# Criar o data frame com os dados
tabela_resumo <- data.frame(Amostra = amost, ToleraPL = ToleraPL, NãoToleraPL = NãoToleraPL,
                            ToleraPT = ToleraPT, NãoToleraPT = NãoToleraPT, TolerOsDois = TolerOsDois,
                            NãoToleraOsDois = NãoToleraOsDois)

# Exibir a tabela resumida
print(tabela_resumo)
#install.packages("openxlsx")
# Carregar a biblioteca openxlsx
library(openxlsx)

# Criar um novo arquivo Excel
wb <- createWorkbook()

# Adicionar a tabela_resumo ao arquivo Excel
addWorksheet(wb, "Tabela_Resumo")
writeData(wb, "Tabela_Resumo", tabela_resumo)

# Salvar o arquivo Excel
saveWorkbook(wb, "Tabela_Resumo.xlsx", overwrite = TRUE)
#


prop.table(table(df3$toleraPL, df3$voto),2)*100
prop.table(table(df3$toleraPT, df3$voto),2)*100
prop.table(table(df3$dep_robustez, df3$voto),2)*100


# Criar um data frame com os dados
dados <- data.frame(
  voto = c("Outros/Abstenção","Votou em Bolsonaro", "Votou em Lula"),
  ToleraPL = c(36.33, 65.27, 25.96),
  ToleraPT = c(43.69, 16.07, 86.93),
  ToleraAmbos = c(23.31, 12.89, 25.00)
)

# Transformar os dados no formato longo para facilitar o gráfico
dados_long <- tidyr::gather(dados, key = "Grupo", value = "Porcentagem", -voto)

# Criar o gráfico de barras
ggplot(dados_long, aes(x = voto, y = Porcentagem, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Voto", y = "Porcentagem", fill = "Grupo") +
  ggtitle("Tolerância ao PT, PL e Ambos por Grupo de Voto") +
  theme_minimal()


modelorobustez33 <- glm(dep_robustez ~ voto+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                        data = df3, family=binomial(link=logit))
summary(modelorobustez33)#para depois

#agora feeling
table(df3$feeling, useNA = "always")
df3$feeling[df$P18 >= 8 & df$P18 <= 10 & df$P20 >= 8 & df$P20 <= 10] <- NA
table(df3$feeling, useNA = "always")# removido os 21


table(df3$toleraPL)
table(df3$toleraPT)
table(df3$dep_robustez)
table(df3$toleraPL, df3$feeling, useNA = "always")
table(df3$toleraPT, df3$feeling, useNA = "always" )
table(df3$dep_robustez, df3$feeling, useNA = "always")
amost <- c("Amostra","Nem Gosta Muito do PL, Nem Gosta Muito do PL",
           "Gosta Muito do PL", "Gosta Muito do PT", "Gosta Muito de Ambos (NA)")
ToleraPL <- c(637,245,279,96,17)
NãoToleraPL <-c(863,461,37,361,4)
ToleraPT <- c(741,267,28,428,18)
NãoToleraPT <-c(759,439,288,29,3)
TolerOsDois <-c(306,171,27,93,15)
NãoToleraOsDois <-c(1194,535,289,364,6)
# Criar o data frame com os dados
tabela_resumo2 <- data.frame(Amostra = amost, ToleraPL = ToleraPL, NãoToleraPL = NãoToleraPL,
                             ToleraPT = ToleraPT, NãoToleraPT = NãoToleraPT, TolerOsDois = TolerOsDois,
                             NãoToleraOsDois = NãoToleraOsDois)

# Exibir a tabela resumida
print(tabela_resumo2)
#install.packages("openxlsx")
# Carregar a biblioteca openxlsx
library(openxlsx)

# Criar um novo arquivo Excel
wb <- createWorkbook()

# Adicionar a tabela_resumo ao arquivo Excel
addWorksheet(wb, "Tabela_Resumo2")
writeData(wb, "Tabela_Resumo2", tabela_resumo2)

# Salvar o arquivo Excel
saveWorkbook(wb, "Tabela_Resumo2.xlsx", overwrite = TRUE)
#


prop.table(table(df3$toleraPL, df3$feeling,useNA="always"),2)*100
prop.table(table(df3$toleraPT, df3$feeling,useNA="always"),2)*100
prop.table(table(df3$dep_robustez, df3$feeling,useNA="always"),2)*100


# Criar um data frame com os dados
dados <- data.frame(
  feeling = c("Neutro*","Gosta Muito PL", "Gosta Muito do PT","NA**"),
  ToleraPL = c(34.7,88.29,21,80.95),
  ToleraPT = c(37.81,8.86,93.65,85.71),
  ToleraAmbos = c(24.22,8.54,20.35,71.42)
)

# Transformar os dados no formato longo para facilitar o gráfico
dados_long <- tidyr::gather(dados, key = "Grupo", value = "Porcentagem", -feeling)

# Criar o gráfico de barras
ggplot(dados_long, aes(x = feeling, y = Porcentagem, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "feeling", y = "Porcentagem", fill = "Grupo",
       caption= "* Não Gosta Muito do PL e Não Gosta Muito do PT;
       ** Gosta Muito ao mesmo tempo dos dois (foi considerado NA, 21 casos)") +
  ggtitle("Tolerância ao PT, PL e Ambos por Grupo de sentimento partidário") +
  theme_minimal()+theme(legend.position = "bottom")


modelorobustez34 <- glm(dep_robustez ~ feeling+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                        data = df3, family=binomial(link=logit))
summary(modelorobustez34)#para depois


tab_model(modelorobustez33, modelorobustez34, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#
# teste de media

#dep 1
# Calcular a média
media <- mean(df3$dep_robustez, na.rm = TRUE)

# Calcular o desvio padrão
desvio_padrao <- sd(df3$dep_robustez, na.rm = TRUE)

# Número de observações
n <- sum(!is.na(df3$dep_robustez))

# Calcular o erro padrão da média
erro_padrao <- desvio_padrao / sqrt(n)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%)
erro_padrao_95 <- qt(0.975, df = n - 1) * erro_padrao
ic_inferior <- media - erro_padrao_95
ic_superior <- media + erro_padrao_95
# Exibir resultados
cat("dep_robustez:", media, "\n")
cat("Intervalo de confiança (95%): [", ic_inferior, ", ", ic_superior, "]\n") 

table(df3$voto)
# Dividir o dataframe em dois baseado na categoria de voto
df_voto1 <- df3[df3$voto == "Bolsonaro", ]
df_voto2 <- df3[df3$voto == "Lula", ]
df_voto3 <- df3[df3$voto == "Outros/Abstenção", ]

# Calcular as estatísticas para cada grupo
media_voto1 <- mean(df_voto1$dep_robustez, na.rm = TRUE)
media_voto2 <- mean(df_voto2$dep_robustez, na.rm = TRUE)
media_voto3 <- mean(df_voto3$dep_robustez, na.rm = TRUE)
desvio_padrao_voto1 <- sd(df_voto1$dep_robustez, na.rm = TRUE)
desvio_padrao_voto2 <- sd(df_voto2$dep_robustez, na.rm = TRUE)
desvio_padrao_voto3 <- sd(df_voto3$dep_robustez, na.rm = TRUE)
n_voto1 <- sum(!is.na(df_voto1$dep_robustez))
n_voto2 <- sum(!is.na(df_voto2$dep_robustez))
n_voto3 <- sum(!is.na(df_voto3$dep_robustez))
# Calcular o erro padrão da média para cada grupo
erro_padrao_voto1 <- desvio_padrao_voto1 / sqrt(n_voto1)
erro_padrao_voto2 <- desvio_padrao_voto2 / sqrt(n_voto2)
erro_padrao_voto3 <- desvio_padrao_voto3 / sqrt(n_voto2)
# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%) para cada grupo
erro_padrao_95_voto1 <- qt(0.975, df = n_voto1 - 1) * erro_padrao_voto1
ic_inferior_voto1 <- media_voto1 - erro_padrao_95_voto1
ic_superior_voto1 <- media_voto1 + erro_padrao_95_voto1

erro_padrao_95_voto2 <- qt(0.975, df = n_voto2 - 1) * erro_padrao_voto2
ic_inferior_voto2 <- media_voto2 - erro_padrao_95_voto2
ic_superior_voto2 <- media_voto2 + erro_padrao_95_voto2
erro_padrao_95_voto3 <- qt(0.975, df = n_voto3 - 1) * erro_padrao_voto3
ic_inferior_voto3 <- media_voto3 - erro_padrao_95_voto3
ic_superior_voto3 <- media_voto3 + erro_padrao_95_voto3

# Exibir resultados
cat("Categoria 1 Bolsonaro (voto):", media_voto1, "\n")
cat("Intervalo de confiança (95%) para categoria 1: [", ic_inferior_voto1, ", ", ic_superior_voto1, "]\n\n")

cat("Categoria 2 Lula (voto):", media_voto2, "\n")
cat("Intervalo de confiança (95%) para categoria 2: [", ic_inferior_voto2, ", ", ic_superior_voto2, "]\n")


cat("Categoria 3 Outros/abs (voto):", media_voto3, "\n")
cat("Intervalo de confiança (95%) para categoria 2: [", ic_inferior_voto3, ", ", ic_superior_voto3, "]\n")


table(df3$feeling,useNA="always")
# Dividir o dataframe em dois baseado na categoria de feeling
df_feeling1 <- df3[df3$feeling == "Gosta Muito PL", ]
df_feeling1 <- df_feeling1[!is.na(df_feeling1$feeling), ]
df_feeling2 <- df3[df3$feeling == "Gosta Muito PT", ]
df_feeling2 <- df_feeling2[!is.na(df_feeling2$feeling), ]
df_feeling3 <- df3[df3$feeling == "Neutro", ]
df_feeling3 <- df_feeling3[!is.na(df_feeling3$feeling), ]
df_feeling4 <- df3[is.na(df3$feeling), ]

# Calcular as estatísticas para cada grupo
media_feeling1 <- mean(df_feeling1$dep_robustez, na.rm = TRUE)
media_feeling2 <- mean(df_feeling2$dep_robustez, na.rm = TRUE)
media_feeling3 <- mean(df_feeling3$dep_robustez, na.rm = TRUE)
media_feeling4 <- mean(df_feeling4$dep_robustez, na.rm = TRUE)


desvio_padrao_feeling1 <- sd(df_feeling1$dep_robustez, na.rm = TRUE)
desvio_padrao_feeling2 <- sd(df_feeling2$dep_robustez, na.rm = TRUE)
desvio_padrao_feeling3 <- sd(df_feeling3$dep_robustez, na.rm = TRUE)
desvio_padrao_feeling4 <- sd(df_feeling4$dep_robustez, na.rm = TRUE)


n_feeling1 <- sum(!is.na(df_feeling1$dep_robustez))
n_feeling2 <- sum(!is.na(df_feeling2$dep_robustez))
n_feeling3 <- sum(!is.na(df_feeling3$dep_robustez))
n_feeling4 <- sum(!is.na(df_feeling4$dep_robustez))

# Calcular o erro padrão da média para cada grupo
erro_padrao_feeling1 <- desvio_padrao_feeling1 / sqrt(n_feeling1)
erro_padrao_feeling2 <- desvio_padrao_feeling2 / sqrt(n_feeling2)
erro_padrao_feeling3 <- desvio_padrao_feeling3 / sqrt(n_feeling3)
erro_padrao_feeling4 <- desvio_padrao_feeling4 / sqrt(n_feeling4)

# Calcular o intervalo de confiança (assumindo um nível de confiança de 95%) para cada grupo
erro_padrao_95_feeling1 <- qt(0.975, df = n_feeling1 - 1) * erro_padrao_feeling1
ic_inferior_feeling1 <- media_feeling1 - erro_padrao_95_feeling1
ic_superior_feeling1 <- media_feeling1 + erro_padrao_95_feeling1

erro_padrao_95_feeling2 <- qt(0.975, df = n_feeling2 - 1) * erro_padrao_feeling2
ic_inferior_feeling2 <- media_feeling2 - erro_padrao_95_feeling2
ic_superior_feeling2 <- media_feeling2 + erro_padrao_95_feeling2

erro_padrao_95_feeling3 <- qt(0.975, df = n_feeling3 - 1) * erro_padrao_feeling3
ic_inferior_feeling3 <- media_feeling3 - erro_padrao_95_feeling3
ic_superior_feeling3 <- media_feeling3 + erro_padrao_95_feeling3

erro_padrao_95_feeling4 <- qt(0.975, df = n_feeling4 - 1) * erro_padrao_feeling4
ic_inferior_feeling4 <- media_feeling4 - erro_padrao_95_feeling4
ic_superior_feeling4 <- media_feeling4 + erro_padrao_95_feeling4

# Exibir resultados
cat("Categoria 1 (feeling): Gosta Muito PL", media_feeling1, "\n")
cat("Intervalo de confiança (95%) para categoria 1: [", ic_inferior_feeling1, ", ", ic_superior_feeling1, "]\n\n")

cat("Categoria 2 (feeling) : Gosta Muito PT", media_feeling2, "\n")
cat("Intervalo de confiança (95%) para categoria 2: [", ic_inferior_feeling2, ", ", ic_superior_feeling2, "]\n")

cat("Categoria 3 (feeling): Nem Nem", media_feeling3, "\n")
cat("Intervalo de confiança (95%) para categoria 3: [", ic_inferior_feeling3, ", ", ic_superior_feeling3, "]\n")


cat("Categoria 4 (feeling): NA ambos", media_feeling4, "\n")
cat("Intervalo de confiança (95%) para categoria 4: [", ic_inferior_feeling4, ", ", ic_superior_feeling4, "]\n")




X <- c("Média Amostra","Votou Bolsonaro","Votou Lula", "Votou em Outros/Se absteve",
       "Gosta Muito PL", "Gosta Muito PT", "Neutro*","NA (21 casos)**")
media <-c(0.204,0.1289683,0.25, 0.2331933, 0.08544304,0.2035011,0.2422096,0.7142857 )
lower <- c(0.183,0.0996074,0.2126596,0.1967169,0.0544539,0.1664504,0.2105307,0.5035716 )
upper <- c(0.224,0.1583291,0.2873404,0.2696697,0.1164321,0.2405517,0.2738885,0.9249998 )
colorir <-c("1","2","3","4","2","3","4","4")
cores2 <- c("black","darkgreen", "darkred","grey43")
dadosmediadep_robustez <- data.frame(X,media,lower,upper,colorir)
dadosmediadep_robustez $X <- factor(dadosmediadep_robustez$X,
                                    levels = c("Neutro*","NA (21 casos)**","Média Amostra",
                                               "Votou Bolsonaro","Votou Lula",
                                               "Votou em Outros/Se absteve",
                                               "Gosta Muito PL",
                                               "Gosta Muito PT"))
dadosmediadep_robustez
graficomediadep_robustez <- ggplot(dadosmediadep_robustez, aes(x = X, y = media, color=colorir)) +
  geom_point(size = 3) +  # Adicionar pontos para as estimativas
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) +  # Adicionar barras de erro
  coord_flip() +  # Inverter os eixos x e y
  theme_minimal() +  # Aplicar um tema mínimo ao gráfico
  geom_text(aes(label = sprintf("%.2f", media)), hjust = 0.5, size = 3, vjust=-0.9) +
  labs(x = NULL,
       y = NULL,
       subtitle = "Tolera o direito de concorrer nas eleições
       para presidente do Brasil de ambos (PL & PT)",
       caption= "* Não Gosta Muito do PL e Não Gosta Muito do PT;
       ** Gosta Muito ao mesmo tempo dos dois (foi considerado NA, 21 casos)")+
  theme(legend.position = "none")+
  scale_color_manual(values = cores2)#+ scale_y_continuous(limits = c(0.162, 0.634))
graficomediadep_robustez


dadosmediadep_robustez

table(df3$dep_robustez)



# novo formato da variável independente - usar feeling thermomether ########

# relação com a variável dep 1 - apoio explícito
table(df1$dep1, useNA = "always")
df1$FeelingNumericPT <- df$P18
df1$FeelingNumericPT[df1$FeelingNumericPT == 11 | df1$FeelingNumericPT == 12] <- NA
table(df1$FeelingNumericPT,useNA = "always")
df1$FeelingNumericPL <- df$P20
df1$FeelingNumericPL[df1$FeelingNumericPL == 11 | df1$FeelingNumericPL == 12] <- NA
table(df1$FeelingNumericPL,useNA = "always")


table(df1$dep1, df1$FeelingNumericPT)

# Calcular médias e intervalos de confiança
df_summary <- df1 %>%
  group_by(FeelingNumericPT) %>%
  summarise(
    mean_dep1 = mean(dep1, na.rm = TRUE),
    ci_lower = mean(dep1, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(dep1, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean(dep1, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(dep1, na.rm = TRUE) / sqrt(n())
  )
graf1 <- ggplot(df_summary, aes(x = FeelingNumericPT, y = mean_dep1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  coord_cartesian(ylim = c(0.4, 1)) +  # Ajusta os limites do eixo y 
  labs(
    title = "Média de dep1 por nível de FeelingNumericPT",
    x = "FeelingNumericPT",
    y = "Média de dep1"
  ) +
  theme_minimal()+coord_flip()


graf1
df_summary <- df1 %>%
  group_by(FeelingNumericPL) %>%
  summarise(
    mean_dep1 = mean(dep1, na.rm = TRUE),
    ci_lower = mean(dep1, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(dep1, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean(dep1, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(dep1, na.rm = TRUE) / sqrt(n())
  )
graf2 <- ggplot(df_summary, aes(x = FeelingNumericPL, y = mean_dep1)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  coord_cartesian(ylim = c(0.4, 1)) +  # Ajusta os limites do eixo y
  labs(
    title = "Média de dep1 por nível de FeelingNumericPL",
    x = "FeelingNumericPL",
    y = "Média de dep1"
  ) +
  theme_minimal()+coord_flip()
graf2


plots(graf1, graf2, n_rows = 2)

#


modelo412 <- glm(dep1 ~ FeelingNumericPT+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                data = df1, family=binomial(link=logit))
modelo512 <- glm(dep1 ~ FeelingNumericPL+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df1, family=binomial(link=logit))
modelo612 <- glm(dep1 ~ FeelingNumericPT+FeelingNumericPL+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df1, family=binomial(link=logit))


tab_model(modelo412, modelo512, modelo612, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoioexplicito412 <- plot_predictions(modelo412, condition=c("FeelingNumericPT")) + theme_bw()+labs(x="Sentimento partidário PT",
                                                                                         y= "Apoio Explicíto")
apoioexplicito412

apoioexplicito512 <- plot_predictions(modelo512, condition=c("FeelingNumericPL")) + theme_bw()+labs(x="Sentimento partidário PL",
                                                                                                    y= "Apoio Explicíto")
apoioexplicito512

plots(apoioexplicito412, apoioexplicito512, n_rows = 2)



# anova

anova_resultado <- aov(dep1 ~ FeelingNumericPT, data = df1)
summary(anova_resultado)

#
anova_resultado <- aov(dep1 ~ FeelingNumericPL, data = df1)
summary(anova_resultado)

# relação com a variável dep 2 - apoio nivel 2
summary(df2$dep2_padron, useNA = "always")
df2$FeelingNumericPT <- df$P18
df2$FeelingNumericPT[df2$FeelingNumericPT == 11 | df2$FeelingNumericPT == 12] <- NA
table(df2$FeelingNumericPT,useNA = "always")
df2$FeelingNumericPL <- df$P20
df2$FeelingNumericPL[df2$FeelingNumericPL == 11 | df2$FeelingNumericPL == 12] <- NA
table(df2$FeelingNumericPL,useNA = "always")




# Calcular médias e intervalos de confiança
df_summary <- df2 %>%
  group_by(FeelingNumericPT) %>%
  summarise(
    mean_dep2_padron = mean(dep2_padron, na.rm = TRUE),
    ci_lower = mean(dep2_padron, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(dep2_padron, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean(dep2_padron, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(dep2_padron, na.rm = TRUE) / sqrt(n())
  )
graf1 <- ggplot(df_summary, aes(x = FeelingNumericPT, y = mean_dep2_padron)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  #coord_cartesian(ylim = c(0.4, 1)) +  # Ajusta os limites do eixo y 
  labs(
    title = "Média de dep2_padron por nível de FeelingNumericPT",
    x = "FeelingNumericPT",
    y = "Média de dep2"
  ) +
  theme_minimal()+coord_flip()


graf1
df_summary <- df2 %>%
  group_by(FeelingNumericPL) %>%
  summarise(
    mean_dep2_padron = mean(dep2_padron, na.rm = TRUE),
    ci_lower = mean(dep2_padron, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(dep2_padron, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean(dep2_padron, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(dep2_padron, na.rm = TRUE) / sqrt(n())
  )
graf2 <- ggplot(df_summary, aes(x = FeelingNumericPL, y = mean_dep2_padron)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  #coord_cartesian(ylim = c(0.4, 1)) +  # Ajusta os limites do eixo y
  labs(
    title = "Média de dep2_padron por nível de FeelingNumericPL",
    x = "FeelingNumericPL",
    y = "Média de dep2"
  ) +
  theme_minimal()+coord_flip()
graf2


plots(graf1, graf2, n_rows = 2)

#

modelo422 <-  lm(dep2_padron ~ FeelingNumericPT+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modelo522 <- lm(dep2_padron ~ FeelingNumericPL+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)
modelo622 <- lm(dep2_padron ~ FeelingNumericPT+FeelingNumericPL+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df2)


tab_model(modelo422, modelo522, modelo622, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
apoiodep2422 <- plot_predictions(modelo422, condition=c("FeelingNumericPT")) + theme_bw()+labs(x="Sentimento partidário PT",
                                                                                                    y= "Apoio aos princípios")
apoiodep2422

apoiodep2522 <- plot_predictions(modelo522, condition=c("FeelingNumericPL")) + theme_bw()+labs(x="Sentimento partidário PL",
                                                                                                    y= "Apoio aos princípios")
apoiodep2522

plots(apoiodep2422, apoiodep2522, n_rows = 2)



# anova

anova_resultado <- aov(dep2_padron ~ FeelingNumericPT, data = df2)
summary(anova_resultado)

#
anova_resultado <- aov(dep2_padron ~ FeelingNumericPL, data = df2)
summary(anova_resultado)
#


# dep 3 - tolerância ao rival
# vou ter q refazer o caminho - criar uma base para cada um - sem os NA´s e fazer o teste a parte
# nesse caso não precisa criar tolerancia a ambos, basta rodar separado


df3$FeelingNumericPT <- df$P18
df3$FeelingNumericPT[df3$FeelingNumericPT == 11 | df3$FeelingNumericPT == 12] <- NA
table(df3$FeelingNumericPT,useNA = "always")
df3$FeelingNumericPL <- df$P20
df3$FeelingNumericPL[df3$FeelingNumericPL == 11 | df3$FeelingNumericPL == 12] <- NA
table(df3$FeelingNumericPL,useNA = "always")

df3$toleraPL # dep tolera pl ~ dep feeling numeric pt + controles # modelos
df3$toleraPT # dep tolera pt ~ dep feeling numeric pl + controles # modelos




# primeiro testes de média
df_summary <- df3 %>%
  group_by(FeelingNumericPT) %>%
  summarise(
    mean_toleraPL = mean(toleraPL, na.rm = TRUE),
    ci_lower = mean(toleraPL, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(toleraPL, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean(toleraPL, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(toleraPL, na.rm = TRUE) / sqrt(n())
  )
graf1 <- ggplot(df_summary, aes(x = FeelingNumericPT, y = mean_toleraPL)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous(limits=c(0,1.1))+
  labs(
    title = "Média de toleraPL por nível de FeelingNumericPT",
    x = "FeelingNumericPT",
    y = "Média de Tolerância ao PL"
  ) +
  theme_minimal()+coord_flip()


graf1
df_summary <- df3 %>%
  group_by(FeelingNumericPL) %>%
  summarise(
    mean_toleraPT = mean(toleraPT, na.rm = TRUE),
    ci_lower = mean(toleraPT, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(toleraPT, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean(toleraPT, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(toleraPT, na.rm = TRUE) / sqrt(n())
  )

df_summary
graf2 <- ggplot(df_summary, aes(x = FeelingNumericPL, y = mean_toleraPT)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous(limits=c(0,1.1))+
  labs(
    title = "Média de toleraPT por nível de FeelingNumericPL",
    x = "FeelingNumericPL",
    y = "Média de Tolerância ao PT"
  ) +
  theme_minimal()+coord_flip()
graf2


plots(graf1, graf2, n_rows = 2)

# anova


summary(df3$toleraPL)
summary(df3$toleraPT)

anova_resultado <- aov(toleraPL ~ FeelingNumericPT, data = df3)
summary(anova_resultado)

#
anova_resultado <- aov(toleraPT ~ FeelingNumericPL, data = df3)
summary(anova_resultado)


#

modelo532_toleraPL <-  lm(toleraPL~ FeelingNumericPT+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                 data = df3)
modelo532_toleraPT <- lm(toleraPT ~ FeelingNumericPL+Escolaridade+Idade+Renda+raca_branca+interesse+Mulher,
                data = df3)


tab_model(modelo532_toleraPL,modelo532_toleraPT, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")

apoio_predic_modelo532_toleraPL <- plot_predictions(modelo532_toleraPL, condition=c("FeelingNumericPT")) + theme_bw()+labs(x="Sentimento partidário PT",
                                                                                                                           y= "Tolerância ao Rival (PL)")
apoio_predic_modelo532_toleraPL

apoio_predic_modelo532_toleraPT <- plot_predictions(modelo532_toleraPT, condition=c("FeelingNumericPL")) + theme_bw()+labs(x="Sentimento partidário PL",
                                                                                               y= "Tolerância ao Rival (PT)")
apoio_predic_modelo532_toleraPT



plots(apoio_predic_modelo532_toleraPL,
      apoio_predic_modelo532_toleraPT, n_rows = 2)
