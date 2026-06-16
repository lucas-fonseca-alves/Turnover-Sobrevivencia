library(readxl)
library(tidyverse)
library(survival)
library(AdequacyModel)
library(MASS)
library(ggplot2)
library(survminer)


turnover <- read_excel("banco/turnover.xlsx")

#Transformando as variáveis
turnover$stag <- as.numeric(turnover$stag)
turnover$event <- as.numeric(turnover$event)
turnover$gender <- as.factor(turnover$gender)
turnover$age <- as.numeric(turnover$age)
turnover$industry <- as.factor(turnover$industry)
turnover$profession <- as.factor(turnover$profession)
turnover$traffic <- as.factor(turnover$traffic)
turnover$coach <- as.factor(turnover$coach)
turnover$head_gender <- as.factor(turnover$head_gender)
turnover$greywage <- as.factor(turnover$greywage)
turnover$way <- as.factor(turnover$way)
turnover$extraversion <- as.numeric(turnover$extraversion)
turnover$independ <- as.numeric(turnover$independ)
turnover$selfcontrol <- as.numeric(turnover$selfcontrol)
turnover$anxiety <- as.numeric(turnover$anxiety)
turnover$novator <- as.numeric(turnover$novator)

attach(turnover)

#Stag = tempo, event = censura

#-----------------------------------------------------------------------------------------------#----
#2) Fazer uma análise exploratória apenas da variável resposta: estimativa de Kaplan-Meier (KM), 
#gráfico da função de risco acumulado e gráfico do Tempo Total em Teste (gráfico TTT).
#-----------------------------------------------------------------------------------------------#----

#Estimando a função de sobrevivência por Kaplan-Meier

km <- survfit(Surv(stag,event)~1, conf.int = T)
summary(km)

#Sem censura
curva_sobrevivencia <- ggsurvplot(km, 
                                   data = turnover,
                                   conf.int = FALSE,
                                   xlab = "Meses",
                                   ylab = "S(t)",
                                   legend = "none",
                                   censor = F, #Censura,
                                   censor.shape = 124,   # barra vertical
                                   censor.size = 3,
                                   palette  = "black")

ggsave("CurvaSobrevivencia.png", 
       plot = curva_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Com censura
curva_sobrevivenciaCensura <- ggsurvplot(km, 
                                  data = turnover,
                                  conf.int = FALSE,
                                  xlab = "Meses",
                                  ylab = "S(t)",
                                  legend = "none",
                                  #censor = F, #Censura,
                                  censor.shape = 124,   # barra vertical
                                  censor.size = 3,
                                  palette  = "black")

ggsave("CurvaSobrevivenciaComCensura.png", 
       plot = curva_sobrevivenciaCensura$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 



#Plotando o gráfico de função de risco acumulado
curva_RiscoAcumulado <- ggsurvplot(km,
           data = turnover,
           fun = "cumhaz",          # <--- função de risco acumulado
           conf.int = F,
           xlab = "Meses",
           ylim = c(0, 4),
           ylab = "H(t)",
           legend = "none",
           palette = "black",
           censor = F)

# curva_RiscoAcumulado$plot +
#   geom_hline(yintercept = 4, linetype = "dashed", color = "red")


ggsave("CurvaRiscoAcumulado.png", 
       plot = curva_RiscoAcumulado$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 
#Classificaria como o início constante e próximo do 100 ela fica convexa 


  
#Plotando gráfico TTT

# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/graficos_TTT/grafico_TTT.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)

#Plotando o gráfico de função de risco acumulado
TTT(stag, col="red", lty = 2, grid = T)
#O TTT aqui não é tão indicado pela quantidade de censuras, mas se formos avaliar, estaria mais para constante

# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()

#-----------------------------------------------------------------------------------------------#----
# 3)Fazer uma análise exploratória de cada covariável com a variável resposta: estimativa de 
# Kaplan-Meier (KM) e teste de comparação das curvas.
#-----------------------------------------------------------------------------------------------#----

cores_grupos <- c(
  "#E5989B",  # rosa queimado,
  "#264653",  # azul escuro
  "#2A9D8F",  # verde petróleo
  "#E9C46A",  # areia/dourado
  "#F4A261",  # laranja terroso
  "#E76F51",  # coral/terracota
  "#457B9D",  # azul médio
  "#A8DADC",  # azul claro
  "#1D3557",  # azul marinho
  "#B5838D",  # rosa acinzentado
  "#6D6875",  # roxo acinzentado
  "#FFB4A2",  # salmão claro

  "#6C9EBF",  # azul suave
  "#95B8D1",  # azul pastel
  
  "#003f5c",  # azul petróleo escuro
  "#2c4875",  # azul médio
  "#8a5082",  # roxo acinzentado
  "#bc5090",  # rosa magenta
  "#ff6361",  # coral
  "#ff8531",  # laranja queimado
  "#ffa600",  # dourado
  "#7a5195",  # roxo profundo
  "#ef5675",  # rosa vibrante
  "#ff7c43",  # laranja terracota
  "#665191",  # roxo médio
  "#a05195",  # rosa acinzentado
  "#d45087",  # rosa intenso
  "#f95d6a",  # vermelho suave
  "#ffbe7a"   # pêssego
)

caminho_curvaSobrevivencia <- "resultados/Exploratoria"

#Gender
km_gender <- survfit(Surv(stag, event) ~ gender, conf.int=T)
summary(km_gender)

gender_sobrevivencia <- ggsurvplot(km_gender, 
           data = turnover,
           conf.int = FALSE,
           xlab = "Meses",
           ylab = "S(t)",
           legend = "right",        # posição
           legend.title = "Gender",
           font.legend = 8,         # tamanho
           legend.ncol = 2,
           palette = cores_grupos, # colunas
           # censor = F, #Censura,
           censor.shape = 124,   # barra vertical
           censor.size = 3
           )  
ggsave("CurvaSobrevivenciaGender.png", 
       plot = gender_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 

#Teste de Comparação
gender_logRank <- survdiff(Surv(stag, event) ~ gender, rho = 0)
#O log rank diz que não tem diferença entre os grupos

gender_wilcoxon <- survdiff(Surv(stag, event) ~ gender, rho = 1 )
#O wilcoxon diz que não tem diferença entre os grupos 

#A 5% de significância não há diferença entre as curvas


#*** Industry (Refazer gráfico, mas potencial variável) ***

km_industry <- survfit(Surv(stag, event) ~ industry, conf.int=T)
summary(km_industry)

ggsurvplot(km_industry, 
           data = turnover,
           conf.int = FALSE,
           xlab = "Meses",
           ylab = "S(t)",
           legend = "right",        # posição
           legend.title = "Grupos",
           font.legend = 8,         # tamanho
           legend.ncol = 2,
           palette = cores_grupos)         # colunas


industry_sobrevivencia <- ggsurvplot(km_industry, 
                                   data = turnover,
                                   conf.int = FALSE,
                                   xlab = "Meses",
                                   ylab = "S(t)",
                                   legend = "right",        # posição
                                   legend.title = "Industry",
                                   font.legend = 8,         # tamanho
                                   legend.ncol = 2,
                                   palette = cores_grupos, # colunas
                                   # censor = F, #Censura,
                                   censor.shape = 124,   # barra vertical
                                   censor.size = 3
)  
ggsave("CurvaSobrevivenciaIndustry.png", 
       plot = industry_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Teste de Comparação
industry_logRank <- survdiff(Surv(stag, event) ~ industry, rho = 0)
#O log rank diz que tem diferença entre os grupos (mas tem muitos cruzamentos, então não é confiável)

industry_wilcoxon <- survdiff(Surv(stag, event) ~ industry, rho = 1 )
#O wilcoxon diz que tem diferença entre os grupos 

#*** Profession (candidata) ***
km_profession <- survfit(Surv(stag, event) ~ profession, conf.int=T)
summary(km_profession)

profession_sobrevivencia <- ggsurvplot(km_profession, 
                                     data = turnover,
                                     conf.int = FALSE,
                                     xlab = "Meses",
                                     ylab = "S(t)",
                                     legend = "right",        # posição
                                     legend.title = "Profession",
                                     font.legend = 8,         # tamanho
                                     legend.ncol = 2,
                                     palette = cores_grupos, # colunas
                                     # censor = F, #Censura,
                                     censor.shape = 124,   # barra vertical
                                     censor.size = 3
)  
ggsave("CurvaSobrevivenciaProfession.png", 
       plot = profession_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Teste de Comparação
Profession_logRank <- survdiff(Surv(stag, event) ~ profession, rho = 0)
#O log rank diz que tem diferença entre os grupos (mas tem muitos cruzamentos, então não é confiável)

Profession_wilcoxon <- survdiff(Surv(stag, event) ~ profession, rho = 1 )
#O wilcoxon diz que tem diferença entre os grupos 


#*** Traffic (candidata) ***
km_traffic <- survfit(Surv(stag, event) ~ traffic, conf.int=T)
summary(km_traffic)

traffic_sobrevivencia <- ggsurvplot(km_traffic, 
                                       data = turnover,
                                       conf.int = FALSE,
                                       xlab = "Meses",
                                       ylab = "S(t)",
                                       legend = "right",        # posição
                                       legend.title = "Traffic",
                                       font.legend = 8,         # tamanho
                                       legend.ncol = 2,
                                       palette = cores_grupos, # colunas
                                       # censor = F, #Censura,
                                       censor.shape = 124,   # barra vertical
                                       censor.size = 3
)  
ggsave("CurvaSobrevivenciaTraffic.png", 
       plot = traffic_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 



#Teste de Comparação
Traffic_logRank <- survdiff(Surv(stag, event) ~ traffic, rho = 0)
#O log rank diz que tem diferença entre os grupos (mas tem muitos cruzamentos, então não é confiável)

Traffic_wilcoxon <- survdiff(Surv(stag, event) ~ traffic, rho = 1 )
#O wilcoxon diz que tem diferença entre os grupos 

#Coach 
km_coach <- survfit(Surv(stag, event) ~ coach, conf.int=T)
summary(km_coach)

coach_sobrevivencia <- ggsurvplot(km_coach, 
                                    data = turnover,
                                    conf.int = FALSE,
                                    xlab = "Meses",
                                    ylab = "S(t)",
                                    legend = "right",        # posição
                                    legend.title = "Coach",
                                    font.legend = 8,         # tamanho
                                    legend.ncol = 2,
                                    palette = cores_grupos, # colunas
                                    # censor = F, #Censura,
                                    censor.shape = 124,   # barra vertical
                                    censor.size = 3
)  
ggsave("CurvaSobrevivenciaCoach.png", 
       plot = coach_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Teste de Comparação
coach_logRank <- survdiff(Surv(stag, event) ~ coach, rho = 0)
#O log rank diz que não tem diferença entre os grupos (mas tem muitos cruzamentos, então não é confiável)

coach_wilcoxon <- survdiff(Surv(stag, event) ~ coach, rho = 1 )
#O wilcoxon diz que não tem diferença entre os grupos 


#head gender
km_HeadGender <- survfit(Surv(stag, event) ~ head_gender, conf.int=T)
summary(km_HeadGender)
plot(km_HeadGender, conf.int = F, xlab = "Meses", ylab = "S(t)", mark.time = T,
     col = cores_grupos,
)
legend("topright",
       legend = names(km_HeadGender$strata),
       col= cores_grupos, 
       lty = 2
)


headGender_sobrevivencia <- ggsurvplot(km_HeadGender, 
                                  data = turnover,
                                  conf.int = FALSE,
                                  xlab = "Meses",
                                  ylab = "S(t)",
                                  legend = "right",        # posição
                                  legend.title = "Head Gender",
                                  font.legend = 8,         # tamanho
                                  legend.ncol = 2,
                                  palette = cores_grupos, # colunas
                                  # censor = F, #Censura,
                                  censor.shape = 124,   # barra vertical
                                  censor.size = 3
)  
ggsave("CurvaSobrevivenciaHeadGender.png", 
       plot = headGender_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Teste de Comparação
HeadGender_logRank <- survdiff(Surv(stag, event) ~ head_gender, rho = 0)
#O log rank diz que não tem diferença entre os grupos (mas tem cruzamentos, então ficar atento!)

HeadGender_wilcoxon <- survdiff(Surv(stag, event) ~ head_gender, rho = 1 )
#O wilcoxon diz que não tem diferença entre os grupos 


#*** GreyWage (Candidata) ***
km_GreyWage <- survfit(Surv(stag, event) ~ greywage, conf.int=T)
summary(km_GreyWage)
plot(km_GreyWage, conf.int = F, xlab = "Meses", ylab = "S(t)", mark.time = T,
     col = cores_grupos,
)
legend("topright",
       legend = names(km_GreyWage$strata),
       col= cores_grupos, 
       lty = 2
)


#Teste de Comparação
GreyWage_logRank <- survdiff(Surv(stag, event) ~ greywage, rho = 0)
#O log rank diz que tem diferença entre os grupos

GreyWage_wilcoxon <- survdiff(Surv(stag, event) ~ greywage, rho = 1 )
#O wilcoxon diz que tem diferença entre os grupos 


#*** Way (Candidata) ***
km_Way <- survfit(Surv(stag, event) ~ way, conf.int=T)
summary(km_Way)

way_sobrevivencia <- ggsurvplot(km_Way, 
                                data = turnover,
                                conf.int = FALSE,
                                xlab = "Meses",
                                ylab = "S(t)",
                                legend = "right",        # posição
                                legend.title = "Way",
                                font.legend = 8,         # tamanho
                                legend.ncol = 2,
                                palette = cores_grupos, # colunas
                                # censor = F, #Censura,
                                censor.shape = 124,   # barra vertical
                                censor.size = 3
)  
ggsave("CurvaSobrevivenciaWay.png", 
       plot = way_sobrevivencia$plot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Teste de Comparação
Way_logRank <- survdiff(Surv(stag, event) ~ way, rho = 0)
#O log rank diz que tem diferença entre os grupos (prefere-se utilizar o log rank por não cruzar)

Way_wilcoxon <- survdiff(Surv(stag, event) ~ way, rho = 1 )
#O wilcoxon diz que tem diferença entre os grupos 


#Age
age_boxplot <- ggplot(turnover) +
  aes(
    x = as.factor(event),
    y = age
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Censura", y = "Age")+
  theme_classic()

ggsave("BoxplotAge.png", 
       plot = age_boxplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Extraversion
extraversion_boxplot <- ggplot(turnover) +
  aes(
    x = as.factor(event),
    y = extraversion
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Censura", y = "Extraversion")+
  theme_classic()

ggsave("BoxplotExtraversion.png", 
       plot = extraversion_boxplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Independ
independ_boxplot <- ggplot(turnover) +
  aes(
    x = as.factor(event),
    y = independ
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Censura", y = "Independ")+
  theme_classic()

ggsave("BoxplotIndepend.png", 
       plot = independ_boxplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 

#selfcontrol
selfcontrol_boxplot <- ggplot(turnover) +
  aes(
    x = as.factor(event),
    y = selfcontrol
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Censura", y = "Selfcontrol")+
  theme_classic()

ggsave("BoxplotSelfControl.png", 
       plot = selfcontrol_boxplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#anxiety
anxiety_boxplot <- ggplot(turnover) +
  aes(
    x = as.factor(event),
    y = anxiety
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Censura", y = "Anxiety")+
  theme_classic()

ggsave("BoxplotAnxiety.png", 
       plot = anxiety_boxplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#Novator
novator_boxplot <- ggplot(turnover) +
  aes(
    x = as.factor(event),
    y = novator
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Censura", y = "Novator")+
  theme_classic()

ggsave("BoxplotNovator.png", 
       plot = novator_boxplot, 
       width = 8, 
       height = 6, 
       dpi = 300,
       path = caminho_curvaSobrevivencia) 


#-----------------------------------------------------------------------------------------------#----
#4) Sugestão: construir gráficos do Tempo Total em Teste (gráfico TTT) para variáveis categóricas.
#-----------------------------------------------------------------------------------------------#----

pasta_destino_TTT <- "resultados/graficos_TTT"

# Cria a pasta se ela não existir (recursivamente)
if (!dir.exists(pasta_destino_TTT)) {
  dir.create(pasta_destino_TTT, recursive = TRUE)
}

#Gender
genero <- unique(gender)

for (i in seq_along(genero)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_gender_", genero[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(gender == genero[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = genero[i])
  dev.off()
}

#Industry
industria <- unique(industry)

for (i in seq_along(industria)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_industry_", industria[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(industry == industria[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = industria[i])
  dev.off()
}


#profession
profissao <- unique(profession)

for (i in seq_along(profissao)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_profession_", profissao[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(profession == profissao[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = profissao[i])
  dev.off()
}

#traffic
trafego<- unique(traffic)

for (i in seq_along(trafego)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_traffic_", trafego[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(traffic == trafego[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = trafego[i])
  dev.off()
}

#coach
treinador<- unique(coach)

for (i in seq_along(treinador)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_coach_", treinador[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(coach == treinador[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = treinador[i])
  dev.off()
}

#HeadGender

generoChefe<- unique(head_gender)

for (i in seq_along(generoChefe)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_coach_", generoChefe[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(coach == generoChefe[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = generoChefe[i])
  dev.off()
}

#greywage

salario_cinza<- unique(greywage)

for (i in seq_along(salario_cinza)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_greywage_", salario_cinza[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(greywage == salario_cinza[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = salario_cinza[i])
  dev.off()
}

#Way

transporte <- unique(way)

for (i in seq_along(transporte)) {
  # Define o caminho completo do arquivo
  arquivo <- file.path(pasta_destino_TTT, paste0("grafico_TTT_way_", transporte[i], ".png"))
  
  png(filename = arquivo, width = 800, height = 600)  # opcional: definir dimensões
  turnover %>% 
    filter(way == transporte[i]) %>%
    pull(stag) %>% 
    TTT(col = "red", lwd = 2.5, grid = TRUE, lty = 2)
  title(main = transporte[i])
  dev.off()
}


#-----------------------------------------------------------------------------------------------#----
#5) Com base nos resultados encontrados, discutir quais funções de densidade de probabilidade 
#podem ter gerado a resposta do estudo?
#-----------------------------------------------------------------------------------------------#----

#-----------------------------------------------------------------------------------------------#----
#6) Utilize um método gráfico para avaliar o ajuste das distribuições sugeridas no item (5). Se 
#possível utilize teste de hipótese e medidas AIC, BIC, AICc.
#-----------------------------------------------------------------------------------------------#----

tempo_km <-  km$time
turnover_limpo <- turnover %>% 
  na.omit()


modelo_intercepto_exp <- survreg(Surv(stag, event) ~ 1, data = turnover_limpo, dist = "exponential")
summary(modelo_intercepto_exp)

alfa_exp <- exp(modelo_intercepto_exp$coefficients[1])
sobrev_exp <- exp(-tempo_km/alfa_exp)


modelo_intercepto_weibull <- survreg(Surv(stag, event) ~ 1, data = turnover_limpo, dist = "weibull")
summary(modelo_intercepto_weibull)

alfa_weibull <- exp(modelo_intercepto_weibull$coefficients)
gamma_weibull <- 1/modelo_intercepto_weibull$scale

sobrev_weibull <- exp(-(tempo_km/alfa_weibull)^gamma_weibull)


modelo_intercepto_lognormal  <- survreg(Surv(stag, event) ~ 1, data = turnover_limpo, dist = "lognormal")

media_log <- modelo_intercepto_lognormal$coefficients 
desvioPadrao_log <- modelo_intercepto_lognormal$scale

sobrev_lognormal <- (1-plnorm(tempo_km, media_log, desvioPadrao_log))


modelo_intercepto_loglogistic  <- survreg(Surv(stag, event) ~ 1, data = turnover_limpo, dist = "loglogistic")

alfa_loglogistic <- exp(modelo_intercepto_loglogistic$coefficients)
gamma_loglogistic <- 1/modelo_intercepto_loglogistic$scale

sobrev_loglogistic <- 1/(1+(tempo_km/alfa_loglogistic)^gamma_loglogistic)


# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Ajuste_Distribuicoes/Distribuicoes_Implementadas.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(km, conf.int = FALSE, mark.time = FALSE,
     xlab = "Tempo", ylab = "Sobrevivência",
     main = "Comparação de distribuições")
lines(tempo_km, sobrev_exp, col = "blue", lwd=2)
lines(tempo_km, sobrev_weibull, col = "red", lwd=2)
lines(tempo_km, sobrev_lognormal, col = "green", lwd=2)
lines(tempo_km, sobrev_loglogistic, col = "orange", lwd=2)
legend("topright",
       legend = c("Exponencial", "Weibull", "Log-normal", "Log-logística"),
       col= c("blue", "red", "green", "orange"), 
       lwd = 2
)
# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


#Pelo gráfico o Weibull é a que mais se adequa
# Rank Gráfico
# Weibull
# Exponencial
# log logistica
# log normal


n <- nrow(turnover_limpo)
p <- length(turnover_limpo)-2

n/p

#Como n/p é maior que 40, recomenda-se o AIC

aic_val <- c(AIC(modelo_intercepto_exp), 
             AIC(modelo_intercepto_weibull), 
             AIC(modelo_intercepto_lognormal), 
             AIC(modelo_intercepto_loglogistic))



bic_val <- c(BIC(modelo_intercepto_exp), 
             BIC(modelo_intercepto_weibull), 
             BIC(modelo_intercepto_lognormal), 
             BIC(modelo_intercepto_loglogistic))

#Pelas medidas a loglogistica é a melhor

#Rank AIC
# loglogistic	6032,215
# Weibull	6032,548
# Lognormal	6033,234
# Exponencial	6034,613

#Rank BIC
# Exponencial	6039,642
# loglogistic	6042,273
# Weibull	6042,606
# Lognormal	6043,292



#==============================Log-Normal===============================================

#-----------------------------------------------------------------------------------------------#----
#7) Utilize a densidade definida na etapa 6 e ajuste modelos de regressão com uma única covariável. 
#-----------------------------------------------------------------------------------------------#----


lognormal_gender  <- survreg(Surv(stag, event) ~ gender, data = turnover_limpo, dist = "lognormal")
summary(lognormal_gender)

#*
lognormal_age  <- survreg(Surv(stag, event) ~ age, data = turnover_limpo, dist = "lognormal")
summary(lognormal_age)

lognormal_industry  <- survreg(Surv(stag, event) ~ industry, data = turnover_limpo, dist = "lognormal")
summary(lognormal_industry)

lognormal_profession  <- survreg(Surv(stag, event) ~ profession, data = turnover_limpo, dist = "lognormal")
summary(lognormal_profession)

lognormal_traffic  <- survreg(Surv(stag, event) ~ traffic, data = turnover_limpo, dist = "lognormal")
summary(lognormal_traffic)

lognormal_coach  <- survreg(Surv(stag, event) ~ coach, data = turnover_limpo, dist = "lognormal")
summary(lognormal_coach)

lognormal_headGender  <- survreg(Surv(stag, event) ~ head_gender, data = turnover_limpo, dist = "lognormal")
summary(lognormal_headGender)

lognormal_greywage  <- survreg(Surv(stag, event) ~ greywage, data = turnover_limpo, dist = "lognormal")
summary(lognormal_greywage)

lognormal_way <- survreg(Surv(stag, event) ~ way, data = turnover_limpo, dist = "lognormal")
summary(lognormal_way)

lognormal_extraversion <- survreg(Surv(stag, event) ~ extraversion, data = turnover_limpo, dist = "lognormal")
summary(lognormal_extraversion)

lognormal_independ <- survreg(Surv(stag, event) ~ independ, data = turnover_limpo, dist = "lognormal")
summary(lognormal_independ)

lognormal_selfcontrol <- survreg(Surv(stag, event) ~ selfcontrol, data = turnover_limpo, dist = "lognormal")
summary(lognormal_selfcontrol)

lognormal_anxiety <- survreg(Surv(stag, event) ~ anxiety, data = turnover_limpo, dist = "lognormal")
summary(lognormal_anxiety)

lognormal_novator <- survreg(Surv(stag, event) ~ novator, data = turnover_limpo, dist = "lognormal")
summary(lognormal_novator)



#-----------------------------------------------------------------------------------------------#----
#8) Construir um modelo completo de regressão com todas as covariáveis que foram significativas ao 
#nível de 10% na etapa 7
#-----------------------------------------------------------------------------------------------#----

modelo_lognormal <- survreg(Surv(stag, event) ~ gender +
                            age +
                            industry +
                            profession +
                            traffic +
                            coach +
                            head_gender +
                            greywage +
                            way +
                            extraversion +
                            independ +
                            selfcontrol +
                            anxiety +
                            novator, 
                            data = turnover_limpo, dist = "lognormal")
summary(modelo_lognormal)

#-----------------------------------------------------------------------------------------------#----
# 9) Excluir covariáveis não significativas (a nível de 10%) na etapa 8 uma de cada vez. Se essa etapa 
# não se aplica a esses dados, passe para a etapa10
#-----------------------------------------------------------------------------------------------#----

stepAIC(modelo_lognormal, direction = "backward")

#Surv(stag, event) ~ age + industry + profession + traffic + coach + greywage + selfcontrol + anxiety, data = turnover_limpo, 
#dist = "lognormal"

stepAIC(modelo_intercepto_lognormal, 
        direction = "forward",
        scope = list(lower = formula(modelo_intercepto_lognormal), upper = modelo_lognormal))
# survreg(formula = Surv(stag, event) ~ greywage + industry + traffic + 
#           age + profession + selfcontrol + anxiety + coach, data = turnover_limpo, 
#         dist = "lognormal")


stepAIC(modelo_intercepto_lognormal, 
        scope = list(upper = modelo_lognormal, lower = modelo_intercepto_lognormal), 
        direction = "both")

#survreg(formula = Surv(stag, event) ~ greywage + industry + traffic + 
#          age + profession + selfcontrol + anxiety + coach, data = turnover_limpo, 
#        dist = "lognormal")


#OBS: TODOS os métodos chegaram a mesma seleção de variáveis

modelo_lognormal_8va <- survreg(Surv(stag, event) ~ 
                                  age +
                                  industry +
                                  profession +
                                  traffic +
                                  coach +
                                  greywage +
                                  selfcontrol +
                                  anxiety, 
                                data = turnover_limpo, dist = "lognormal")
summary(modelo_lognormal_8va)



#-----------------------------------------------------------------------------------------------#----
# 13) Verifique a qualidade do ajuste do modelo final
#-----------------------------------------------------------------------------------------------#----
#cox snell

tempo <- turnover_limpo$stag
mi <- modelo_lognormal_8va$linear.predictors


Smod <- pnorm((-log(tempo)+mi)/modelo_lognormal_8va$scale)
ei <- (-log(Smod)) #resíduo de Cox Snell

KMew <- survfit(Surv(ei, turnover_limpo$event)~1,conf.int = F)
te <- KMew$time #Resíduo de Cox-Snell
ste <- KMew$surv
sexp <- exp(-te)

# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_lognormal_8va_KMxEP.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(ste, sexp, 
     xlab = "S(ei): Kaplan-Meier",
     ylab = "S(ei): Exponencial Padrão")

# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_lognormal_8va_CoxSnell.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(KMew, conf.int = F, 
     xlab = "Resíduos de Cox-Snell",
     ylab = 'Sobrevivência Estimada')
lines(te, sexp, lty=2, col=2)
legend(
  20,
  1.0,
  lty = c(1,2),
  col = c(1,2),
  c("Kaplan-Meier", "Exponencial Padrão"),
  cex=0.8,
  bty = "n"
)
# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()



#martingal 

martingal <- turnover_limpo$event-ei

par(mfrow=c(1,1))
plot(tempo, martingal,
     xlab = "log(tempo)",
     ylab = "Residuo Martingal",  
     pch = turnover_limpo$event+1
     
)

plot(rank(tempo), martingal,
     xlab = "Rank das Observações",
     ylab = "Resíduos Martingal",
     pch = turnover_limpo$event+1
)

#identify(rank(tempo), martingal)




#deviance
devw <- (martingal/abs(martingal))*(-2*(martingal+turnover_limpo$event*log(turnover_limpo$event-martingal)))^(1/2)
plot(tempo, devw,
     xlab = "log(tempo)",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event)

plot(rank(tempo), devw,
     xlab = "ranks das observações",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event+1)

#identify(rank(tempo), devw)



#======================================WEIBULL========================================================
#-----------------------------------------------------------------------------------------------#----
#7) Utilize a densidade definida na etapa 6 e ajuste modelos de regressão com uma única covariável. 
#-----------------------------------------------------------------------------------------------#----


weibull_gender  <- survreg(Surv(stag, event) ~ gender, data = turnover_limpo, dist = "weibull")
summary(weibull_gender)

#*
weibull_age  <- survreg(Surv(stag, event) ~ age, data = turnover_limpo, dist = "weibull")
summary(weibull_age)

weibull_industry  <- survreg(Surv(stag, event) ~ industry, data = turnover_limpo, dist = "weibull")
summary(weibull_industry)

weibull_profession  <- survreg(Surv(stag, event) ~ profession, data = turnover_limpo, dist = "weibull")
summary(weibull_profession)

weibull_traffic  <- survreg(Surv(stag, event) ~ traffic, data = turnover_limpo, dist = "weibull")
summary(weibull_traffic)

weibull_coach  <- survreg(Surv(stag, event) ~ coach, data = turnover_limpo, dist = "weibull")
summary(weibull_coach)

weibull_headGender  <- survreg(Surv(stag, event) ~ head_gender, data = turnover_limpo, dist = "weibull")
summary(weibull_headGender)

weibull_greywage  <- survreg(Surv(stag, event) ~ greywage, data = turnover_limpo, dist = "weibull")
summary(weibull_greywage)

weibull_way <- survreg(Surv(stag, event) ~ way, data = turnover_limpo, dist = "weibull")
summary(weibull_way)

weibull_extraversion <- survreg(Surv(stag, event) ~ extraversion, data = turnover_limpo, dist = "weibull")
summary(weibull_extraversion)

weibull_independ <- survreg(Surv(stag, event) ~ independ, data = turnover_limpo, dist = "weibull")
summary(weibull_independ)

weibull_selfcontrol <- survreg(Surv(stag, event) ~ selfcontrol, data = turnover_limpo, dist = "weibull")
summary(weibull_selfcontrol)

weibull_anxiety <- survreg(Surv(stag, event) ~ anxiety, data = turnover_limpo, dist = "weibull")
summary(weibull_anxiety)

weibull_novator <- survreg(Surv(stag, event) ~ novator, data = turnover_limpo, dist = "weibull")
summary(weibull_novator)



#-----------------------------------------------------------------------------------------------#----
#8) Construir um modelo completo de regressão com todas as covariáveis que foram significativas ao 
#nível de 10% na etapa 7
#-----------------------------------------------------------------------------------------------#----

modelo_weibull <- survreg(Surv(stag, event) ~ gender +
                              age +
                              industry +
                              profession +
                              traffic +
                              coach +
                              head_gender +
                              greywage +
                              way +
                              extraversion +
                              independ +
                              selfcontrol +
                              anxiety +
                              novator, 
                            data = turnover_limpo, dist = "weibull")
summary(modelo_weibull)

#-----------------------------------------------------------------------------------------------#----
# 9) Excluir covariáveis não significativas (a nível de 10%) na etapa 8 uma de cada vez. Se essa etapa 
# não se aplica a esses dados, passe para a etapa10
#-----------------------------------------------------------------------------------------------#----

stepAIC(modelo_weibull, direction = "backward")

#Surv(stag, event) ~ age + industry + profession + traffic + greywage + way + selfcontrol + anxiety, data = turnover_limpo, 
#dist = "weibull"

stepAIC(modelo_intercepto_weibull, 
        direction = "forward",
        scope = list(lower = formula(modelo_intercepto_weibull), upper = modelo_weibull))
# survreg(formula = Surv(stag, event) ~ industry + traffic + greywage + 
#           age + selfcontrol + anxiety + profession + way, data = turnover_limpo, 
#         dist = "weibull")

stepAIC(modelo_intercepto_weibull, 
        scope = list(upper = modelo_weibull, lower = modelo_intercepto_weibull), 
        direction = "both")

# survreg(formula = Surv(stag, event) ~ industry + traffic + greywage + 
#           age + selfcontrol + anxiety + profession + way, data = turnover_limpo, 
#         dist = "weibull")

#OBS: TODOS os métodos chegaram a mesma seleção de variáveis

modelo_weibull_8va <- survreg(Surv(stag, event) ~ 
                                  age +
                                  industry +
                                  profession +
                                  traffic +
                                  greywage +
                                  way +
                                  selfcontrol +
                                  anxiety,
                                data = turnover_limpo, dist = "weibull")
summary(modelo_weibull_8va)



#-----------------------------------------------------------------------------------------------#----
# 13) Verifique a qualidade do ajuste do modelo final
#-----------------------------------------------------------------------------------------------#----
#cox snell

tempo <- turnover_limpo$stag
mi <- modelo_weibull_8va$linear.predictors


Smod <- pnorm((-log(tempo)+mi)/modelo_weibull_8va$scale)
ei <- (-log(Smod)) #resíduo de Cox Snell

KMew <- survfit(Surv(ei, turnover_limpo$event)~1,conf.int = F)
te <- KMew$time #Resíduo de Cox-Snell
ste <- KMew$surv
sexp <- exp(-te)

# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_weibull_8va_KMxEP.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(ste, sexp, 
     xlab = "S(ei): Kaplan-Meier",
     ylab = "S(ei): Exponencial Padrão")

# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_weibull_8va_CoxSnell.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(KMew, conf.int = F, 
     xlab = "Resíduos de Cox-Snell",
     ylab = 'Sobrevivência Estimada')
lines(te, sexp, lty=2, col=2)
legend(
  20,
  1.0,
  lty = c(1,2),
  col = c(1,2),
  c("Kaplan-Meier", "Exponencial Padrão"),
  cex=0.8,
  bty = "n"
)
# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


#Ajuste terrível, superestimou toda curva.

#martingal 

martingal <- turnover_limpo$event-ei

par(mfrow=c(1,1))
plot(tempo, martingal,
     xlab = "log(tempo)",
     ylab = "Residuo Martingal",  
     pch = turnover_limpo$event+1
     
)

plot(rank(tempo), martingal,
     xlab = "Rank das Observações",
     ylab = "Resíduos Martingal",
     pch = turnover_limpo$event+1
)

#identify(rank(tempo), martingal)

#deviance
devw <- (martingal/abs(martingal))*(-2*(martingal+turnover_limpo$event*log(turnover_limpo$event-martingal)))^(1/2)
plot(tempo, devw,
     xlab = "log(tempo)",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event)

plot(rank(tempo), devw,
     xlab = "ranks das observações",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event+1)

#identify(rank(tempo), devw)





#===============================Exponencial==========================================================

#-----------------------------------------------------------------------------------------------#----
#7) Utilize a densidade definida na etapa 6 e ajuste modelos de regressão com uma única covariável. 
#-----------------------------------------------------------------------------------------------#----


exponential_gender  <- survreg(Surv(stag, event) ~ gender, data = turnover_limpo, dist = "exponential")
summary(exponential_gender)

#*
exponential_age  <- survreg(Surv(stag, event) ~ age, data = turnover_limpo, dist = "exponential")
summary(exponential_age)

exponential_industry  <- survreg(Surv(stag, event) ~ industry, data = turnover_limpo, dist = "exponential")
summary(exponential_industry)

exponential_profession  <- survreg(Surv(stag, event) ~ profession, data = turnover_limpo, dist = "exponential")
summary(exponential_profession)

exponential_traffic  <- survreg(Surv(stag, event) ~ traffic, data = turnover_limpo, dist = "exponential")
summary(exponential_traffic)

exponential_coach  <- survreg(Surv(stag, event) ~ coach, data = turnover_limpo, dist = "exponential")
summary(exponential_coach)

exponential_headGender  <- survreg(Surv(stag, event) ~ head_gender, data = turnover_limpo, dist = "exponential")
summary(exponential_headGender)

exponential_greywage  <- survreg(Surv(stag, event) ~ greywage, data = turnover_limpo, dist = "exponential")
summary(exponential_greywage)

exponential_way <- survreg(Surv(stag, event) ~ way, data = turnover_limpo, dist = "exponential")
summary(exponential_way)

exponential_extraversion <- survreg(Surv(stag, event) ~ extraversion, data = turnover_limpo, dist = "exponential")
summary(exponential_extraversion)

exponential_independ <- survreg(Surv(stag, event) ~ independ, data = turnover_limpo, dist = "exponential")
summary(exponential_independ)

exponential_selfcontrol <- survreg(Surv(stag, event) ~ selfcontrol, data = turnover_limpo, dist = "exponential")
summary(exponential_selfcontrol)

exponential_anxiety <- survreg(Surv(stag, event) ~ anxiety, data = turnover_limpo, dist = "exponential")
summary(exponential_anxiety)

exponential_novator <- survreg(Surv(stag, event) ~ novator, data = turnover_limpo, dist = "exponential")
summary(exponential_novator)



#-----------------------------------------------------------------------------------------------#----
#8) Construir um modelo completo de regressão com todas as covariáveis que foram significativas ao 
#nível de 10% na etapa 7
#-----------------------------------------------------------------------------------------------#----

modelo_exponential <- survreg(Surv(stag, event) ~ gender +
                            age +
                            industry +
                            profession +
                            traffic +
                            coach +
                            head_gender +
                            greywage +
                            way +
                            extraversion +
                            independ +
                            selfcontrol +
                            anxiety +
                            novator, 
                          data = turnover_limpo, dist = "exponential")
summary(modelo_exponential)

#-----------------------------------------------------------------------------------------------#----
# 9) Excluir covariáveis não significativas (a nível de 10%) na etapa 8 uma de cada vez. Se essa etapa 
# não se aplica a esses dados, passe para a etapa10
#-----------------------------------------------------------------------------------------------#----

stepAIC(modelo_exponential, direction = "backward")

#Surv(stag, event) ~ age + industry  + traffic + greywage + way + selfcontrol + anxiety, data = turnover_limpo, 
#dist = "exponential"

stepAIC(modelo_intercepto_exp, 
        direction = "forward",
        scope = list(lower = formula(modelo_intercepto_exp), upper = modelo_exponential))
# survreg(formula = Surv(stag, event) ~ industry + traffic + greywage + 
#           age + selfcontrol + anxiety  + way, data = turnover_limpo, 
#         dist = "exponential")

stepAIC(modelo_intercepto_exp, 
        scope = list(upper = modelo_exponential, lower = modelo_intercepto_exp), 
        direction = "both")

# survreg(formula = Surv(stag, event) ~ industry + traffic + greywage + 
#           age + selfcontrol + anxiety + way, data = turnover_limpo, 
#         dist = "exponential")

#OBS: TODOS os métodos chegaram a mesma seleção de variáveis

modelo_exponential_7va <- survreg(Surv(stag, event) ~ 
                                    age  +
                                    industry  +
                                    traffic  +
                                    greywage  +
                                    way  +
                                    selfcontrol  +
                                    anxiety  ,
                              data = turnover_limpo, dist = "exponential")
summary(modelo_exponential_7va)



#-----------------------------------------------------------------------------------------------#----
# 13) Verifique a qualidade do ajuste do modelo final
#-----------------------------------------------------------------------------------------------#----
#cox snell

tempo <- turnover_limpo$stag
mi <- modelo_exponential_7va$linear.predictors


Smod <- pnorm((-log(tempo)+mi)/modelo_exponential_7va$scale)
ei <- (-log(Smod)) #resíduo de Cox Snell

KMew <- survfit(Surv(ei, turnover_limpo$event)~1,conf.int = F)
te <- KMew$time #Resíduo de Cox-Snell
ste <- KMew$surv
sexp <- exp(-te)

# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_exponential_7va_KMxEP.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(ste, sexp, 
     xlab = "S(ei): Kaplan-Meier",
     ylab = "S(ei): Exponencial Padrão")

# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_exponential_7va_CoxSnell.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(KMew, conf.int = F, 
     xlab = "Resíduos de Cox-Snell",
     ylab = 'Sobrevivência Estimada')
lines(te, sexp, lty=2, col=2)
legend(
  20,
  1.0,
  lty = c(1,2),
  col = c(1,2),
  c("Kaplan-Meier", "Exponencial Padrão"),
  cex=0.8,
  bty = "n"
)
# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


#Ajuste terrível, superestimou toda curva.

#martingal 

martingal <- turnover_limpo$event-ei

par(mfrow=c(1,1))
plot(tempo, martingal,
     xlab = "log(tempo)",
     ylab = "Residuo Martingal",  
     pch = turnover_limpo$event+1
     
)

plot(rank(tempo), martingal,
     xlab = "Rank das Observações",
     ylab = "Resíduos Martingal",
     pch = turnover_limpo$event+1
)

#identify(rank(tempo), martingal)

#deviance
devw <- (martingal/abs(martingal))*(-2*(martingal+turnover_limpo$event*log(turnover_limpo$event-martingal)))^(1/2)
plot(tempo, devw,
     xlab = "log(tempo)",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event)

plot(rank(tempo), devw,
     xlab = "ranks das observações",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event+1)

#identify(rank(tempo), devw)






#=========================================log logistica================================================


#-----------------------------------------------------------------------------------------------#----
#7) Utilize a densidade definida na etapa 6 e ajuste modelos de regressão com uma única covariável. 
#-----------------------------------------------------------------------------------------------#----


loglogistic_gender  <- survreg(Surv(stag, event) ~ gender, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_gender)

#*
loglogistic_age  <- survreg(Surv(stag, event) ~ age, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_age)

loglogistic_industry  <- survreg(Surv(stag, event) ~ industry, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_industry)

loglogistic_profession  <- survreg(Surv(stag, event) ~ profession, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_profession)

loglogistic_traffic  <- survreg(Surv(stag, event) ~ traffic, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_traffic)

loglogistic_coach  <- survreg(Surv(stag, event) ~ coach, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_coach)

loglogistic_headGender  <- survreg(Surv(stag, event) ~ head_gender, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_headGender)

loglogistic_greywage  <- survreg(Surv(stag, event) ~ greywage, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_greywage)

loglogistic_way <- survreg(Surv(stag, event) ~ way, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_way)

loglogistic_extraversion <- survreg(Surv(stag, event) ~ extraversion, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_extraversion)

loglogistic_independ <- survreg(Surv(stag, event) ~ independ, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_independ)

loglogistic_selfcontrol <- survreg(Surv(stag, event) ~ selfcontrol, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_selfcontrol)

loglogistic_anxiety <- survreg(Surv(stag, event) ~ anxiety, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_anxiety)

loglogistic_novator <- survreg(Surv(stag, event) ~ novator, data = turnover_limpo, dist = "loglogistic")
summary(loglogistic_novator)



#-----------------------------------------------------------------------------------------------#----
#8) Construir um modelo completo de regressão com todas as covariáveis que foram significativas ao 
#nível de 10% na etapa 7
#-----------------------------------------------------------------------------------------------#----

modelo_loglogistic <- survreg(Surv(stag, event) ~ gender +
                                age +
                                industry +
                                profession +
                                traffic +
                                coach +
                                head_gender +
                                greywage +
                                way +
                                extraversion +
                                independ +
                                selfcontrol +
                                anxiety +
                                novator, 
                              data = turnover_limpo, dist = "loglogistic")
summary(modelo_loglogistic)

#-----------------------------------------------------------------------------------------------#----
# 9) Excluir covariáveis não significativas (a nível de 10%) na etapa 8 uma de cada vez. Se essa etapa 
# não se aplica a esses dados, passe para a etapa10
#-----------------------------------------------------------------------------------------------#----

stepAIC(modelo_loglogistic, direction = "backward")

# survreg(formula = Surv(stag, event) ~ age + industry + profession + 
#           traffic + greywage + selfcontrol + anxiety, data = turnover_limpo, 
#         dist = "loglogistic")

stepAIC(modelo_intercepto_loglogistic, 
        direction = "forward",
        scope = list(lower = formula(modelo_intercepto_loglogistic), upper = modelo_loglogistic))
# survreg(formula = Surv(stag, event) ~ greywage + industry + traffic + 
#           age + selfcontrol + profession + anxiety, data = turnover_limpo, 
#         dist = "loglogistic")

stepAIC(modelo_intercepto_loglogistic, 
        scope = list(upper = modelo_loglogistic, lower = modelo_intercepto_loglogistic), 
        direction = "both")

# survreg(formula = Surv(stag, event) ~ greywage + industry + traffic + 
#           age + selfcontrol + profession + anxiety, data = turnover_limpo, 
#         dist = "loglogistic")


#OBS: TODOS os métodos chegaram a mesma seleção de variáveis

modelo_loglogistic_7va <- survreg(Surv(stag, event) ~ 
                                    age  +
                                    industry  +
                                    profession  +
                                    traffic  +
                                    greywage  +
                                    selfcontrol  +
                                    anxiety  ,
                                  data = turnover_limpo, dist = "loglogistic")
summary(modelo_loglogistic_7va)



#-----------------------------------------------------------------------------------------------#----
# 13) Verifique a qualidade do ajuste do modelo final
#-----------------------------------------------------------------------------------------------#----
#cox snell

tempo <- turnover_limpo$stag
mi <- modelo_loglogistic_7va$linear.predictors


Smod <- pnorm((-log(tempo)+mi)/modelo_loglogistic_7va$scale)
ei <- (-log(Smod)) #resíduo de Cox Snell

KMew <- survfit(Surv(ei, turnover_limpo$event)~1,conf.int = F)
te <- KMew$time #Resíduo de Cox-Snell
ste <- KMew$surv
sexp <- exp(-te)

# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_loglogistic_7va_KMxEP.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(ste, sexp, 
     xlab = "S(ei): Kaplan-Meier",
     ylab = "S(ei): Exponencial Padrão")

# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


# 1. Abre o arquivo ("liga a impressora")
png(filename = "resultados/Analise_Residuos/Residuo_exponential_7va_CoxSnell.png", width = 800, height = 600)

# 2. Gera o gráfico (ele não vai aparecer na tela do R, vai direto para o arquivo)
plot(KMew, conf.int = F, 
     xlab = "Resíduos de Cox-Snell",
     ylab = 'Sobrevivência Estimada')
lines(te, sexp, lty=2, col=2)
legend(
  20,
  1.0,
  lty = c(1,2),
  col = c(1,2),
  c("Kaplan-Meier", "Exponencial Padrão"),
  cex=0.8,
  bty = "n"
)
# 3. Salva e fecha o arquivo ("ejeta o papel") - ISSO É O MAIS IMPORTANTE!
dev.off()


#Ajuste terrível, superestimou toda curva.

#martingal 

martingal <- turnover_limpo$event-ei

par(mfrow=c(1,1))
plot(tempo, martingal,
     xlab = "log(tempo)",
     ylab = "Residuo Martingal",  
     pch = turnover_limpo$event+1
     
)

plot(rank(tempo), martingal,
     xlab = "Rank das Observações",
     ylab = "Resíduos Martingal",
     pch = turnover_limpo$event+1
)

#identify(rank(tempo), martingal)

#deviance
devw <- (martingal/abs(martingal))*(-2*(martingal+turnover_limpo$event*log(turnover_limpo$event-martingal)))^(1/2)
plot(tempo, devw,
     xlab = "log(tempo)",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event)

plot(rank(tempo), devw,
     xlab = "ranks das observações",
     ylab = "Resíduos Deviance",
     pch = turnover_limpo$event+1)

#identify(rank(tempo), devw)