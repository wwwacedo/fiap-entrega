# Instalar pacotes necessários se ainda não estiverem instalados
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

# Carregar pacotes
library(dplyr)
library(ggplot2)

# Para reprodutibilidade
set.seed(123)
n_meses <- 6
equipes <- paste("Equipe", 1:5)

# Gerando dados simulados

dados <- expand.grid(Equipe = equipes, Mes = 1:n_meses)
dados$Commits <- sample(20:100, size = nrow(dados), replace = TRUE)
dados$RevisoesCodigo <- round(runif(n = nrow(dados), min = 1, max = 5), 2)
dados$TempoDesenvolvimento <- sample(5:30, size = nrow(dados), replace = TRUE)

cat("- - -\n")
print("1. Análise Exploratória")
## ex 1 
# Médias para cada variável
media_commits <- mean(dados$Commits)
media_revisoes <- mean(dados$RevisoesCodigo)
media_tempo <- mean(dados$TempoDesenvolvimento)

# Medianas para cada variável
mediana_commits <- median(dados$Commits)
mediana_revisoes <- median(dados$RevisoesCodigo)
mediana_tempo <- median(dados$TempoDesenvolvimento)

# Imprimir resultados
cat("\nCommits:\n")
cat("Média:", media_commits, "\n")
cat("Mediana:", mediana_commits, "\n")
cat("\nRevisões de Código:\n")
cat("Média:", media_revisoes, "\n")
cat("Mediana:", mediana_revisoes, "\n")
cat("\nTempo de Desenvolvimento:\n")
cat("Média:", media_tempo, "\n")
cat("Mediana:", mediana_tempo, "\n\n")
##ex 1 b
# Calculando estatísticas descritivas para cada equipe
estatisticas_por_equipe <- dados %>%
  group_by(Equipe) %>%
  summarise(
    MediaCommits = mean(Commits),
    SDCommits = sd(Commits),
    MediaRevisoesCodigo = mean(RevisoesCodigo),
    SDRevisoesCodigo = sd(RevisoesCodigo),
    MediaTempoDesenvolvimento = mean(TempoDesenvolvimento),
    SDTempoDesenvolvimento = sd(TempoDesenvolvimento)
  )

# Imprimindo a tabela completa
print(estatisticas_por_equipe)

#############
##ex 2
cat("- - -\n")
print("2. Transformação e Criação de Variáveis")

# Definindo pesos
peso1 <- 1
peso2 <- 2
peso3 <- 3

# Agregando o número total de commits por equipe
total_commits_por_equipe <- aggregate(Commits ~ Equipe, data = dados, sum)

# Mostrando os resultados
print(total_commits_por_equipe)


# Agrupando os dados por equipe e calculando a média de RevisoesCodigo para cada equipe
media_revisoes_por_equipe <- dados %>%
  group_by(Equipe) %>%
  summarise(MediaRevisoesCodigo = mean(RevisoesCodigo))


# Imprimindo o resultado
print(media_revisoes_por_equipe)

cat("\n")
# Calculando a eficiência para cada equipe
estatisticas_por_equipe$Eficiencia <- (estatisticas_por_equipe$MediaCommits * peso1) +
  (estatisticas_por_equipe$MediaRevisoesCodigo * peso2) -
  (1 / estatisticas_por_equipe$MediaTempoDesenvolvimento * peso3)

# Tabela apenas com a eficiência
tabela_eficiencia <- data.frame(
  Equipe = estatisticas_por_equipe$Equipe,
  Eficiencia = estatisticas_por_equipe$Eficiencia
)

# Imprimir a tabela
print(tabela_eficiencia)




cat("- - -\n")
print("3. Visualização de Dados:")

# Visualizando o número de commits por equipe ao longo do tempo
ggplot(dados, aes(x = Mes, y = Commits, group = Equipe, color = Equipe)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Commits por Equipe ao Longo do Tempo",
       x = "Mês",
       y = "Número de Commits") +
  scale_x_continuous(breaks = 1:n_meses) 

# Calculando as médias das pontuações de revisão de código por equipe
media_revisoes_por_equipe <- dados %>%
  group_by(Equipe) %>%
  summarise(MediaRevisoesCodigo = mean(RevisoesCodigo))

# Criando o gráfico de barras
ggplot(media_revisoes_por_equipe, aes(x = Equipe, y = MediaRevisoesCodigo, fill = Equipe)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Média das Pontuações de Revisão de Código por Equipe",
       x = "Equipe",
       y = "Média da Pontuação de Revisão de Código") 


cat("- - -\n")
print("4. Análise Estatística:")

# Carregar pacote necessário para ANOVA
if (!require("stats")) install.packages("stats")
library(stats)



# Realizando o teste ANOVA
resultado_anova <- aov(TempoDesenvolvimento ~ Equipe, data = dados)
summary(resultado_anova)

print(dados$TempoDesenvolvimento)


# Calcular a correlação entre revisões de código e tempo de desenvolvimento
correlacao <- cor(dados$RevisoesCodigo, dados$TempoDesenvolvimento, method = "pearson")

# Imprimir o coeficiente de correlação
cat("Coeficiente de Correlação:", correlacao, "\n")

cat("- - -\n")
print("5. Interpretação e Conclusão:")
