install.packages("DescTools")

data.frame(DadosSaeb)
attach(DadosSaeb)
library(DescTools)
matrix_questions_text = c("Você já foi reprovado?", "Você já abandonou a escola?", "Você gosta de estudar Língua Portuguesa?", "Você gosta de estudar Matemática?", "Você faz o dever de casa de Matemática?")
matrix_questions = cbind(DadosSaeb$`Você já foi reprovado?`, DadosSaeb$`Você já abandonou a escola?`, DadosSaeb$`Você gosta de estudar Língua Portuguesa?`, DadosSaeb$`Você gosta de estudar Matemática?`, DadosSaeb$`Você faz o dever de casa de Matemática?`)

#Funções
table_calculate = function(variable){
  absolute = table(variable)
  relative = round(prop.table(table(variable)) * 100, 2)
  complete_table = cbind(Frequência_Absoluta = absolute, Frequência_Relativa = relative)
  return(complete_table)
}

for (coluna in 1:ncol(matrix_questions)) {
  cat("Tabela para a coluna: ", matrix_questions_text[coluna], "\n")
  tabela = table_calculate(matrix_questions[, coluna])
  print(tabela)
  cat("\n")
}

cat("Tabela Cruzada")

#Tabela cruzada AQUI------------------------------------------------------------
calculate_cross_table = function(var_one, var_two){
  absolute_cross = table(var_one, var_two, dnn = NULL)
  relative_cross = round(prop.table(absolute_cross) * 100, 2)
  complete_table = cbind(Frequência_Absoluta = absolute_cross, Frequência_Relativa = relative_cross)
  print(complete_table)
}
cat("Você já foi reprovado? versus Você já abandonou a escola?")
calculate_cross_table(DadosSaeb$`Você já foi reprovado?`, DadosSaeb$`Você já abandonou a escola?`)
cat("Você faz o dever de casa de Matemática? versus Você gosta de estudar Matemática?")
calculate_cross_table(DadosSaeb$`Você faz o dever de casa de Matemática?`,DadosSaeb$`Você gosta de estudar Matemática?`)

#gráfico de barras--------------------------------------------------------------
cat("Gráfico de barras: Entrada na Escola")
barplot(table(DadosSaeb$`Quando você entrou na escola?`), 
        main = "Entrada na Escola", 
        xlab = "Quando Entrou na Escola", 
        ylab = "Número de Pessoas",
        col = "skyblue",
        border = "blue",
        ylim = c(0, max(table(DadosSaeb$`Quando você entrou na escola?`)) * 1.2))

#Gráfico de setor---------------------------------------------------------------
math_table = table(DadosSaeb$`Você gosta de estudar Matemática?`)
portuguese_table = table(DadosSaeb$`Você gosta de estudar Língua Portuguesa?`)

pie(math_table, 
    main = "Gosta de estudar Matemática?", 
    col = rainbow(length(math_table)),  
    labels = paste(names(math_table), "\n", math_table, sep = ""))  

pie(portuguese_table, 
    main = "Gosta de estudar Língua Portuguesa?", 
    col = rainbow(length(portuguese_table)), 
    labels = paste(names(portuguese_table), "\n", portuguese_table, sep = ""))

#Medidas de posição e separatrizes----------------------------------------------
pt_note = DadosSaeb$`Nota Português`
math_note = DadosSaeb$`Nota Matemática`

cat("Medidas de posição e separatrizes para 'Nota de Português':\n", 
    "Média: ", mean(pt_note),"\n", 
    "Médiana: ", median(pt_note),"\n",
    "Moda: ", Mode(pt_note),"\n", 
    "Quartil: ", quantile(pt_note, probs = c(0.25, 0.5, 0.75)),"\n", 
    "Percentil: ", quantile(pt_note, probs = seq(0.1, 0.9, by = 0.1)), "\n"
    )
cat("Medidas de posição e separatrizes para 'Nota de Matemática':\n", 
    "Média: ", mean(math_note),"\n", 
    "Médiana: ", median(math_note),"\n",
    "Moda: ", Mode(math_note),"\n",
    "Quartil: ", quantile(math_note, probs = c(0.25, 0.5, 0.75)),"\n", 
    "Percentil: ", quantile(math_note, probs = seq(0.1, 0.9, by = 0.1)), "\n"
    )
  
#Medidas de dispersão-----------------------------------------------------------
cat("Medidas de dispersão para 'Nota de Português':\n",
    "Amplitude: ", max(pt_note) - min(pt_note), "\n",
    "Desvio_Padrao: ", sd(pt_note), "\n",
    "Variancia: ", var(pt_note), "\n"
    )
cat("Medidas de dispersão para 'Nota de Matemática':\n",
    "Amplitude: ", max(math_note) - min(math_note), "\n",
    "Desvio_Padrao: ", sd(math_note), "\n",
    "Variancia: ", var(math_note), "\n"
)

#boxplot------------------------------------------------------------------------
boxplot(pt_note, 
        main = "Boxplot - Nota de Português", 
        ylab = "notas",
        col = "skyblue", 
        border = "blue")
boxplot(math_note, 
        main = "Boxplot - Notas de Matemática", 
        ylab = "notas",
        col = "green")

#Histograma---------------------------------------------------------------------
hist(pt_note, 
        main = "Histograma - Nota Português",
        xlab = "Nota",
        col = "skyblue")
hist(math_note, 
        main = "Histograma - Nota Matemática",
        xlab = "Nota",
        col = "lightgreen")
