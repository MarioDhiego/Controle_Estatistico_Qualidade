
################# Controle Estatistico da Qualidade ###################################### 
################# 7 Ferramentas da Qualidade #############################################
# 1) Fluxograma (ou Estratificação)
# 2) Diagrama de Ishikawa
# 3) Folhas de Verificação (check sheet)
# 4) Diagrama de Pareto
# 5) Histograma
# 6) Disgrama de Dispersão
# 7) Gráficos de Controle
##########################################################################################


################ Instalar Pacotes ########################################################
install.packages(c("tidyverse","dplyr","ggplot2", "qcc", "SixSigna", "qualityTools", "fdth"))
##########################################################################################


################ Carregar Pacotes ########################################################
library(dplyr)
library(ggplot2)
library(qcc)
library(SixSigma)
library(qualityTools)
library(fdth)
library(IQCC)
library(MSQC)
library(spc)
##########################################################################################


############### DIAGRAMA DE ISHIKAWA 1 #######################################################
# AUTOR : Kaoru Ishikawa (1915-1989)
#Pra que serve?
# Identificar, explorar, ressaltar, mapear fatores que julgamos afetar um problema.

# VANTAGENS
# 1) Separa as causas dos efeitos
# 2) Identifica várias causas para o mesmo efeito
# 3) Visualização clara das causas possíveis para um mesmo efeito


### Usando o pacote qcc (“Quality control chart”)
# Argumentos do Pacote
# 1) cause     : uma lista de causas e ramos fornecendo rótulos descritivos. 
# 2) effect    : um rótulo de string ou o efeito.
# 3) title	   : uma string especificando o título principal para aparecer no diagrama  
# 4) cex       : um vetor de valores para a expansão gráfica do personagem. Os valores referem-se, pela ordem, aos RAMOS, CAUSA e EFEITOS.
# 5) font      : um vetor de valores para a fonte, os valores referem-se, pela ordem, aos ramos, causas e efeitos.

###################################################################################################
cause.and.effect(cause = list(Medida   = c("Aferição","Acompanhamento","Indicador"),
                              Material = c("Qualidade","Padronização","Entrega"),
                              Pessoas  = c("Comunicação","Disciplina","Criatividade"),
                              Ambiente = c("Calor","Frio","Espaço"),
                              Metodo   = c("Procedimentos","Etapas","Manuais"),
                              Maquina  = c("Equipamentos","Ferramentas","Instrumentos")),
                 effect = c("Redução/Aumento/Nº de Defeitos"),
                 title  = "Diagrama de Causa e Efeito", 
                 cex    = c(1,1,1), 
                 font  = c(2,2,2)) 
##########################################################################################


############### Diagra de Ishikawa 2 #######################################################
### Usando o pacote SixSigma 
# Argumentos do Pacote
# 1) effect    : Uma sequência de caracteres curta que representa o efeito que desejamos analisar.
# 2) causes.gr : Um vetor de caracteres que representa os grupos de causas.
# 3) causes	   : Um vetor com listas que representam as causas individuais para cada
# 4) main	     : Título principal do diagrama
# 5) sub	     : Subtítulo para o diagrama (recomendado o nome do projeto Six Sigma)
# 6) ss.col	   : Um vetor de cores para um desenho personalizado. Pelo menos cinco cores, classificadas por intensidade descendente

###########################################################################################
Efeito <- "ALTA DENSIDADE"

Causas_Primárias <- c("MÉTODOS", "MÁQUINAS", "MÃO DE OBRA", 
                      "MATERIAL", "MEDIDAS", "MEIO AMBIENTE")

Causas_Secundárias <- vector(mode = "list", length = length(Causas_Primárias))

Causas_Secundárias[1] <- list(c("Recpção de Materias-Primas", "Metodo de Transporte"))
Causas_Secundárias[2] <- list(c("Compressor", "Condições Operacionais", "Regulagem da Maquina"))
Causas_Secundárias[3] <- list(c("Recpcionista", "Operador do Processo", "Operador de Armazenamento"))
Causas_Secundárias[4] <- list(c("Fornecedor", "Transportadora", "Embalagem"))
Causas_Secundárias[5] <- list(c("Metodo de Medicao", "Instrumento Descalibrado"))
Causas_Secundárias[6] <- list(c("Calor", "Ventilação", "Iluminação"))

Ishikawa_Densidade <- ss.ceDiag(Efeito, Causas_Primárias, Causas_Secundárias,
                                main = "Diagrama de Ishikawa", sub = "DENSIDADE",
                                ss.col= c("#666666","#BBBBBB","#CCCCCC","#DDDDDD","#EEEEEE"))
##########################################################################################



############### Diagrama de Pareto #######################################################
#
# Vilfredo Pareto (1848-1923), engenheiro e economista italiano


# Ex: Observação da Linha de Produção de Tecidos

# 1) Tipos de Não Conformidades
#   i)   Riscos
#   ii)  Manchas
#   iii) Dobras
#   iv)  Furos
#   vi)  Rasgos

# 2) Número de Não Conformidades
# 3) Custo de Retrabalho p/ Unidades
# 4) Custo Total de Retrabalho

############## Criando Tabela NC e Custo de Retrabalho ###############################
nc <- c(201, 78, 47, 31, 15)
names(nc) <- c("Riscos", "Manchas", "Dobras", "Furos", "Rasgos")

pareto.chart(nc, 
             ylab= "Frequência de Defeitos",
             main = "Diagrama de Pareto",
             cumperc= seq(0,100, by=10))
#######################################################################################

############### Criando Tabela Custo Retrabalho #######################################
retrabalho <- c(4287, 2423, 1119, 8947, 1864)
names(retrabalho) <- c("Riscos", "Manchas", "Dobras", "Furos", "Rasgos")
pareto.chart(retrabalho,
             main= "Diagrama de Pareto P/Custo Retrabalho",
             ylab = "Frequência do Defeito",
             xlab = "Tipo de Defeito",
             cumperc = seq(0,100, by=10),
             last=1)
###################################################################################################


################ Folha de Verificação ############################################################

# Pra que Serve?
# 1) Tabela para coletar e resumir informações
# 2) Contagem de ocorrência de eventos: ocorrências, consultas, falhas,defeitos, não-conformidades, etc
# 3) Fácil utilização, visualização e interpretação
# 4) Coleta de dados de forma padronizada
# 5) É um Registro da Qualidade, portanto deve ser devidamente identificada, datada, e assinada por responsável


# Passos para Aplicação
# 1) Escolher um tipo de folha de verificação que permite um fácil preenchimento, organizar conforme a necessidade da organização ou setor, no qual será feita a coleta de dados.
# 2) Definir a quantidade de informações irá conter no problema/falhas ou ocorrência e o tamanho da amostra dos dados
# 3) Decidir o local ou setor onde será a coleta de dados
# 4) Determinar a frequência com que serão coletados os dados, pode ser diário, semanal ou mensal. Serve para verificar a ocorrência durante um determinado período
# 5) Definir quem deverá coletar os dados, poderá ser o gestor do setor que está sendo coletado os dados ou supervisor direto do setor operacional;
# 6) Aplicar a folha de verificação atingir a coleta dentro do planejado.


# Fazer uma Folha de Verificação
folha_verificação <- rbind(data.frame(Group = "MÉTODOS", cause = Métodos),
                           data.frame(Group = "MÁQUINAS", cause = Máquinas),
                           data.frame(Group = "MÃO DE OBRA", cause = Mãodeobra),
                           data.frame(Group = "MATERIAL", cause = Material),
                           data.frame(Group = "MEDIDAS", cause = Medidas),
                           data.frame(Group = "MEIO AMBIENTE", cause = Meioambiente))

folha_verificação$Inspetor_A <- NA 
folha_verificação$Inspetor_B <- NA
folha_verificação$Inspetor_C <- NA

folha_verificação


# Investigação p/ Inpetores
folha_verificação$Inspetor_A <- c(0, 0, 4, 3, 2, 0, 2, 0,
                                  2, 0, 0, 1, 1, 0, 0, 1)

folha_verificação$Inspetor_B <- c(0, 0, 5, 3, 2, 0, 2, 0,
                                  2, 0, 0, 1, 1, 0, 0, 1)

folha_verificação$Inspetor_C <- c(0, 0, 6, 4, 3, 0, 2, 0,
                                  2, 0, 0, 1, 1, 0, 0,1)

folha_verificação$total <- folha_verificação$Inspetor_A + 
        folha_verificação$Inspetor_B + 
        folha_verificação$Inspetor_C

folha_verificação
#################################################################################################


#################### Histograma ########################################################
# Pacote fdth
# Frequency distribution tables, histograms and polygons

# Histograma simples
hist(densidade_pastilhas)

# Calcular a Densidade
df_den_p <- fdt(densidade_pastilhas)
df_den_p 

# Histogram de Frequencia
plot(df_den_p, type = "fh")


#Histograma freqência relativa
plot(df_den_p, type = "rfh")


#Histograma freqência relativa em %
plot(df_den_p, type = "rfph")


#Histograma freqência Acumulada
plot(df_den_p, type = "cfh")


#Histograma freqência Acumulada em %
plot(df_den_p, type = "cfph")
########################################################################################






#################### Parâmetros do Grafico de Controle #############################################
# 1) Limites de Controle
# i)   LIC = Limite Inferior de Controle
# ii)  LIC = Limite Superior de Controle
# iii) μ   = Média da Amostra
# iv)  σ   = Desvio Padrão.
# vi)  MR  = é a média das amplitudes
# vii) d2  = é uma constante tabelada 
#####################################################################################################

#################### Tipos de Gráficos de Controle ####################################################

# 1) Grafico de Controle da Média
# 2) Grafico de Controle da Amplitude
# 3) Grafico de Controle da Desvio-Padrão
# 4) Grafico de Controle da Media Movel
# 5) Grafico de Controle da Cussum
# 6) Grafico de Controle da Ewma
#####################################################################################################


#################### Regra de Shewhart #########################################################
# Regra 1: Um único ponto fora dos limites de controle
# Regra 2: Oito ou mais pontos seguidos acima (ou abaixo) da linha central
# Regra 3: Seis pontos consecutivos crescendo (tendência para cima) ou descendo (tendência para baixo)
# Regra 4: Dois de três pontos consecutivos próximos (um terço externo) de um limite de controle
# Regra 5: Quinze pontos consecutivos próximos (um terço interno) da linha central

################################################################################################



#################### APLICAÇÃO PRÁTICA ###############################################################

# Densidade de Pastilas Cerâmicas
# Um certo processo industrial produz pastilhas cerâmicas cuja densidade é uma característica 
# crítica de qualidade de acordo com as necessidades do cliente


# As especificações técnicas atuais afirmam que a densidade de uma pastilha é considerada aceitável
# SE for superior a 10,5 g/cm³. 


# O que se poderia dizer sobre a qualidade do produto e o controle de processos?
# Leitura da Base de Dados
densidade_pastilhas <- c(10.6817, 10.6040, 10.5709, 10.7858, 10.7668, 10.8101, 
                         10.6905, 10.6079, 10.5724, 10.7736, 11.0921, 11.1023, 
                         11.0934, 10.8530, 10.6774, 10.6712, 10.6935, 10.5669, 
                         10.8002, 10.7607, 10.5470, 10.5555, 10.5705, 10.7723)

# Encontrar a Média dos valores 
media <- mean(densidade_pastilhas)

# Econtrar as Amplitudes Móveis dos valores
diff(densidade_pastilhas)

# Alguns dos valores de amplitude móvel são negativos
# Calcular em Modulo
amplitude_Modulo <- abs(diff(densidade_pastilhas))


# Media da Amplitude Movel (MR)
MR <- mean(abs(diff(densidade_pastilhas)))

# Constante d
d2 <- 1.1284


###Cálculo dos limites de controle (LC)
LIC <- media - 3*(MR/d2)
LSC <- media + 3*(MR/d2)


############# Gráfico de Controle da Média Amostral #############################################################
plot_xbar <- qcc(data = densidade_pastilhas, type="xbar", plot = T)

############# Gráfico de Controle da Amplitude(R) ######################################################
plot.xbar=qcc(densidade_pastilhas, type="R", plot = T)

############ Gráfico de Medidas Individuais #####################################################################
plot.ind = qcc(data2, type="xbar.one")

############# Gráfico de Controle do Desvio Padrão #####################################################
plot.xbar=qcc(densidade_pastilhas, type="S")


############# Gráfico de Controle de Proporção ###########################
plot.xbar=qcc(densidade_pastilhas, type="p")







############# Indice de Capacidade #######################################
process.capability(object = plot.xbar,
                   spec.limits=c(6,9))




############ Atualizar o Gráfico de Controle #################################

plot.atual.1 = qcc(densidade_pastilhas[1:20,], type = "xbar", newdata = data[21:24,])

plot(plot.atual.2, chart.all=FALSE)

#############################################################################################









################### Grafico de Controle Densidade ######################################################
# gráfico de controle para a densidade das pastilhas cerâmicas.

plot(densidade_pastilhas,
     type = "b",
     pch = 16,
     ylim = c(10.400,11.100),
     axes = FALSE,
     main = "Gráfico de controle para Densidade",
     sub = "Densidade Pastilhas Cerâmicas",
     xlab = "Amostras",
     ylab = "Densidade")
axis(1,
     at = 1:24)
axis(2)
box()
grid()
abline(h = 10.7342,
       lwd = 2)
abline(h = 10.4512, lwd = 2, col = "red")
abline(h = 11.0171, lwd = 2, col = "red")
###########################################################################################################















################### Referencias Bibliograficas ##############################################################################

# 1) Mason, R.L. and Young, J.C. (2002) Multivariate Statistical Process Control with Industrial Applications, SIAM. 

# 2) Montgomery, D.C. (2005) Introduction to Statistical Quality Control, 5th ed. New York: John Wiley & Sons. 

# 3) Ryan, T. P. (2000), Statistical Methods for Quality Improvement, 2nd ed. New York: John Wiley & Sons, Inc. 

# 4) Scrucca, L. (2004). qcc: an R package for quality control charting and statistical process control. R News 4/1, 11-17. 

# 5) Wetherill, G.B. and Brown, D.W. (1991) Statistical Process Control. New York: Chapman & Hall.

# 6) Allen, T. T. (2010) Introduction to Engineering Statistics and Lean Six Sigma - Statistical Quality Control and Design of Experiments and Systems (Second Edition ed.). London: Springer.

# 7) Box, G. (1991). Teaching engineers experimental design with a paper helicopter. Report 76, Center for Quality and Productivity Improvement. University of Wisconsin.

# 8) Cano, Emilio L., Moguerza, Javier M. and Redchuk, Andrés. 2012. Six Sigma with R. Statistical Engineering for Process Improvement, Use R!, vol. 36. Springer, New York. http://www.springer.com/statistics/book/978-1-4614-3651-5.

# 9) Cano, Emilio L., Moguerza, Javier M. and Prieto Corcoba, Andrés. 2015. Quality Control with R. An ISO Standards approach, Use R!, Springer, New York.


#############################################################################################################################












