# Especializacao em Ciencias de Dados
# Professora: Kelly Cristina M. Goncalves
# Disciplina: Introducao a Modelagem de Incertezas
# Aluno: Marcel Dantas de Quintela
# LISTA DE EXERCICIO 04

# Q1-Uma urna contem 6 bolas brancas e 10 bolas vermelhas. Bolas sao retiradas ao
# acaso, com reposicao. Nos itens a seguir, use um modelo probabilstico e faca os
# calculos das probabilidades no R usando funcoes do modelo.
#------------------------------------------------------------------------------
# RESPOSTA
# seja X v.a. onde x~Geo(6/16)
#------------------------------------------------------------------------------
# (a)Calcule a probabilidade de que no maximo 3 bolas (incluindo a ultima branca)
# precisem ser retiradas ate que a primeira bola branca seja encontrada.

#P(X<=3)= P(x=1)+P(x=2)+(Px=3)
(dgeom(0,0.375)+dgeom(1,0.375)+dgeom(2,0.375))
#P(X<=3)=1-P(x>3)
1-(1-0.375)^3
# acumulado de 2 fracasos até o 1o sucesso
pgeom(2,0.375,lower.tail=TRUE)
1-(0.625^3)

# (b)Calcule o valor esperado e desvio padrao do numero de bolas extradas
# (incluindo a ultima branca) ate que a primeira bola branca seja retirada.

#E(x)
1/0.375
#Var(X)
var(rgeom(n =10000, prob = 0.375))
(1-.375)/0.375^2


# q2 - Um area rural tem 20 alces. Hoje, 5 alces sao capturados, etiquetados e soltos de
# volta nesta regiao. Em uma data posterior, 8 alces sao recapturados aleatoriamente.
# Calcule a probabilidade de no maximo 2 alces dentre os 8 recapturados estarem
# etiquetados? Use um modelo probabilistico e faca os calculos das probabilidades
# no R usando comandos associados ao modelo.

#------------------------------------------------------------------------------
# RESPOSTA
# Seja X número de alces etiquetados em 8 capturados, numa área de onde existem
# 20 alces e desres 5 foram etiquetados.
# X v.a. onde x~Binomial(8,0.25)
# P(X<=2)=?
#------------------------------------------------------------------------------
pbinom(2,8,0.25) # P(X<=2)
1-pbinom(2,8,0.25,lower.tail = FALSE) #1-P(x>2)

# Q3- As cinco primeiras repeticoes de um experimento custam R$10,00 cada. Todas
# as repeticoes subsequentes custam R$5,00 cada. Suponha que o experimento seja
# repetido ate que o primeiro sucesso ocorra. Suponha que a probabilidade de sucesso
# em uma repeticao seja igual a 0.46 e que as repeticoes sao independentes. Nos itens
# a seguir, use um modelo probabilistico e faca os calculos das probabilidades no R
# usando funcoes do modelo.
#------------------------------------------------------------------------------
# RESPOSTA
# seja X v.a. onde x~Geo(0.46)
#------------------------------------------------------------------------------

# (a)Calcule a probabilidade de que 5 repeticoes (incluindo o sucesso) sejam necessarias
# ate que o primeiro sucesso ocorra?

#P(X<=5)= P(x=1)+P(x=2)+P(x=3)+P(x=4)+P(x=5)
(dgeom(0,0.46)+dgeom(1,0.46)+dgeom(2,0.46)+dgeom(3,0.46)+dgeom(4,0.46))
#P(X<=5)=1-P(x>5)
1-(1-0.46)^5
# acumulado de 4 fracasos até o 1o sucesso
pgeom(4,0.46,lower.tail=TRUE)


# (b)Qual e o custo esperado da operacao?
Set.seed(1)
n<-10000
g<-rgeom(n,prob=0.46)+1 # numero de ensaios inluindo o sucesso
cx<-c(rep(0,n))

for (i in 1:n){
  if (g[i]<=5){
    cx[i]<-g[i]*10
    }else{
      cx[i]<-g[i]*5
    }
}
mean(cx)

# 4.Acidentes ocorrem numa plataforma de petroleo segundo um modelo de Poisson a
# uma taxa media de 1,5 por mes. Pergunta-se (faca os calculos das probabilidades
# no R usando funcoes do modelo Poisson):
#   (a)Qual a probabilidade de nenhum acidente em marco?
#P(X=0) -  X~Poisson(1,5)
dpois(0,1.5)
#   (b)Qual a probabilidade de ocorrer 5 acidentes no perodo de junho a agosto?
#P(X=5) -  X~Poisson(1,5*3)
dpois(5,3*1.5)


# 5.As chegadas de petroleiros a uma refinaria em cada dia ocorrem segundo uma
# distribuicao de Poisson, com media 2. As atuais instalacoes podem atender, no
# maximo, a 3 petroleiros por dia. Se mais de 3 petroleiros chegarem num dia, o
# excesso e enviado a outro porto. Por outro lado, se mais de 18 petroleiros chegam
# numa semana, o porto deve ficar fechado por 3 dias. (Faca os calculos das probabilidades
# no R usando funcoes do modelo Poisson).
#   (a)Em um dia, qual a probabilidade de se enviar petroleiros para outro porto?
#P(X>=3)=1-P(X<=2) -  X~Poisson(2)
1-ppois(2,2)
1-(dpois(0,2)+dpois(1,2)+dpois(2,2))

#   (b)Apos uma semana, qual a probabilidade do porto ser obrigado a parar as suas
# atividades por 3 dias?
#P(X>=18)=1-P(X<=17) -  X~Poisson(2*7)
1-ppois(17,2*7)

# Q6- m homem lanca um dardo ao alvo repetidas vezes e so para quando acerta uma
# regi~ao em torno do centro 3 vezes, a qual tem chance de 40% de ser atingida. Use
# um modelo probabilstico e faca os calculos das probabilidades no R usando funcoes
# do modelo.
# (a)Qual a probabilidade dele atingir sua meta na oitava tentativa?

dnbinom(x=7,size = 3,prob = 0.4)
# (b)Qual o numero esperado de numero de tentativas ate ele parar de jogar.

3/0.4


# Q7 - um torneio informal de jogo de dardos o atual campe~ao destaca-se entre os outros
# por ter a habilidade de lancar com boa precis~ao o dardo com ambas as m~aos. Se o
# mesmo joga com a m~ao direita a chance de acertar o alvo e de 0.90, enquanto com
# a m~ao esquerda a chance reduz um pouco para 0.78. O atual campe~ao foi ent~ao
# desaado a lancar o dardos com uma das m~aos ate alcancar o quarto sucesso. Sem
# saber da diferenca de performance do jogador com ambas as m~aos, uma pessoa do
# publico foi escolhida para selecionar aleatoriamente com qual das m~aos o campe~ao
# teria de enfrentar o desao. Sabendo que o jogador precisou lancar os dardos no
# maximo 6 vezes ate alcancar o objetivo, qual a probabilidade do espectador ter
# selecionado a m~ao esquerda? Para calcular as probabilidades associadas ao modelo
# probabilstico, use o R.

# Q8.Um ^onibus passa no ponto de ^onibus dentro de uma universidade em qualquer
# instante (em min) no intervalo (0,30) uniformemente. (Para calcular as probabilidades
# associadas ao modelo probabilstico, use o R.)
#   (a)Calcule a probabilidade do ^onibus passar nos primeiros 12 minutos?

punif(12,0,30)

#   (b)Calcule a probabilidade do ^onibus levar mais de 20 minutos nesse intervalo?
1-punif(20,0,30)

# 9.Assuma que o tempo de durac~ao X de uma consulta medica tenha distribuic~ao
# exponencial com media de 15 minutos. (Para calcular as probabilidades associadas
#                                        ao modelo probabilstico, use o R.)
# (a)Calcule a probabilidade uma consulta demorar 30 minutos no maximo;
1-pexp(30,rate=1/15,lower.tail = FALSE)
# (b)Qual o tempo mnimo de durac~ao dessa consulta com probabilidade de 80%?
qexp(0.8,rate=1/15)
