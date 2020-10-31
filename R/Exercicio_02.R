# Especializacao em Ciencias de Dados
# Professora: Kelly Cristina M. Goncalves
# Disciplina: Introducao a Modelagem de Incertezas
# Aluno: Marcel Dantas de Quintela
# LISTA DE EXERCICIO 02

#Q1 - Um aluno responde a uma questao de multipla escolha com 4 alternativas, com
#uma so correta. A probabilidade de que ele saiba a resposta certa da questao
#e de 30%. Se ele nao sabe a resposta, existe a possibilidade de ele acertar no
#chute". N~ao existe a possibilidade de ele obter a resposta certa por cola".
# Qual e a probabilidade de ele acertar a questao?

#Faca o item 1. agora no R por meio de simulacoes do experimento aleatorio em
#questao. Dica: ha dois eventos em quest~ao, o fato do aluno saber ou nao a
# questao e ele acertar ou n~ao, dado que sabe ou n~ao.

A<-c("Acertar","Não Acertar")
S<-c("Saber","Não Saber")


N = 1e+05           # número de repeticoes
AC = 0

for(i in 1:N){
  S<-sample(c("Saber","Não Saber"),size=1,replace=TRUE, prob=c(0.3,0.7))
  if (S=="Saber"){  # como não existe "cola" P(A="Acertar"|S="Saber")=1
    AC<-AC+1
  } else {          # se (S=="Não Saber") -> verifica se aluno "Acertou"
                    # com P(A="Acertpou"|S="Não Saber")=0.25
    A<-sample(c("Acertar","Não Acertar"),size=1,replace=TRUE, prob=c(0.25,0.75))
    if (A=="Acertar") {
      AC<-AC+1
    }
  }
}

cat("A probalidade de uma aluno acertar a questão é de: ", round(AC/N,3))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Q5- Uma caixa contem tres moedas. A moeda 1  eh honesta, a moeda 2 tem duas
# caras e a moeda 3 eh viciada de tal modo que cara eh duas vezes mais provavel
# que coroa. Uma moeda eh escolhida ao acaso e lancada.

rm (list = ls ())
N = 1e+04           # numero de repeticoes

# Função que retorna 1 se cara e 0 C.C.,
# dado a probabilidade de ocorrencia de cara
Face_Cara=function(p){
  x<-sample(c("Cara","Coroa"),size = 1, prob = c(p,1-p))
  if (x=="Cara"){return(1)}else{return(0)}
}

M<-sample(c("Moeda_1","Moeda_2","Moeda_3"),size = N, replace=TRUE)

Result<-matrix(rep(0,6),2,3,
               dimnames = list(c("Cara","Coroa"),
                               c("Moeda_1","Moeda_2","Moeda_3")))
for (i in 1:N){
  if (M[i]=="Moeda_1"){
    #C<-C+Face_Cara(0.5)
    Result[1,1]<-Result[1,1]+Face_Cara(0.5)
    Result[2,1]<-Result[2,1]+(1-Face_Cara(0.5))
    #cat(M,C,"\n")
  } else if(M[i]=="Moeda_2"){
    #C<-C+Face_Cara(1)
    Result[1,2]<-Result[1,2]+Face_Cara(1)
    Result[2,2]<-Result[2,2]+(1-Face_Cara(1))
    #cat(M,C,"\n")
  } else{ # Moeda_3
    #C<-C+Face_Cara(2/3)
    Result[1,3]<-Result[1,3]+Face_Cara(2/3)
    Result[2,3]<-Result[2,3]+(1-Face_Cara(2/3))
    #cat(M,C,"\n")
  }
}
Result/N #apply(Result,1:2,function(x) x/N)   # P(Face ou Moeda)
apply(Result,MARGIN = 2,function(x) x/sum(x)) # P(Face | Moeda)
apply(Result,MARGIN = 1,function(x) x/sum(x)) # P(Moeda | Face)
apply(Result,1,sum)/N                         # Marginais nas Linhas (Faces)
apply(Result,2,sum)/N                         # Marginais nas colunas (Moedas=1/3)

# a) P(Cara ou Moeda_1)
Result[1,1]/N

# b) P(Cara)
apply(Result,1,sum)[[1]]/N

# c) P(Moeda_1|Cara)
apply(Result,MARGIN = 1,function(x) x/sum(x))[1,1]
