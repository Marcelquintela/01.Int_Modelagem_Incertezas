# Especializacao em Ciencias de Dados
# Professora: Kelly Cristina M. Goncalves
# Disciplina: Introducao a Modelagem de Incertezas
# Aluno: Marcel Dantas de Quintela
# LISTA DE EXERCICIO 01


# EX4- Simule o R o lancamento de um dado e calcule 1000 repeticoes do experimento
# a frequencia relativa do numero de vezes que a face lancada eh divisivel por 3.
# Dica: no R o comando 6%%3 fornece o resto da divisao de 6 por 3.

N<-1000                                           #numero de lancamentos do dado
cont<-0

Sorteio <- sample(1:6,size=N,replace=TRUE,prob=rep(1/6,6))

for (i in 1:N){
  if(Sorteio[i]%%3==0){cont<-cont+1}
}

table(Sorteio)

table(Sorteio)[3]+table(Sorteio)[6]==cont
cont/N

# Repita o exercicio 4 agora assumindo o lancamento de um dado por duas vezes
# consecutivas e calcule 1000 repeticoes do experimento e a frequencia relativa
# do numero de vezes que a soma das faces eh par.
rm (list = ls ())

N<-1000
cont<-0
?replicate
M<-matrix(c(rep(0,36)), nrow =6, ncol = 6, dimnames #matrix usada para testar a rotina
          = list(c("D1.1","D1.2","D1.3","D1.4","D1.5","D1.6"),
                 c("D2.1","D2.2","D2.3","D2.4","D2.5","D2.6")))

for (i in 1:N){
  Dados <- sample(1:6,size=2,replace=TRUE,prob=rep(1/6,6))
  M[Dados[1],Dados[2]]<-M[Dados[1],Dados[2]]+1 #teste da rotina
  if((Dados[1]+Dados[2])%%2==0){cont<-cont+1}
}

#Teste
M

(Soma<-M[1,1]+M[1,3]+M[1,5]+
       M[2,2]+M[2,4]+M[2,6]+
       M[3,1]+M[3,3]+M[3,5]+
       M[4,2]+M[4,4]+M[4,6]+
       M[5,1]+M[5,3]+M[5,5]+
       M[6,2]+M[6,4]+M[6,6])==cont

cont/N
