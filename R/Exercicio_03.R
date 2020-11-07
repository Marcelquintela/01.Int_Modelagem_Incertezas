# Especializacao em Ciencias de Dados
# Professora: Kelly Cristina M. Goncalves
# Disciplina: Introducao a Modelagem de Incertezas
# Aluno: Marcel Dantas de Quintela
# LISTA DE EXERCICIO 03

# Q4 -Um supermercado faz a seguinte promocao: o cliente, ao passar pelo caixa, lanca
# um dado honesto. Se sair face '6' ele ganha um desconto de 30% sobre o total de
# sua compra. Se sair face '5', o desconto eh de 20%. Se sair face '4', o desconto eh
# de 10% e, se sair faces '1', '2' ou '3', o desconto eh de 5%. Pede-se:

# (b)a probabilidade de que num grupo de 5 clientes, pelo menos 1 obtenha um
# desconto maior do que 10%. Use um modelo probabilistico e faca os calculos
# das probabilidades no R usando funcoes do modelo.

#P(X>=1) X~Binom(5,1/3)
1-dbinom(0,5,1/3)
#ou
sum(dbinom(c(1,2,3,4,5),5,1/3))


# Q5.Seja uma prova com 10 questoes. Suponha que cada questao tenha 4 alternativas
# de respostas, das quais somente uma eh a correta. Use um modelo probabilistico e
# faca os calculos das probabilidades no R usando funcoes do modelo.

# (a)Determine a probabilidade de um aluno acertar pelo menos cinco questoes de
# forma puramente aleatoria, isto eh, 'chutando'.

#P(X>=5) X~Binom(10,0.25)
1-pbinom(4,10,0.25)
#ou
sum(dbinom(c(5:10),10,0.25))

# (b)No maximo quantas questoes o aluno acerta 'chutando' com probabilidade
# 90%?

#P(X>=xi)=0.90
qbinom(0.9,10,0.25)

# Grafico que ajuda a visualizar #P(X>=xi)=0.90
plot(c(0:10),pbinom(c(0:10),10,0.25), type="h",cex=1,bty="n",pch=19,
     yaxt="n",xaxs="i",xlab="x", ylab="F(x)",main="X ~ Binomial(10,0.25)")
axis(2, at=seq(0, 1, by=0.1))

points(x=4,y=0.9,col="red",type="p", cex=.7, pch=19)
abline(v=4,lty=3,col="red")
abline(h=0.9,lty=3,col="red")
legend(x=3.7, y=1, legend="P(X>=x)=0,90", bty="n", cex=.6, text.col="red")


# Q6.Uma pesquisa eleitoral indicou que 40% dos eleitores votariam num candidato A.
# Com base nessa informacao numa amostra aleatoria (suponha sorteios independentes)
# de 10 adultos, qual a probabilidade de que pelo menos 5 eleitores sejam
# favoraveis ao candidato A? Use um modelo probabilistico e faca os calculos das
# probabilidades no R usando funcoes do modelo.

#P(X>=5) X~Binom(10,0.4)
1-pbinom(4,10,0.4)
#ou
sum(dbinom(c(5:10),10,0.4))

# Grafico FP de X
plot(c(0:10),dbinom(c(0:10),10,0.4), type="h",cex=1,bty="n", pch=19,
     xlab="x", ylab="p(x)",main="X ~ Binomial(10,0.4)")


# 7.Um certo sistema eletronico contem 10 componentes. Suponha que a probabilidade
# de falha de qualquer componente individual seja de 0.2 e que eles falhem independentemente
# uns dos outros. Dado que pelo menos um dos componentes falhou, qual
# a probabilidade de que pelo menos dois falharam?  Use um modelo probabilistico e faca os calculos das
# probabilidades no R usando funcoes do modelo.

#P(X>=2|X>=1) X~Binom(10,0.2)
a<-pbinom(1,10,0.2,lower.tail = FALSE) #P(X>=2) --> 1-pbinom(1,10,0.2)
b<-pbinom(0,10,0.2,lower.tail = FALSE) #P(X>=1) --> 1-pbinom(0,10,0.2)
a/b

#ou
a<-sum(dbinom(2:10,10,.2)) #P(X>=2)
b<-sum(dbinom(1:10,10,.2)) #P(X>=1)
a/b

# Grafico FP de X
plot(c(0:10),dbinom(c(0:10),10,0.2), type="h",cex=1,bty="n", pch=19,
     xlab="x", ylab="p(x)",main="X ~ Binomial(10,0.2)")
