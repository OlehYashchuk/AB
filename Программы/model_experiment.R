rm(list=ls())
m = 200
df<-1:20
for (i in 1:150){
	df<-cbind(df, rnorm(20,mean=0,sd=1))
}
for (i in 1:50){
	df<-cbind(df, rnorm(20,mean=1,sd=1))
}

write.table(df, "bonferroni.txt", sep="\t", quote=FALSE, 
            col.names=FALSE, row.names=FALSE) 
df<-as.data.frame(df)

# без поправки
V<-0
U<-0
p_h0<-0
for (i in 2:151){
	t_test<-t.test(df[i], alternative="two.sided", mu=0)
	if (t_test$p.value<0.05) {V<-V+1} else {U<-U+1}
	p_h0[i-1]<-t_test$p.value
}

T<-0
S<-0
p_h1<-0
for (i in 152:201){
	t_test<-t.test(df[i], alternative="two.sided", mu=0)
	if (t_test$p.value<0.05) {S<-S+1} else {T<-T+1}
	p_h1[i-151]<-t_test$p.value
}

m0<-U+V
R<-V+S

without<-matrix(c(U, V, m0, T, S, m-m0, m-R, R, m), nrow=3, ncol=3, byrow=FALSE)
dimnames(without)<-list(c("# принятых Hi", "# отвергнутых Hi", "Всего"), 
                        c("# верных Hi", "# неверных Hi", "Всего"))  
print("Без поправок:")
print(without)

# график
df1<-as.data.frame(rbind(cbind(p_h0, 0),cbind(p_h1, 1)))
colnames(df1)<-c("pvalue", "Hi")
df1$Hi<-factor(df1$Hi)
df1$color[df1$Hi==0]<-"blue"
df1$color[df1$Hi==1]<-"red"
df1<-df1[order(df1$pvalue),]

plot(1:200, df1$pvalue, col = df1$color, pch=16, cex=1,
	xlab = 'Номер в вариационном ряду',
	ylab = 'Достигаемый уровень значимости',
	main = 'Без поправок')
abline(h=0.05, col='blue')
legend("topleft", c("# справедливая Hi", "# несправедливая Hi"), pch=c(16,16), 
       col=c("blue", "red"), bty='n') 
