CCTV <- function() {


library(igraph)	
print("Please Input a Valid Database")
print("Author Marianna Milano - Bioinformatics Lab - Unicz")
print("Select type of data:hs, ic, th, hi, tcp, ncp, dh, d, tc, sw")	
	
#x=read.table('~/Desktop/FUNZIONI R/dati_covid/analyzeC19D-master/all2.txt', header = T, sep = "\t")


x=read.table('~/Google Drive (m.milano@unicz.it)/2022-Biotech/data.txt', header = T, sep = "\t")


resp=scan(,what="")

if(resp=="hs") { 



aa=x$hs
name="Hospitalised with Symptoms"


}

else if(resp=="ic")  {
	aa=x$ic
	name="Intensive Care"
}

else if(resp=="th")  {
	aa=x$th
	name="Total Hospitalised"
}
else if(resp=="hi")  {
	aa=x$hi
	name="Home Isolation"
}
else if(resp=="tcp")  {
	aa=x$tcp
	name="Total Currently Positive"
}
else if(resp=="ncp")  {
	aa=x$ncp
	name="New Currently Positive"
}
else if(resp=="dh")  {
	aa=x$dh
	name="Discharged|Healed"
}
else if(resp=="d")  {
	aa=x$d
	name="Deceased"
}
else if(resp=="tc")  {
	aa=x$tc
	name="Total Cases"
}
else if(resp=="sw")  {
	aa=x$sw
	name="Swabs"
}

aa=as.numeric(aa)
#numero di giorni da analizzare minimo del massimo dei giorno 677

print("Select period (fist day) ")	

initial=scan(,what=)
print("Select period (last day) ")	
numg=scan(,what=)


abr=which(x=="Abruzzo")
bas=which(x=="Basilicata")
bol=which(x=="Bolzano")
cal=which(x=="Calabria")
cam=which(x=="Campania")
em=which(x=="Emilia")
fr=which(x=="Friuli")
laz=which(x=="Lazio")
lig=which(x=="Liguria")
lom=which(x=="Lombardia")
mar=which(x=="Marche")
mol=which(x=="Molise")
pie=which(x=="Piemonte")
pug=which(x=="Puglia")
sar=which(x=="Sardegna")
sic=which(x=="Sicilia")
tos=which(x=="Toscana")
tre=which(x=="Trento")
umb=which(x=="Umbria")
vall=which(x=="ValleAosta")
ven=which(x=="Veneto")


w1=wilcox.test(aa[abr[initial]:abr[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[abr[initial]:abr[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[abr[initial]:abr[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[abr[initial]:abr[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[abr[initial]:abr[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[abr[initial]:abr[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[abr[initial]:abr[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[abr[initial]:abr[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[abr[initial]:abr[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[abr[initial]:abr[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[abr[initial]:abr[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[abr[initial]:abr[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[abr[initial]:abr[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[abr[initial]:abr[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[abr[initial]:abr[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[abr[initial]:abr[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[abr[initial]:abr[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[abr[initial]:abr[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[abr[initial]:abr[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[abr[initial]:abr[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[abr[initial]:abr[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value



abr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

abr1=as.vector(abr1)



for (i in 1:length(abr1)) {
if(abr1[i]<0.05){ abr1[i]=0; i = i+1}}

Abruzzo =round(abr1, digits = 2)











w1=wilcox.test(aa[bas[initial]:bas[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[bas[initial]:bas[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[bas[initial]:bas[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[bas[initial]:bas[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[bas[initial]:bas[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[bas[initial]:bas[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[bas[initial]:bas[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[bas[initial]:bas[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[bas[initial]:bas[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[bas[initial]:bas[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[bas[initial]:bas[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[bas[initial]:bas[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[bas[initial]:bas[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[bas[initial]:bas[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[bas[initial]:bas[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[bas[initial]:bas[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[bas[initial]:bas[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[bas[initial]:bas[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[bas[initial]:bas[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[bas[initial]:bas[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[bas[initial]:bas[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value






bas1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

bas1=as.vector(bas1)



for (i in 1:length(bas1)) {
if(bas1[i]<0.05){ bas1[i]=0; i = i+1}}

Basilicata =round(bas1, digits = 2)











w1=wilcox.test(aa[bol[initial]:bol[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[bol[initial]:bol[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[bol[initial]:bol[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[bol[initial]:bol[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[bol[initial]:bol[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[bol[initial]:bol[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[bol[initial]:bol[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[bol[initial]:bol[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[bol[initial]:bol[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[bol[initial]:bol[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[bol[initial]:bol[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[bol[initial]:bol[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[bol[initial]:bol[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[bol[initial]:bol[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[bol[initial]:bol[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[bol[initial]:bol[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[bol[initial]:bol[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[bol[initial]:bol[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[bol[initial]:bol[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[bol[initial]:bol[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[bol[initial]:bol[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value




bolz1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

bolz1=as.vector(bolz1)



 for (i in 1:length(bolz1)) {
 if(bolz1[i]<0.05){ bolz1[i]=0; i = i+1}}

Bolzano =round(bolz1, digits = 2)














w1=wilcox.test(aa[cal[initial]:cal[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[cal[initial]:cal[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[cal[initial]:cal[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[cal[initial]:cal[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[cal[initial]:cal[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[cal[initial]:cal[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[cal[initial]:cal[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[cal[initial]:cal[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[cal[initial]:cal[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[cal[initial]:cal[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[cal[initial]:cal[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[cal[initial]:cal[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[cal[initial]:cal[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[cal[initial]:cal[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[cal[initial]:cal[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[cal[initial]:cal[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[cal[initial]:cal[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[cal[initial]:cal[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[cal[initial]:cal[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[cal[initial]:cal[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[cal[initial]:cal[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value




calabr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

calabr1=as.vector(calabr1)



for (i in 1:length(calabr1)) {
if(calabr1[i]<0.05){ calabr1[i]=0; i = i+1}}

Calabria =round(calabr1, digits = 2)













w1=wilcox.test(aa[cam[initial]:cam[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[cam[initial]:cam[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[cam[initial]:cam[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[cam[initial]:cam[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[cam[initial]:cam[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[cam[initial]:cam[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[cam[initial]:cam[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[cam[initial]:cam[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[cam[initial]:cam[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[cam[initial]:cam[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[cam[initial]:cam[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[cam[initial]:cam[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[cam[initial]:cam[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[cam[initial]:cam[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[cam[initial]:cam[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[cam[initial]:cam[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[cam[initial]:cam[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[cam[initial]:cam[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[cam[initial]:cam[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[cam[initial]:cam[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[cam[initial]:cam[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value





camp1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

camp1=as.vector(camp1)



for (i in 1:length(camp1)) {
if(camp1[i]<0.05){ camp1[i]=0; i = i+1}}

Campania =round(camp1, digits = 2)









w1=wilcox.test(aa[em[initial]:em[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[em[initial]:em[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[em[initial]:em[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[em[initial]:em[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[em[initial]:em[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[em[initial]:em[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[em[initial]:em[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[em[initial]:em[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[em[initial]:em[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[em[initial]:em[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[em[initial]:em[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[em[initial]:em[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[em[initial]:em[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[em[initial]:em[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[em[initial]:em[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[em[initial]:em[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[em[initial]:em[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[em[initial]:em[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[em[initial]:em[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[em[initial]:em[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[em[initial]:em[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value




em1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

em1=as.vector(em1)



for (i in 1:length(em1)) {
if(em1[i]<0.05){ em1[i]=0; i = i+1}}

Emilia =round(em1, digits = 2)











w1=wilcox.test(aa[fr[initial]:fr[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[fr[initial]:fr[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[fr[initial]:fr[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[fr[initial]:fr[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[fr[initial]:fr[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[fr[initial]:fr[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[fr[initial]:fr[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[fr[initial]:fr[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[fr[initial]:fr[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[fr[initial]:fr[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[fr[initial]:fr[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[fr[initial]:fr[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[fr[initial]:fr[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[fr[initial]:fr[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[fr[initial]:fr[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[fr[initial]:fr[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[fr[initial]:fr[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[fr[initial]:fr[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[fr[initial]:fr[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[fr[initial]:fr[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[fr[initial]:fr[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value






fr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

fr1=as.vector(fr1)



for (i in 1:length(fr1)) {
if(fr1[i]<0.05){ fr1[i]=0; i = i+1}}

Friuli =round(fr1, digits = 2)












w1=wilcox.test(aa[laz[initial]:laz[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[laz[initial]:laz[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[laz[initial]:laz[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[laz[initial]:laz[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[laz[initial]:laz[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[laz[initial]:laz[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[laz[initial]:laz[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[laz[initial]:laz[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[laz[initial]:laz[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[laz[initial]:laz[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[laz[initial]:laz[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[laz[initial]:laz[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[laz[initial]:laz[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[laz[initial]:laz[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[laz[initial]:laz[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[laz[initial]:laz[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[laz[initial]:laz[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[laz[initial]:laz[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[laz[initial]:laz[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[laz[initial]:laz[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[laz[initial]:laz[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value





laz1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

laz1=as.vector(laz1)



for (i in 1:length(laz1)) {
if(laz1[i]<0.05){ laz1[i]=0; i = i+1}}

Lazio =round(laz1, digits = 2)














w1=wilcox.test(aa[lig[initial]:lig[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[lig[initial]:lig[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[lig[initial]:lig[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[lig[initial]:lig[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[lig[initial]:lig[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[lig[initial]:lig[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[lig[initial]:lig[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[lig[initial]:lig[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[lig[initial]:lig[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[lig[initial]:lig[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[lig[initial]:lig[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[lig[initial]:lig[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[lig[initial]:lig[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[lig[initial]:lig[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[lig[initial]:lig[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[lig[initial]:lig[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[lig[initial]:lig[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[lig[initial]:lig[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[lig[initial]:lig[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[lig[initial]:lig[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[lig[initial]:lig[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value




lig1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

lig1=as.vector(lig1)



for (i in 1:length(lig1)) {
if(lig1[i]<0.05){ lig1[i]=0; i = i+1}}

Liguria =round(lig1, digits = 2)














w1=wilcox.test(aa[lom[initial]:lom[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[lom[initial]:lom[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[lom[initial]:lom[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[lom[initial]:lom[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[lom[initial]:lom[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[lom[initial]:lom[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[lom[initial]:lom[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[lom[initial]:lom[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[lom[initial]:lom[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[lom[initial]:lom[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[lom[initial]:lom[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[lom[initial]:lom[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[lom[initial]:lom[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[lom[initial]:lom[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[lom[initial]:lom[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[lom[initial]:lom[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[lom[initial]:lom[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[lom[initial]:lom[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[lom[initial]:lom[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[lom[initial]:lom[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[lom[initial]:lom[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value




lomb1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

lomb1=as.vector(lomb1)



for (i in 1:length(lomb1)) {
if(lomb1[i]<0.05){ lomb1[i]=0; i = i+1}}

Lombardia =round(lomb1, digits = 2)














w1=wilcox.test(aa[mar[initial]:mar[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[mar[initial]:mar[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[mar[initial]:mar[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[mar[initial]:mar[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[mar[initial]:mar[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[mar[initial]:mar[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[mar[initial]:mar[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[mar[initial]:mar[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[mar[initial]:mar[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[mar[initial]:mar[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[mar[initial]:mar[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[mar[initial]:mar[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[mar[initial]:mar[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[mar[initial]:mar[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[mar[initial]:mar[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[mar[initial]:mar[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[mar[initial]:mar[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[mar[initial]:mar[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[mar[initial]:mar[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[mar[initial]:mar[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[mar[initial]:mar[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value





marc1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

marc1=as.vector(marc1)



for (i in 1:length(marc1)) {
if(marc1[i]<0.05){ marc1[i]=0; i = i+1}}

Marche =round(marc1, digits = 2)














w1=wilcox.test(aa[mol[initial]:mol[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[mol[initial]:mol[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[mol[initial]:mol[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[mol[initial]:mol[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[mol[initial]:mol[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[mol[initial]:mol[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[mol[initial]:mol[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[mol[initial]:mol[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[mol[initial]:mol[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[mol[initial]:mol[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[mol[initial]:mol[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[mol[initial]:mol[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[mol[initial]:mol[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[mol[initial]:mol[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[mol[initial]:mol[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[mol[initial]:mol[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[mol[initial]:mol[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[mol[initial]:mol[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[mol[initial]:mol[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[mol[initial]:mol[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[mol[initial]:mol[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value




mol1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

mol1=as.vector(mol1)



for (i in 1:length(mol1)) {
if(mol1[i]<0.05){ mol1[i]=0; i = i+1}}

Molise =round(mol1, digits = 2)













w1=wilcox.test(aa[pie[initial]:pie[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[pie[initial]:pie[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[pie[initial]:pie[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[pie[initial]:pie[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[pie[initial]:pie[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[pie[initial]:pie[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[pie[initial]:pie[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[pie[initial]:pie[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[pie[initial]:pie[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[pie[initial]:pie[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[pie[initial]:pie[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[pie[initial]:pie[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[pie[initial]:pie[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[pie[initial]:pie[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[pie[initial]:pie[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[pie[initial]:pie[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[pie[initial]:pie[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[pie[initial]:pie[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[pie[initial]:pie[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[pie[initial]:pie[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[pie[initial]:pie[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value








piem1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

piem1=as.vector(piem1)



for (i in 1:length(piem1)) {
if(piem1[i]<0.05){ piem1[i]=0; i = i+1}}

Piemonte =round(piem1, digits = 2)














w1=wilcox.test(aa[pug[initial]:pug[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[pug[initial]:pug[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[pug[initial]:pug[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[pug[initial]:pug[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[pug[initial]:pug[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[pug[initial]:pug[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[pug[initial]:pug[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[pug[initial]:pug[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[pug[initial]:pug[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[pug[initial]:pug[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[pug[initial]:pug[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[pug[initial]:pug[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[pug[initial]:pug[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[pug[initial]:pug[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[pug[initial]:pug[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[pug[initial]:pug[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[pug[initial]:pug[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[pug[initial]:pug[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[pug[initial]:pug[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[pug[initial]:pug[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[pug[initial]:pug[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value



pugl1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

pugl1=as.vector(pugl1)



for (i in 1:length(pugl1)) {
if(pugl1[i]<0.05){ pugl1[i]=0; i = i+1}}

Puglia =round(pugl1, digits = 2)












w1=wilcox.test(aa[sar[initial]:sar[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[sar[initial]:sar[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[sar[initial]:sar[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[sar[initial]:sar[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[sar[initial]:sar[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[sar[initial]:sar[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[sar[initial]:sar[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[sar[initial]:sar[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[sar[initial]:sar[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[sar[initial]:sar[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[sar[initial]:sar[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[sar[initial]:sar[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[sar[initial]:sar[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[sar[initial]:sar[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[sar[initial]:sar[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[sar[initial]:sar[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[sar[initial]:sar[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[sar[initial]:sar[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[sar[initial]:sar[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[sar[initial]:sar[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[sar[initial]:sar[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value



sard1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

sard1=as.vector(sard1)



for (i in 1:length(sard1)) {
if(sard1[i]<0.05){ sard1[i]=0; i = i+1}}

Sardegna =round(sard1, digits = 2)












w1=wilcox.test(aa[sic[initial]:sic[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[sic[initial]:sic[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[sic[initial]:sic[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[sic[initial]:sic[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[sic[initial]:sic[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[sic[initial]:sic[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[sic[initial]:sic[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[sic[initial]:sic[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[sic[initial]:sic[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[sic[initial]:sic[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[sic[initial]:sic[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[sic[initial]:sic[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[sic[initial]:sic[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[sic[initial]:sic[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[sic[initial]:sic[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[sic[initial]:sic[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[sic[initial]:sic[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[sic[initial]:sic[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[sic[initial]:sic[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[sic[initial]:sic[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[sic[initial]:sic[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value




sic1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

sic1=as.vector(sic1)



for (i in 1:length(sic1)) {
if(sic1[i]<0.05){ sic1[i]=0; i = i+1}}

Sicilia =round(sic1, digits = 2)












w1=wilcox.test(aa[tos[initial]:tos[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[tos[initial]:tos[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[tos[initial]:tos[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[tos[initial]:tos[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[tos[initial]:tos[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[tos[initial]:tos[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[tos[initial]:tos[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[tos[initial]:tos[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[tos[initial]:tos[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[tos[initial]:tos[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[tos[initial]:tos[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[tos[initial]:tos[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[tos[initial]:tos[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[tos[initial]:tos[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[tos[initial]:tos[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[tos[initial]:tos[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[tos[initial]:tos[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[tos[initial]:tos[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[tos[initial]:tos[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[tos[initial]:tos[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[tos[initial]:tos[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value





tosc1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

tosc1=as.vector(tosc1)



for (i in 1:length(tosc1)) {
if(tosc1[i]<0.05){ tosc1[i]=0; i = i+1}}

Toscana =round(tosc1, digits = 2)














w1=wilcox.test(aa[tre[initial]:tre[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[tre[initial]:tre[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[tre[initial]:tre[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[tre[initial]:tre[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[tre[initial]:tre[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[tre[initial]:tre[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[tre[initial]:tre[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[tre[initial]:tre[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[tre[initial]:tre[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[tre[initial]:tre[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[tre[initial]:tre[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[tre[initial]:tre[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[tre[initial]:tre[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[tre[initial]:tre[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[tre[initial]:tre[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[tre[initial]:tre[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[tre[initial]:tre[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[tre[initial]:tre[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[tre[initial]:tre[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[tre[initial]:tre[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[tre[initial]:tre[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value






trent1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

trent1=as.vector(trent1)



 for (i in 1:length(trent1)) {
  if(trent1[i]<0.05){ trent1[i]=0; i = i+1}}

Trento =round(trent1, digits = 2)












w1=wilcox.test(aa[umb[initial]:umb[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[umb[initial]:umb[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[umb[initial]:umb[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[umb[initial]:umb[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[umb[initial]:umb[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[umb[initial]:umb[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[umb[initial]:umb[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[umb[initial]:umb[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[umb[initial]:umb[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[umb[initial]:umb[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[umb[initial]:umb[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[umb[initial]:umb[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[umb[initial]:umb[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[umb[initial]:umb[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[umb[initial]:umb[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[umb[initial]:umb[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[umb[initial]:umb[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[umb[initial]:umb[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[umb[initial]:umb[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[umb[initial]:umb[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[umb[initial]:umb[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value



umbr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

umbr1=as.vector(umbr1)



for (i in 1:length(umbr1)) {
if(umbr1[i]<0.05){ umbr1[i]=0; i = i+1}}

Umbria =round(umbr1, digits = 2)














w1=wilcox.test(aa[vall[initial]:vall[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[vall[initial]:vall[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[vall[initial]:vall[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[vall[initial]:vall[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[vall[initial]:vall[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[vall[initial]:vall[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[vall[initial]:vall[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[vall[initial]:vall[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[vall[initial]:vall[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[vall[initial]:vall[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[vall[initial]:vall[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[vall[initial]:vall[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[vall[initial]:vall[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[vall[initial]:vall[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[vall[initial]:vall[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[vall[initial]:vall[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[vall[initial]:vall[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[vall[initial]:vall[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[vall[initial]:vall[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[vall[initial]:vall[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[vall[initial]:vall[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value



va1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

va1=as.vector(va1)



for (i in 1:length(va1)) {
 if(va1[i]<0.05){ va1[i]=0; i = i+1}}

ValleAosta =round(va1, digits = 2)















w1=wilcox.test(aa[ven[initial]:ven[numg]],aa[abr[initial]:abr[numg]], correct=FALSE)$p.value


w2=wilcox.test(aa[ven[initial]:ven[numg]],aa[bas[initial]:bas[numg]], correct=FALSE)$p.value

w3=wilcox.test(aa[ven[initial]:ven[numg]],aa[bol[initial]:bol[numg]], correct=FALSE)$p.value



w4=wilcox.test(aa[ven[initial]:ven[numg]],aa[cal[initial]:cal[numg]], correct=FALSE)$p.value

w5=wilcox.test(aa[ven[initial]:ven[numg]],aa[cam[initial]:cam[numg]], correct=FALSE)$p.value



w6=wilcox.test(aa[ven[initial]:ven[numg]],aa[em[initial]:em[numg]], correct=FALSE)$p.value



w7=wilcox.test(aa[ven[initial]:ven[numg]],aa[fr[initial]:fr[numg]], correct=FALSE)$p.value


w8=wilcox.test(aa[ven[initial]:ven[numg]],aa[laz[initial]:laz[numg]], correct=FALSE)$p.value



w9=wilcox.test(aa[ven[initial]:ven[numg]],aa[lig[initial]:lig[numg]], correct=FALSE)$p.value


w10=wilcox.test(aa[ven[initial]:ven[numg]],aa[lom[initial]:lom[numg]], correct=FALSE)$p.value


w11=wilcox.test(aa[ven[initial]:ven[numg]],aa[mar[initial]:mar[numg]], correct=FALSE)$p.value


w12=wilcox.test(aa[ven[initial]:ven[numg]],aa[mol[initial]:mol[numg]], correct=FALSE)$p.value


w13=wilcox.test(aa[ven[initial]:ven[numg]],aa[pie[initial]:pie[numg]], correct=FALSE)$p.value


w14=wilcox.test(aa[ven[initial]:ven[numg]],aa[pug[initial]:pug[numg]], correct=FALSE)$p.value

w15=wilcox.test(aa[ven[initial]:ven[numg]],aa[sar[initial]:sar[numg]], correct=FALSE)$p.value


w16=wilcox.test(aa[ven[initial]:ven[numg]],aa[sic[initial]:sic[numg]], correct=FALSE)$p.value

w17=wilcox.test(aa[ven[initial]:ven[numg]],aa[tos[initial]:tos[numg]], correct=FALSE)$p.value


w18=wilcox.test(aa[ven[initial]:ven[numg]],aa[tre[initial]:tre[numg]], correct=FALSE)$p.value


w19=wilcox.test(aa[ven[initial]:ven[numg]],aa[umb[initial]:umb[numg]], correct=FALSE)$p.value


w20=wilcox.test(aa[ven[initial]:ven[numg]],aa[vall[initial]:vall[numg]], correct=FALSE)$p.value


w21=wilcox.test(aa[ven[initial]:ven[numg]],aa[ven[initial]:ven[numg]], correct=FALSE)$p.value



ven1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

ven1=as.vector(ven1)



for (i in 1:length(ven1)) {
if(ven1[i]<0.05){ ven1[i]=0; i = i+1}}

Veneto =round(ven1, digits = 2)



tot=cbind(Abruzzo,	Basilicata,	Bolzano,	Calabria,	Campania,	Emilia,	Friuli, 	Lazio,	Liguria,	Lombardia,	Marche,	Molise,	Piemonte,	Puglia,	Sardegna,	Sicilia,	Toscana,	Trento,	Umbria,	ValleAosta,	Veneto)


regions=c("Abruzzo",	"Basilicata",	"Bolzano",	"Calabria",	"Campania",	"Emilia",	"Friuli", 	"Lazio",	"Liguria",	"Lombardia",	"Marche",	"Molise",	"Piemonte",	"Puglia",	"Sardegna",	"Sicilia",	"Toscana",	"Trento",	"Umbria",	"ValleAosta",	"Veneto")

tot=cbind(regions, tot)

write.table(tot, paste0("~/Google Drive (m.milano@unicz.it)/2022-Biotech/matrici/",name,"_2.txt"),append = FALSE, quote = FALSE,row.names=FALSE, sep="\t")


space="tiny"

write.table(tot, paste0("~/Google Drive (m.milano@unicz.it)/2022-Biotech/tabelle/",name,"_2_tb.txt"),append = FALSE, quote = FALSE,row.names=FALSE, col.names=FALSE, sep=paste("\t & \t",space,"\t"))

M1=tot
 
  M1=M1[,-1]
 dim(M1)
     
 #utilizzare su solo x pfam
    
    M1=as.matrix(M1);
    
   
    
    M1[is.na(M1)] <- 0
    
   png(paste0("~/Google Drive (m.milano@unicz.it)/2022-Biotech/reti/", name,"_2.png")) 
 g2 <- graph_from_adjacency_matrix(M1, weighted=TRUE, diag=FALSE, mode='undirected')
 E(g2)$weight



plot(g2, layout=layout_with_fr, vertex.size=10,
     vertex.label.dist=1.5, vertex.color="cyan3", edge.arrow.size=25, main=name)
   
dev.off()
#dev.new()
 png(paste0("~/Google Drive (m.milano@unicz.it)/2022-Biotech/comunita/", name, "_2_CC.png"))
 wc <- cluster_walktrap(g2, weights = E(g2)$weight,
  merges = TRUE, modularity = TRUE, membership = TRUE)
  
  plot(wc, g2,vertex.size=10,
      vertex.label.dist=1.5,main=name)
     
dev.off()

}