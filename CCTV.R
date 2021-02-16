CCTV <- function() {


library(igraph)	
print("Please Input a Valid Database")
print("Author Marianna Milano - Bioinformatics Lab - Unicz")
print("Select type of data:hs, ic, th, hi, tcp, ncp, dh, d, tc, sw")	
	
x=read.table('~/Desktop/2020-Covid/dati_covid/analyzeC19D-master/dataset.txt', header = T, sep = "\t")




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

w1=wilcox.test(aa[1:63],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[1:63],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[1:63],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[1:63],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[1:63],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[1:63],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[1:63],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[1:63],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[1:63],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[1:63],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[1:63],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[1:63],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[1:63],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[1:63],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[1:63],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[1:63],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[1:63],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[1:63],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[1:63],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[1:63],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[1:63],aa[1261:1323], correct=FALSE)$p.value



abr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

abr1=as.vector(abr1)



for (i in 1:length(abr1)) {
if(abr1[i]<0.05){ abr1[i]=0; i = i+1}}

Abruzzo =round(abr1, digits = 2)











w1=wilcox.test(aa[64:126],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[64:126],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[64:126],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[64:126],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[64:126],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[64:126],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[64:126],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[64:126],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[64:126],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[64:126],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[64:126],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[64:126],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[64:126],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[64:126],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[64:126],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[64:126],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[64:126],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[64:126],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[64:126],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[64:126],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[64:126],aa[1261:1323], correct=FALSE)$p.value






bas1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

bas1=as.vector(bas1)



for (i in 1:length(bas1)) {
if(bas1[i]<0.05){ bas1[i]=0; i = i+1}}

Basilicata =round(bas1, digits = 2)











w1=wilcox.test(aa[127:189],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[127:189],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[127:189],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[127:189],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[127:189],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[127:189],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[127:189],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[127:189],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[127:189],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[127:189],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[127:189],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[127:189],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[127:189],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[127:189],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[127:189],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[127:189],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[127:189],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[127:189],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[127:189],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[127:189],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[127:189],aa[1261:1323], correct=FALSE)$p.value




bolz1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

bolz1=as.vector(bolz1)



for (i in 1:length(bolz1)) {
if(bolz1[i]<0.05){ bolz1[i]=0; i = i+1}}

Bolzano =round(bolz1, digits = 2)














w1=wilcox.test(aa[190:252],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[190:252],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[190:252],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[190:252],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[190:252],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[190:252],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[190:252],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[190:252],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[190:252],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[190:252],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[190:252],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[190:252],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[190:252],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[190:252],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[190:252],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[190:252],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[190:252],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[190:252],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[190:252],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[190:252],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[190:252],aa[1261:1323], correct=FALSE)$p.value




calabr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

calabr1=as.vector(calabr1)



for (i in 1:length(calabr1)) {
if(calabr1[i]<0.05){ calabr1[i]=0; i = i+1}}

Calabria =round(calabr1, digits = 2)













w1=wilcox.test(aa[253:315],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[253:315],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[253:315],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[253:315],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[253:315],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[253:315],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[253:315],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[253:315],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[253:315],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[253:315],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[253:315],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[253:315],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[253:315],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[253:315],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[253:315],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[253:315],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[253:315],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[253:315],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[253:315],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[253:315],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[253:315],aa[1261:1323], correct=FALSE)$p.value





camp1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

camp1=as.vector(camp1)



for (i in 1:length(camp1)) {
if(camp1[i]<0.05){ camp1[i]=0; i = i+1}}

Campania =round(camp1, digits = 2)









w1=wilcox.test(aa[316:378],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[316:378],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[316:378],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[316:378],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[316:378],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[316:378],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[316:378],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[316:378],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[316:378],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[316:378],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[316:378],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[316:378],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[316:378],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[316:378],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[316:378],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[316:378],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[316:378],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[316:378],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[316:378],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[316:378],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[316:378],aa[1261:1323], correct=FALSE)$p.value




em1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

em1=as.vector(em1)



for (i in 1:length(em1)) {
if(em1[i]<0.05){ em1[i]=0; i = i+1}}

Emilia =round(em1, digits = 2)











w1=wilcox.test(aa[379:441],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[379:441],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[379:441],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[379:441],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[379:441],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[379:441],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[379:441],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[379:441],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[379:441],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[379:441],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[379:441],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[379:441],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[379:441],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[379:441],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[379:441],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[379:441],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[379:441],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[379:441],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[379:441],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[379:441],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[379:441],aa[1261:1323], correct=FALSE)$p.value






fr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

fr1=as.vector(fr1)



for (i in 1:length(fr1)) {
if(fr1[i]<0.05){ fr1[i]=0; i = i+1}}

Friuli =round(fr1, digits = 2)












w1=wilcox.test(aa[442:504],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[442:504],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[442:504],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[442:504],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[442:504],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[442:504],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[442:504],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[442:504],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[442:504],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[442:504],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[442:504],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[442:504],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[442:504],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[442:504],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[442:504],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[442:504],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[442:504],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[442:504],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[442:504],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[442:504],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[442:504],aa[1261:1323], correct=FALSE)$p.value





laz1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

laz1=as.vector(laz1)



for (i in 1:length(laz1)) {
if(laz1[i]<0.05){ laz1[i]=0; i = i+1}}

Lazio =round(laz1, digits = 2)














w1=wilcox.test(aa[505:567],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[505:567],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[505:567],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[505:567],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[505:567],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[505:567],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[505:567],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[505:567],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[505:567],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[505:567],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[505:567],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[505:567],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[505:567],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[505:567],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[505:567],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[505:567],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[505:567],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[505:567],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[505:567],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[505:567],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[505:567],aa[1261:1323], correct=FALSE)$p.value




lig1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

lig1=as.vector(lig1)



for (i in 1:length(lig1)) {
if(lig1[i]<0.05){ lig1[i]=0; i = i+1}}

Liguria =round(lig1, digits = 2)














w1=wilcox.test(aa[568:630],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[568:630],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[568:630],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[568:630],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[568:630],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[568:630],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[568:630],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[568:630],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[568:630],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[568:630],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[568:630],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[568:630],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[568:630],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[568:630],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[568:630],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[568:630],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[568:630],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[568:630],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[568:630],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[568:630],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[568:630],aa[1261:1323], correct=FALSE)$p.value




lomb1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

lomb1=as.vector(lomb1)



for (i in 1:length(lomb1)) {
if(lomb1[i]<0.05){ lomb1[i]=0; i = i+1}}

Lombardia =round(lomb1, digits = 2)














w1=wilcox.test(aa[631:693],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[631:693],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[631:693],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[631:693],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[631:693],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[631:693],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[631:693],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[631:693],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[631:693],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[631:693],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[631:693],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[631:693],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[631:693],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[631:693],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[631:693],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[631:693],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[631:693],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[631:693],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[631:693],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[631:693],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[631:693],aa[1261:1323], correct=FALSE)$p.value





marc1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

marc1=as.vector(marc1)



for (i in 1:length(marc1)) {
if(marc1[i]<0.05){ marc1[i]=0; i = i+1}}

Marche =round(marc1, digits = 2)














w1=wilcox.test(aa[694:756],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[694:756],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[694:756],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[694:756],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[694:756],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[694:756],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[694:756],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[694:756],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[694:756],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[694:756],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[694:756],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[694:756],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[694:756],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[694:756],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[694:756],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[694:756],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[694:756],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[694:756],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[694:756],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[694:756],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[694:756],aa[1261:1323], correct=FALSE)$p.value




mol1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

mol1=as.vector(mol1)



for (i in 1:length(mol1)) {
if(mol1[i]<0.05){ mol1[i]=0; i = i+1}}

Molise =round(mol1, digits = 2)













w1=wilcox.test(aa[757:819],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[757:819],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[757:819],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[757:819],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[757:819],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[757:819],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[757:819],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[757:819],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[757:819],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[757:819],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[757:819],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[757:819],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[757:819],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[757:819],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[757:819],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[757:819],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[757:819],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[757:819],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[757:819],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[757:819],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[757:819],aa[1261:1323], correct=FALSE)$p.value








piem1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

piem1=as.vector(piem1)



for (i in 1:length(piem1)) {
if(piem1[i]<0.05){ piem1[i]=0; i = i+1}}

Piemonte =round(piem1, digits = 2)














w1=wilcox.test(aa[820:882],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[820:882],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[820:882],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[820:882],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[820:882],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[820:882],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[820:882],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[820:882],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[820:882],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[820:882],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[820:882],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[820:882],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[820:882],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[820:882],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[820:882],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[820:882],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[820:882],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[820:882],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[820:882],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[820:882],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[820:882],aa[1261:1323], correct=FALSE)$p.value



pugl1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

pugl1=as.vector(pugl1)



for (i in 1:length(pugl1)) {
if(pugl1[i]<0.05){ pugl1[i]=0; i = i+1}}

Puglia =round(pugl1, digits = 2)












w1=wilcox.test(aa[883:945],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[883:945],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[883:945],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[883:945],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[883:945],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[883:945],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[883:945],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[883:945],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[883:945],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[883:945],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[883:945],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[883:945],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[883:945],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[883:945],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[883:945],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[883:945],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[883:945],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[883:945],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[883:945],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[883:945],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[883:945],aa[1261:1323], correct=FALSE)$p.value



sard1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

sard1=as.vector(sard1)



for (i in 1:length(sard1)) {
if(sard1[i]<0.05){ sard1[i]=0; i = i+1}}

Sardegna =round(sard1, digits = 2)












w1=wilcox.test(aa[946:1008],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[946:1008],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[946:1008],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[946:1008],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[946:1008],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[946:1008],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[946:1008],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[946:1008],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[946:1008],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[946:1008],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[946:1008],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[946:1008],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[946:1008],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[946:1008],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[946:1008],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[946:1008],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[946:1008],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[946:1008],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[946:1008],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[946:1008],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[946:1008],aa[1261:1323], correct=FALSE)$p.value




sic1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

sic1=as.vector(sic1)



for (i in 1:length(sic1)) {
if(sic1[i]<0.05){ sic1[i]=0; i = i+1}}

Sicilia =round(sic1, digits = 2)












w1=wilcox.test(aa[1009:1071],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[1009:1071],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[1009:1071],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[1009:1071],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[1009:1071],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[1009:1071],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[1009:1071],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[1009:1071],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[1009:1071],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[1009:1071],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[1009:1071],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[1009:1071],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[1009:1071],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[1009:1071],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[1009:1071],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[1009:1071],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[1009:1071],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[1009:1071],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[1009:1071],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[1009:1071],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[1009:1071],aa[1261:1323], correct=FALSE)$p.value





tosc1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

tosc1=as.vector(tosc1)



for (i in 1:length(tosc1)) {
if(tosc1[i]<0.05){ tosc1[i]=0; i = i+1}}

Toscana =round(tosc1, digits = 2)














w1=wilcox.test(aa[1072:1134],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[1072:1134],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[1072:1134],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[1072:1134],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[1072:1134],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[1072:1134],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[1072:1134],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[1072:1134],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[1072:1134],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[1072:1134],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[1072:1134],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[1072:1134],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[1072:1134],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[1072:1134],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[1072:1134],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[1072:1134],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[1072:1134],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[1072:1134],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[1072:1134],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[1072:1134],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[1072:1134],aa[1261:1323], correct=FALSE)$p.value






trent1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

trent1=as.vector(trent1)



for (i in 1:length(trent1)) {
if(trent1[i]<0.05){ trent1[i]=0; i = i+1}}

Trento =round(trent1, digits = 2)












w1=wilcox.test(aa[1135:1197],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[1135:1197],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[1135:1197],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[1135:1197],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[1135:1197],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[1135:1197],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[1135:1197],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[1135:1197],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[1135:1197],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[1135:1197],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[1135:1197],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[1135:1197],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[1135:1197],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[1135:1197],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[1135:1197],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[1135:1197],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[1135:1197],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[1135:1197],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[1135:1197],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[1135:1197],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[1135:1197],aa[1261:1323], correct=FALSE)$p.value



umbr1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

umbr1=as.vector(umbr1)



for (i in 1:length(umbr1)) {
if(umbr1[i]<0.05){ umbr1[i]=0; i = i+1}}

Umbria =round(umbr1, digits = 2)














w1=wilcox.test(aa[1198:1260],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[1198:1260],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[1198:1260],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[1198:1260],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[1198:1260],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[1198:1260],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[1198:1260],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[1198:1260],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[1198:1260],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[1198:1260],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[1198:1260],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[1198:1260],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[1198:1260],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[1198:1260],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[1198:1260],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[1198:1260],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[1198:1260],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[1198:1260],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[1198:1260],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[1198:1260],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[1198:1260],aa[1261:1323], correct=FALSE)$p.value



va1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

va1=as.vector(va1)



for (i in 1:length(va1)) {
if(va1[i]<0.05){ va1[i]=0; i = i+1}}

ValleAosta =round(va1, digits = 2)















w1=wilcox.test(aa[1261:1323],aa[1:63], correct=FALSE)$p.value


w2=wilcox.test(aa[1261:1323],aa[64:126], correct=FALSE)$p.value

w3=wilcox.test(aa[1261:1323],aa[127:189], correct=FALSE)$p.value



w4=wilcox.test(aa[1261:1323],aa[190:252], correct=FALSE)$p.value

w5=wilcox.test(aa[1261:1323],aa[253:315], correct=FALSE)$p.value



w6=wilcox.test(aa[1261:1323],aa[316:378], correct=FALSE)$p.value



w7=wilcox.test(aa[1261:1323],aa[379:441], correct=FALSE)$p.value


w8=wilcox.test(aa[1261:1323],aa[442:504], correct=FALSE)$p.value



w9=wilcox.test(aa[1261:1323],aa[505:567], correct=FALSE)$p.value


w10=wilcox.test(aa[1261:1323],aa[568:630], correct=FALSE)$p.value


w11=wilcox.test(aa[1261:1323],aa[631:693], correct=FALSE)$p.value


w12=wilcox.test(aa[1261:1323],aa[694:756], correct=FALSE)$p.value


w13=wilcox.test(aa[1261:1323],aa[757:819], correct=FALSE)$p.value


w14=wilcox.test(aa[1261:1323],aa[820:882], correct=FALSE)$p.value

w15=wilcox.test(aa[1261:1323],aa[883:945], correct=FALSE)$p.value


w16=wilcox.test(aa[1261:1323],aa[946:1008], correct=FALSE)$p.value

w17=wilcox.test(aa[1261:1323],aa[1009:1071], correct=FALSE)$p.value


w18=wilcox.test(aa[1261:1323],aa[1072:1134], correct=FALSE)$p.value


w19=wilcox.test(aa[1261:1323],aa[1135:1197], correct=FALSE)$p.value


w20=wilcox.test(aa[1261:1323],aa[1198:1260], correct=FALSE)$p.value


w21=wilcox.test(aa[1261:1323],aa[1261:1323], correct=FALSE)$p.value



ven1=rbind(w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12,w13,w14,w15,w16,w17,w18, w19, w20,w21)

ven1=as.vector(ven1)



for (i in 1:length(ven1)) {
if(ven1[i]<0.05){ ven1[i]=0; i = i+1}}

Veneto =round(ven1, digits = 2)



tot=cbind(Abruzzo,	Basilicata,	Bolzano,	Calabria,	Campania,	Emilia,	Friuli, 	Lazio,	Liguria,	Lombardia,	Marche,	Molise,	Piemonte,	Puglia,	Sardegna,	Sicilia,	Toscana,	Trento,	Umbria,	ValleAosta,	Veneto)


regions=c("Abruzzo",	"Basilicata",	"Bolzano",	"Calabria",	"Campania",	"Emilia",	"Friuli", 	"Lazio",	"Liguria",	"Lombardia",	"Marche",	"Molise",	"Piemonte",	"Puglia",	"Sardegna",	"Sicilia",	"Toscana",	"Trento",	"Umbria",	"ValleAosta",	"Veneto")

tot=cbind(regions, tot)

write.table(tot, paste0("~/Desktop/",name,".txt"),append = FALSE, quote = FALSE,row.names=FALSE, sep="\t")


M1=tot
 
  M1=M1[,-1]
 dim(M1)
     
 #utilizzare su solo x pfam
    
    M1=as.matrix(M1);
    
   
    
    M1[is.na(M1)] <- 0
    
   png(paste0("~/Desktop/", name,".png")) 
 g2 <- graph_from_adjacency_matrix(M1, weighted=TRUE, diag=FALSE, mode='undirected')
 E(g2)$weight



plot(g2, layout=layout_with_fr, vertex.size=10,
     vertex.label.dist=1.5, vertex.color="cyan3", edge.arrow.size=25, main=name)
   
dev.off()
#dev.new()
 png(paste0("~/Desktop/", name, "_CC. png"))
 wc <- cluster_walktrap(g2, weights = E(g2)$weight,
  merges = TRUE, modularity = TRUE, membership = TRUE)
  
  plot(wc, g2,vertex.size=10,
      vertex.label.dist=1.5,main=name)
     
dev.off()

}