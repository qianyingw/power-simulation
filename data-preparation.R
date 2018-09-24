setwd("D:/PhD/Sim-180916/submit")
study <- read.csv("3116w.csv", header = T)
attach(study)


# --- Switch label matrix to categorical value matrix -----
stu <- matrix(0, nrow=nrow(study), ncol = 26, byrow = T)
stu[,1] = study[,1]

# d1: Drug.Group
# count(Drug.Group) 
stu[,2][study[,2]=="Stem Cells"]=1; stu[,2][study[,2]=="Stem cells"]=1 
stu[,2][study[,2]=="Growth factor"]=2
stu[,2][study[,2]=="Thrombolytic"]=3            
stu[,2][study[,2]=="Antidepressant"]=4; stu[,2][study[,2]=="antidepressant"]=4
stu[,2][study[,2]=="NOS Inhibitor"]=5
stu[,2][study[,2]=="Anti-Inflammatory"]=6; 
stu[,2][study[,2]=="PPAR-gamma agonist"]=7
stu[,2][study[,2]=="Antioxidant"]=8; stu[,2][study[,2]=="Anti-oxidant"]=8
stu[,2][study[,2]=="Vitamin"]=9

# d2: Animal
# count(Animal)  # 2631+(451+11) = 3093
stu[,3][study[,3]=="Rat"]=1
stu[,3][study[,3]=="Mouse"]=2; stu[,3][study[,3]=="mouse"]=2

# d3: Sex
# count(Sex)
stu[,4][study[,4]=="Male"]=1
stu[,4][study[,4]=="Unknown"]=2
stu[,4][study[,4]=="Female"]=3

# d4: Type.of.Ischaemia 
# count(Type.of.Ischaemia)
stu[,5][study[,5]=="Temporary"]=1
stu[,5][study[,5]=="Permanent"]=2

# d5: Anaesthetic.ID
# count(Anaesthetic.ID)
stu[,6][study[,6]==1]=1
stu[,6][study[,6]==2]=2
stu[,6][study[,6]==6]=3
stu[,6][study[,6]==3]=4
stu[,6][study[,6]==5]=5

# d6: Method.of.Induction.of.Injury
# count(Method.of.Induction.of.Injury)
stu[,7][study[,7]==17]=1
stu[,7][study[,7]==5]=2
stu[,7][study[,7]==1]=3
stu[,7][study[,7]==36]=4
stu[,7][study[,7]==3]=5
stu[,7][study[,7]==2]=6
stu[,7][study[,7]==16]=7

# d7: Method.of.Quantification.of.Injury
# count(Method.of.Quantification.of.Injury)
stu[,8][study[,8]==1]=1
stu[,8][study[,8]==2]=2
stu[,8][study[,8]==3]=3
stu[,8][study[,8]==6]=4
stu[,8][study[,8]==4]=5

# d8: Ventiliation
# count(Ventiliation)
stu[,9][study[,9]=="Unknown"]=1; stu[,9][study[,9]=="unknown"]=1
stu[,9][study[,9]=="Spontaneous"]=2

# d9: Route.of.Drug.Delivery
# count(Route.of.Drug.Delivery)
stu[,10][study[,10]=="IVenous"]=1; stu[,10][study[,10]=="Ivenous"]=1; 
stu[,10][study[,10]=="Iperitoneal"]=2; stu[,10][study[,10]=="IPeritoneal"]=2; stu[,10][study[,10]=="IPperitoneal"]=2
stu[,10][study[,10]=="SubCut"]=3
stu[,10][study[,10]=="ICerebVentricular"]=4
stu[,10][study[,10]=="Stereotactic"]=5; stu[,10][study[,10]=="stereotactic"]=5

# d10: Comorbidity
# count(Comorbidity)
stu[,11][study[,11]==2]=1

# b1: Cotreatment.in.both.Groups
# b2: Cotreatment.in.Treatment.Group.Only
# b3: Peer.Review.Publication
# b4: Control.of.Temperature
# b5: Monitoring.of.Physiological.Variables
# b6: Random.Allocation.to.Group
# b7: Blinded.Induction.of.Ischaemia
# b8: Blinded.Assessment.of.Outcome
# b9: Anaesthetic.without.marked.intrinsic.neuroprotective.activity
# b10: Use.of.Comorbid.Animals
# b11: Sample.Size.Calculation
# b12: Compliance.with.Animal.Welfare.Regulations
# b13: Statement.of.Potential.Conflicts.of.Interest
for (i in 12:24)  {stu[,i][study[,i]=="TRUE"]=1}

# c1: Time.of.Administration..mins.
tem <- data.frame(cbind(study[,1], study[,25]))
tem1 <- tem[order(tem[,2]),]
tem1[1:623,2] = -2
tem1[624:1246,2] = 0.17
tem1[1247:1869,2] = 2
tem1[1870:2492,2] = 6
tem1[2493:3116,2] = 24
tem2 <- tem1[order(tem1[,1]),]
stu[,25] = tem2[,2]

# c2: Time.of.Assessment..hrs.
tem <- data.frame(cbind(study[,1], study[,26]))
tem1 <- tem[order(tem[,2]),]
tem1[1:1246,2] = 24
tem1[1247:1869,2] = 72
tem1[1870:2492,2] = 168
tem1[2493:3116,2] = 672
tem2 <- tem1[order(tem1[,1]),]
stu[,26] = tem2[,2]

colnames(stu) <- c("ID","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10",
                   "b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","c1","c2")
save(stu, file = "3116stu.RData")


# ---------- meta-regression (NMD) -------------
load("3116stu.RData")
res <- rma(yi = study$yi, vi = (study$si)^2,
           mods = ~ factor(d1)+factor(d2)+factor(d3)+factor(d4)+factor(d5)
           +factor(d6)+factor(d7)+factor(d8)+factor(d9)+factor(d10)
           +factor(b1)+factor(b2)+factor(b3)+factor(b4)+factor(b5)
           +factor(b6)+factor(b7)+factor(b8)+factor(b9)+factor(b10)
           +factor(b11)+factor(b12)+factor(b13)
           +c1+c2-1, 
           data = stu, 
           method = "REML")


coef.nmd = res$b
save(coef.nmd, file = "coef.nmd.RData")

# ---------- meta-regression (SMD) -------------
res <- rma(yi = study$yi.smd, vi = (as.numeric(study$si.smd))^2,
           mods = ~ factor(d1)+factor(d2)+factor(d3)+factor(d4)+factor(d5)
           +factor(d6)+factor(d7)+factor(d8)+factor(d9)+factor(d10)
           +factor(b1)+factor(b2)+factor(b3)+factor(b4)+factor(b5)
           +factor(b6)+factor(b7)+factor(b8)+factor(b9)+factor(b10)
           +factor(b11)+factor(b12)+factor(b13)
           +c1+c2-1, 
           data = stu, 
           method = "REML")

coef.smd = res$b
save(coef.smd, file = "coef.smd.RData")




# ---------- frequency of variables -------------
freq = list()
for (i in 1:25) {
  temp = table(stu[,i+1])
  freq[[i]] = c(temp[2:length(temp)],temp[1])/nrow(stu)
}
save(freq, file = "freq.RData")

# ---------- exploring true effect -------------
load("coef.nmd.RData")
coef = as.matrix(c(coef))

# load("coef.smd.RData")
# coef = as.matrix(c(coef.smd))

N = 100
base = 0
beta.voi = 0
beta.cf = 0
p.cf = 0.4
SD = 20
a = 0.4
b = 0.2
p1 = a
p0 = b

tlist = c()

for (nr in 1:1000) {
  
  set.seed(nr)
  
  # --------------- generated random sample for 25 variables --------------------
  cmat = matrix(nrow = N, ncol = 25, byrow = T)
  cmat = data.frame(cmat)
  for(j in 1:23) {
    cmat[,j] = factor(sample(1:length(freq[[j]]), size = N, replace = T, prob = freq[[j]]))
  }
  cmat[,24] <- sample(c(-2, 0.17, 2, 6, 24), size = N, replace = T, prob = freq[[24]])
  cmat[,25] <- sample(c(24, 72, 168, 672), size = N, replace = T, prob = freq[[25]])
  tname = c()
  for (j in 1:25) { tname = c(tname,paste0("x",j)) }
  colnames(cmat) = tname
  


  # -------- convert 25 variables maxtrix to dummay matrix ---------------------   
  dmat <- matrix(0, nrow = N, ncol = 56, byrow = T)
  no <- c(10,3,4,3,6,8,6,3,6,2, rep(2,13)) # (10+3+4+3+6+8+6+3+6+2-10)+13+2 = 56          
  k = 0  
  for(j in 1:23){   
    for(i in 1:(no[j]-1)){ 
      dmat[which(cmat[,j] == i),k+i] <- 1 
    }
    k = k + no[j] - 1
  } 
  dmat = data.frame(dmat)
  dmat[,55:56] <- cmat[,24:25]
  
  d0 = matrix(0, nrow = N, ncol = 1, byrow = T)
  ind = cmat[,1]==10
  d0[ind] <- 1 
  
  
  dmat = cbind(d0, dmat)
  dmat = as.matrix(dmat)
  
  # ----------- cf & voi -----------------------
  voi = c()
  cf = sample(c(1,0), size=N, replace=T, prob=c(p.cf, 1-p.cf))      
  
  temp1 <- which(cf == 1)       
  voi[temp1] <- sample(c(1,0), size=length(temp1), replace=T, prob=c(p1,1-p1))
  temp0 <- which(cf == 0)       
  voi[temp0] <- sample(c(1,0), size=length(temp0), replace=T, prob=c(p0,1-p0)) 
  
  
  # ------------------------ tv --------------------------
  overall = 17.57 # NMD
  # overall = 0.58 # SMD
  tv = base + dmat%*%coef - overall  + cf*beta.cf + voi*beta.voi
  tlist = cbind(tlist, base + dmat%*%coef - overall)
  
}

vec = colSums(tlist)/100
write.csv(vec,"nmd.csv")
ks.test(vec, "pnorm", mean=mean(vec), sd=sd(vec))




