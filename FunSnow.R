library(metafor)
library(snowfall)

main <- function(seed_index) {
  
  set.seed(seed_index)
  
  N = 100
  meth = "REML"
  sig = 0.05
  
  base = 0
  beta.voi = 0
  beta.cf = 0
  
  p1 = 0.05
  p0 = 0.05
  
  p.cf = 0.4
  SD = 20
  

  coef = as.matrix(c(37.0804,30.815,33.5923,14.3392,8.5259,30.0196,22.5823,43.5495,36.7216,48.3307,
                     -16.3525,-18.9484,
                     12.2995,21.9202,-3.7573,
                     -23.0549,-27.9488,
                     -1.0397,0.6583,11.943,0.0765,0.8218,
                     5.4739,15.5264,-3.6168,-12.8752,1.8262,-37.8981,0.8638,
                     3.9417,-0.6205,0.2414,-48.15,-10.9301,
                     3.8471,3.2422,
                     2.0447,6.7591,12.0347,16.5366,1.5681,
                     -0.096,
                     -2.8812,
                     0.7402,
                     14.7835,
                     0.8577,
                     -8.0857,
                     0.0377,
                     -10.1258,
                     4.8975,
                     2.2466,
                     -2.0812,
                     16.8706,
                     -6.3387,
                     -6.4065,
                     -0.3033,-0.0137
  ))
  
  
  # Multivariate meta-regression formula
  mo = c()
  for (i in 2:57){ mo = paste0(mo,paste0("dmat[,",i,"]+")) }
  mo = paste0("~", mo, "cf+voi")
  
  # # Initial output
  # p.gn=Q.n=p.un=p.mn=c()
  # p.gs=Q.s=p.us=p.ms=c()
  
  #for (i in 1:Nr){
  
  # --------------- cmat --------------------
  x1 <- factor(sample(1:10, size = N, replace = T, prob = c(0.18, 0.16, 0.12, 0.06, 0.06, 0.05, 0.05, 0.09, 0.04, 0.18)))
  x2 <- factor(sample(1:3, size = N, replace = T, prob = c(0.80, 0.14, 0.06)))
  x3 <- factor(sample(1:4, size = N, replace = T, prob = c(0.82, 0.09, 0.07, 0.03)))
  x4 <- factor(sample(1:3, size = N, replace = T, prob = c(0.57, 0.29, 0.14)))
  x5 <- factor(sample(1:6, size = N, replace = T, prob = c(0.39, 0.17, 0.11, 0.09, 0.09, 0.15)))
  x6 <- factor(sample(1:8, size = N, replace = T, prob = c(0.52, 0.09, 0.08, 0.06, 0.06, 0.04, 0.04, 0.13)))
  x7 <- factor(sample(1:6, size = N, replace = T, prob = c(0.46, 0.22, 0.06, 0.05, 0.05, 0.15)))
  x8 <- factor(sample(1:3, size = N, replace = T, prob = c(0.64, 0.22, 0.13)))
  x9 <- factor(sample(1:6, size = N, replace = T, prob = c(0.42, 0.20, 0.11, 0.08, 0.08, 0.10)))
  x10 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.91, 0.09)))
  x11 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.08, 0.92)))
  x12 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.04, 0.96)))
  x13 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.98, 0.02)))
  x14 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.76, 0.24)))
  x15 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.52, 0.48)))
  x16 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.48, 0.52)))
  x17 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.24, 0.76)))
  x18 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.47, 0.53)))
  x19 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.73, 0.27)))
  x20 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.11, 0.89)))
  x21 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.02, 0.98)))
  x22 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.72, 0.28)))
  x23 <- factor(sample(c(1,0), size = N, replace = T, prob = c(0.12, 0.88)))
  x24 <- sample(c(-2, 0.17, 2, 6, 24), size = N, replace = T, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))
  x25 <- sample(c(24, 72, 168, 672), size = N, replace = T, prob = c(0.4, 0.2, 0.2, 0.2))
  
  cmat = data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25)
  
  
  # -------- dmat ---------------------   
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
  tv = base + dmat%*%coef -17.52 + cf*beta.cf + voi*beta.voi
  
  # -------------- mean sd gro --------------
  # Group size
  gro = sample(c(2,3,4,5,6,7,8,9), size = N, replace = T, prob = c(0.56, 0.20, 0.10, 0.07, 0.03, 0.02, 0.01, 0.01))
  
  # Mean & SD (Control group)
  mean.c = matrix(nrow = N, ncol = 1, byrow = T)
  sd.c = matrix(nrow = N, ncol = 1, byrow = T)
  for (i in 1:N){
    temc = rnorm(gro[i], mean = 100, sd = SD)
    mean.c[i] = mean(temc)
    sd.c[i] = sd(temc)
  }
  # Mean & SD (Treatment group)
  mean.e = matrix(nrow = N, ncol = 1, byrow = T)
  sd.e = matrix(nrow = N, ncol = 1, byrow = T)
  for (i in 1:N){
    teme = rnorm(gro[i], mean = 100-tv[i], sd = SD)
    mean.e[i] = mean(teme)
    sd.e[i] = sd(teme)
  }
  
  rm(tv)
  
  # ---------- yi.nmd & vi.nmd ----------------
  yi.nmd = 100*(mean.c-mean.e)/mean.c
  vi.nmd = (100*sd.c/mean.c)^2/gro + (100*sd.e/mean.c)^2/gro
  
  # ---------- yi.smd & vi.smd ----------------
  S = sqrt(((gro-1)*(sd.c^2)+(gro-1)*(sd.e^2))/(2*gro-2))
  yi.smd = (mean.c-mean.e)/S*(1-3/(8*gro-9))   
  vi.smd = 2/gro + yi.smd^2/(4*gro-7.88)
  
  rm(mean.c)
  rm(sd.c)
  rm(gro)
  rm(mean.e)
  rm(sd.e)
  
  
  # ------------ global & sub -------------------
  df.n <- data.frame(yi=yi.nmd, vi=vi.nmd, voi=voi)
  df.s <- data.frame(yi=yi.smd, vi=vi.smd, voi=voi)
  
  rm(yi.nmd)
  rm(yi.smd)
  rm(vi.nmd)
  rm(vi.smd)
  
  gn <- rma(yi, vi, data=df.n, method=meth, verbose=F, digits=5, control=list(maxiter=100000,stepadj=.5))
  gs <- rma(yi, vi, data=df.s, method=meth, verbose=F, digits=5, control=list(maxiter=100000,stepadj=.5))
  
  p.gn = gn$pval
  p.gs = gs$pval
  
  if (sum(voi)==N || sum(voi)==0) {  
    Q.n = NA
    Q.s = NA
    
  } else {
    
    sn1 <- rma(yi, vi, data=df.n, subset=(voi==1), method=meth,verbose=F, digits=5, control=list(maxiter=100000,stepadj=.5))
    sn0 <- rma(yi, vi, data=df.n, subset=(voi==0), method=meth,verbose=F, digits=5, control=list(maxiter=100000,stepadj=.5))
    
    ss1 <- rma(yi, vi, data=df.s, subset=(voi==1), method=meth,verbose=F, digits=5, control=list(maxiter=100000,stepadj=.5))
    ss0 <- rma(yi, vi, data=df.s, subset=(voi==0), method=meth,verbose=F, digits=5, control=list(maxiter=100000,stepadj=.5))
    
    Q.n = gn$QE-sn1$QE-sn0$QE
    Q.s = gs$QE-ss1$QE-ss0$QE
    
 
    rm(gn)
    rm(gs)
    rm(sn1)
    rm(sn0)
    rm(ss1)
    rm(ss0)
  }
  
  # ------------ uni reg -------------------
  un <- rma(yi, vi, data=df.n, mods=~voi, method=meth, control=list(maxiter=100000,stepadj=.5))
  us <- rma(yi, vi, data=df.s, mods=~voi, method=meth, control=list(maxiter=100000,stepadj=.5))
  
  p.un = tail(un$pval,1)
  p.us = tail(us$pval,1)
  
  rm(un)
  rm(us)
  
  
  # ------------ multi reg -----------------
  if (N > 60) {
    mn <- rma(yi, vi, data=df.n, mods=as.formula(mo),
              method=meth, control=list(maxiter=100000,stepadj=.5))     
    ms <- rma(yi, vi, data=df.s, mods=as.formula(mo),
              method=meth, control=list(maxiter=100000,stepadj=.5))  
    
    p.mn = tail(mn$pval,1)
    p.ms = tail(ms$pval,1)
    
    rm(mn)
    rm(ms)
  } 
  
  
  return(cbind(p.gn, p.gs, Q.n, Q.s, p.un, p.us, p.mn, p.ms))
  
}    

              
# Initial output
p.gn=Q.n=p.un=p.mn=c()
p.gs=Q.s=p.us=p.ms=c()

sfInit(parallel=T, cpus=4, type="SOCK")
sfExport(list=list("main"))
sfLibrary(metafor)
system.time(
  out <- sfSapply(1:1000, function(x) main(x), simplify=F)
)
sfStop()  
po = c()
for (i in 1:1000) { po = rbind(po, out[[i]])}
power = data.frame(g.nmd = sum(po[,1]<0.05)/10,
                   g.smd = sum(po[,2]<0.05)/10,
                   p.nmd = sum(po[,3]>3.841, na.rm = T)/10,
                   p.smd = sum(po[,4]>3.841, na.rm = T)/10,
                   u.nmd = sum(po[,5]<0.05)/10,
                   u.smd = sum(po[,6]<0.05)/10,
                   m.nmd = sum(po[,7]<0.05)/10,
                   m.smd = sum(po[,8]<0.05)/10)



