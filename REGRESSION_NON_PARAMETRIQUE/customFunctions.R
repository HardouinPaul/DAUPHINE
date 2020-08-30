###############################################################
### REGRESSION : LIBRAIRIES
###############################################################

library(KernSmooth)
library(stats)
library(np)
library(tidyverse)

###############################################################
### REGRESSION : FONCTIONS
###############################################################

#-------------------------------------------------------------- 
# Pas de fonction qui fait de la validation croisee? Fonction dpill.
#-------------------------------------------------------------- 
# VALIDATION CROISEE (leave one out) du cours avec la fonction ksmooth pacgkage stat
# Attention cette fonction est lente si n est trop grand, il faut sous sampler
# (Voir aussi package sm, function hcv)
#-------------------------------------------------------------- 

# CVbwt
#-------------------------------------------------------------- 
# Cette fonction renvoie pour tout (h in grid) la valeur du risque empirique associé.
# Le h optimal à considérer est le h qui minimise la sortie de CVbwt
CVbwt=function(grid,X,Y){
  JJ=function(h){
    fhati=Vectorize(function(i) ksmooth(X[-i],Y[-i],kernel="normal",bandwidth=h,x.points=X[i])$y)
    Forecast=fhati(1:length(X))
    return(mean((Y-Forecast)**2))     
    print(paste0("nombre NaN Forecast : ", length(-which(is.na(Forecast)))))  
  }
  vy=Vectorize(JJ)(grid)
  return(vy)
}

# mNW >> cvNW >> bw.cv.grid
#-------------------------------------------------------------- 
mNW <- function(x, X, Y, h, K = dnorm) {
  #------------------  
  # Arguments
  # x: evaluation points
  # X: vector (size n) with the predictors
  # Y: vector (size n) with the response variable
  # h: bandwidth
  # K: kernel
  #------------------  
  # Matrix of size n x length(x)
  Kx <- sapply(X, function(Xi) K((x - Xi) / h) / h)
  # Weights
  W <- Kx / rowSums(Kx) # Column recycling!
  # Means at x ("drop" to drop the matrix attributes)
  drop(W %*% Y)
}
########################
cvNW <- function(X, Y, h, K = dnorm) {
  
  sum(((Y - mNW(x = X, X = X, Y = Y, h = h, K = K)) /
         (1 - K(0) / colSums(K(outer(X, X, "-") / h))))^2)
  # Beware: outer() is not very memory-friendly!
  
}
########################
bw.cv.grid <- function(X, Y,
                       h.grid = diff(range(X)) * (seq(0.1, 1, l = 200))^2,
                       K = dnorm, plot.cv = FALSE) {
  obj <- sapply(h.grid, function(h) cvNW(X = X, Y = Y, h = h, K = K))
  h <- h.grid[which.min(obj)]
  if (plot.cv) {
    plot(h.grid, obj, type = "o")
    rug(h.grid)
    abline(v = h, col = 2, lwd = 2)
  }
  h
}

###############################################################
### REGRESSION : FONCTION BENCHMARK
###############################################################

#--------------------------------------------------------------  
# Fonction de demo qui trace sur un meme graphe différents estimateurs
# issus de différents polynomes locaux et différents paramètres de lissage adaptatifs 
#(fonction dpill de R, h de Silverman, validation croisée)
#-------------------------------------------------------------- 

demoKSR=function(X,Y){
  # Parametres
  n=length(X) 
  std=sqrt(var(X))
  h_gridCV=exp(seq(log(std/4),log(std),length=10))# initialisation de grid pour CVbwt
  # Application des differentes methodes
  h_dpill=dpill(X,Y)# adapte a du bruit Gaussien
  #-----------------------------
  h_silver=1.06*std*n**(-1/(4+1))# adapte a du bruit Gaussien/densité
  #-----------------------------
  CVerr=CVbwt(h_gridCV,X,Y)
  h_CVopt=h_gridCV[which.min(CVerr)]
  #-----------------------------
  hCVb <- bw.cv.grid(X = X, Y = Y)
  #-----------------------------
  print(paste0("h_pill : ",h_dpill," h_silver : ", h_silver," h_CV1 ; ",h_CVopt," h_CV2 ; ",hCVb))
  # Application des differentes h
  Y_pill  =locpoly(X,Y,drv=0,degree=2,kernel="normal",bandwidth=h_dpill)
  Y_CVb   =locpoly(X,Y,drv=0,degree=2,kernel="normal",bandwidth=hCVb)
  Y_silver=locpoly(X,Y,drv=0,degree=2,kernel="normal",bandwidth=h_silver)
  Y_CV    =locpoly(X,Y,drv=0,degree=2,kernel="normal",bandwidth=h_CVopt)
  
  # Visualisation : Bien pour HTML, mais probleme de legende
  #=========================================================
      # static_plot = data.frame(X,Y) %>%
      #   ggplot(aes(x=X,y=Y)) + geom_point() + labs(x = "X", y = "Y") +
      #   geom_line(aes(x=Y_pill.x  ,y=r.Y_pill.x. , colour = "red"  )        , size=1, data = data.frame(Y_pill$x,r(Y_pill$x))               ) + 
      #   geom_line(aes(x=Y_CVb.x   ,y=Y_CVb.y     , colour = "darkgoldenrod"), size=1, data = data.frame(Y_CVb$x,Y_CVb$y)       , lty="dashed") +
      #   geom_line(aes(x=Y_pill.x  ,y=Y_pill.y    , colour = "green"  )      , size=1, data = data.frame(Y_pill$x,Y_pill$y)     , lty="dashed") +
      #   geom_line(aes(x=Y_silver.x,y=Y_silver.y  , colour = "blue")         , size=1, data = data.frame(Y_silver$x,Y_silver$y) , lty="dashed") +
      #   geom_line(aes(x=Y_CV.x    ,y=Y_CV.y      , colour = "aquamarine")   , size=1, data = data.frame(Y_CV$x,Y_CV$y)                       ) +
      #   scale_color_discrete(name = "Method", labels = c("True","CV1","pill","Silver","CV2"))
      # plotly::ggplotly(static_plot)
  
  # Visualisation : Bien pour HTML
  #=========================================================
      static_plot = plot_ly(data.frame(X,Y) ,x=~X ,y=~Y ,name="Raw data", type = 'scatter', mode = 'markers') %>% # mode = 'lines+markers'
                    add_trace(data=data.frame(Y_pill$x,r(Y_pill$x)) ,x=~Y_pill.x   ,y=~r.Y_pill.x. , name = 'TRUE'  , mode = 'lines', color=I("red")) %>% 
                    add_trace(data=data.frame(Y_CVb$x,Y_CVb$y)      ,x=~Y_CVb.x    ,y=~Y_CVb.y     , name = 'CV1'   , mode = 'lines', color=I("darkgoldenrod"), linetype=I("dash")) %>% 
                    add_trace(data=data.frame(Y_pill$x,Y_pill$y)    ,x=~Y_pill.x   ,y=~Y_pill.y    , name = 'pill'  , mode = 'lines', color=I("green")        , linetype=I("dash")) %>%  
                    add_trace(data=data.frame(Y_silver$x,Y_silver$y),x=~Y_silver.x ,y=~Y_silver.y  , name = 'Silver', mode = 'lines', color=I("blue")         , linetype=I("dash")) %>% 
                    add_trace(data=data.frame(Y_CV$x,Y_CV$y)        ,x=~Y_CV.x     ,y=~Y_CV.y      , name = 'CV2'   , mode = 'lines', color=I("aquamarine"))
      static_plot
  
  # Visualisation : Bien pour PDF
  #=========================================================  
      # plot(X,Y,pch=20,cex=0.01,xlab="X",ylab="Y")
      # lines(Y_pill$x,r(Y_pill$x),col="red")  
      # lines(Y_CVb,lty="dashed",col="darkgoldenrod")
      # lines(Y_pill,lty="dashed",col="green")
      # lines(Y_silver,lty="dashed",col="blue")
      # lines(Y_CV$x,Y_CV$y,type="l",col="aquamarine")
      # legend("bottomright",legend=c("True","pill","CV1","Silver","CV2"),
      #        col=c("red","green","darkgoldenrod","darkgoldenrod","blue","aquamarine"), lty=1:2, cex=0.8)
}
###############################################################
## QQplot >> ggplot(mtcars, aes(sample=mpg)) + stat_qq()
###############################################################
### REGRESSION : AUTRES FONCTIONS
###############################################################

# Fonction recodée de l'estimateur de NW du cours évalué au point x0
#--------------------------------------------------------------  
est_NW=function(x0,X,Y,h){
  Kh=function(y){1/h*dnorm(y,0,1)}
  NW=mean(Kh(x0-X)*Y)/mean(Kh(x0-X))
  return(NW)
}

# Fonction qui calcule des h adaptatifs avec la fonction dpill dépendant "localement" des valeurs de X
#--------------------------------------------------------------   
# Faire du hlocal: très utile en pratique
# Critique de cette fonction: on sous sample pour calculer h et l'estimateur
# Les performances sont un peu décevantes (du au fait qu'on ait un nombre constant d'observations par bloc?)
# Alternative à tester on partitionne l'axe de x et on fait des blocs en gardant à chaque fois les 
# X_i qui tombent dans chaque block
#--------------------------------------------------------------  
h_CVloc=function(X,Y,N_block){
  #N_block multiple de n
  n=length(X)
  h_loc=rep(-1,N_block)
  if((n/N_block)%%1!=0){ print("error n should be a multiple of N_block")}
  X_sort=sort(X)
  Y_sort=Y[order(X)]
  for(i in 1:N_block){
    h_loc[i]=dpill(X_sort[((i-1)*(n/N_block)+1):(i*(n/N_block))],Y_sort[((i-1)*(n/N_block)+1):(i*(n/N_block))])
  }
  h_locG=rep(h_loc,each=400/N_block)
  return(list("h_loc"=h_loc,"h_locG"=h_locG))
}

# Fonction qui permet de calculer l'estimateur de NW aux points de grid
#--------------------------------------------------------------    
NW_hlocal=function(X,Y,grid){
  M=length(grid)
  Ye=rep(0,M)
  h_loc=rep(0,M)
  for(m in 1:M){
    h_loc[m]=(length(X)/sum(1/((grid[m]-X)**2)))
    #print(h_loc[m])
    Ye[m]=ksmooth(X,Y,kernel="normal",bandwidth=h_loc[m],x.points=grid[m])$y      
  }
  return(list("y"=Ye,"x"=grid,"h_loc"=h_loc))
}
