#######################################################################
# DEGRES DE LIBERTE
#   - modele nul : 1 degre de liberte (intercept)
#   - modele avec K covariables : K+1 degres de liberte
#   - modele sature (0  structure) : N degres de liberte
#######################################################################
# TEST DU CHI2 : on veut une p-value petite (<5%) pour rejeter l'hypothese nulle
#     -------------------------------------------------------------
#   - INDEPENDANCE
#     -------------------------------------------------------------
#       >> nombre-1 : relatif aux autres
#       >> (n-1)+(m-1) degres de liberte VS n*m-1 degres de liberte
#       --------------------
#       ~ CHI2[n.m-1-(n-1+m-1)]
#       ~ CHI2[(n-1).(m-1)]
#       --------------------
#       ex : chisq.test(table(survey$Exer, survey$Smoke))
#     -------------------------------------------------------------
#   - RAPPORT DE VRAISEMBLANCE
#     -------------------------------------------------------------
#       >> H0 : modele m0 imbrique dans un autre m1, avec k0 et k1 degres de liberte
#       >> Si cela est faux, m1 aura des coefficient nuls, et donc moins de degres de liberte que prevu
#       --------------------
#       ~ CHI2[k1-k0]
#       --------------------
#       ex : anova(m1, m2, test = "LRT") % Likehood Ratio Test : entre 2 modeles
#       ex : anova(m1,     test = "LRT") % Likehood Ratio Test : etapes depuis le modele nul vers m1
#     -------------------------------------------------------------
#   - DEVIANCE : generalisation du test du rapport de vraisemblance
#     -------------------------------------------------------------
#       >> utilisation des deviances
#       >> Dsat = 0
#       --------------------
#       Dsat = Dsat - Dsat = 0  ~ CHI2[0]
#       D0   = D0   - Dsat      ~ CHI2[n-1]
#       Dk   = Dk   - Dsat      ~ CHI2[n-k-1]
#              D0   - Dk        ~ CHI2[k]
#       --------------------
#       ex :summary(m1)
#           pchisq(1221.7 - 1087.5, 999 - 996, lower = F)
#           pchisq(1087.5, 996, lower = F)
#######################################################################
# ANALYSE AUTOMATIQUE
#   --------------------
#   - m = step(glm(TARGET ~ COVARIABLES , data = dddd , family = binomial), k = ???? , direction = ????)
#   - k = 2 (AIC, default) :: k = log(n) (BIC) :: plus complique pour les autres
#   - direction = "backward" (default), "forward" , "both"
#   --------------------
#   - a chaque iteration on regarde l'impact de l'ajout d'une covariable ('none' est aussi scannee)
#   - le critere choisi (AIC, BIC, ...) = nb. covariable + n/k*log(carres residuel)
#       >> en changeant k, on donne plus ou moins d'importance au carres residuels par rapport au nb. de covariables
#######################################################################
# INTERPRETATION
#------------------------------------------------------------------
# GLM // STEP :: SUMMARY
#------------------------------------------------------------------
#             Null deviance: 1221.73  on 999  degrees of freedom (modele nul)
#         Residual deviance:  959.32  on 984  degrees of freedom (modele retenu)
#         AIC: 991.32
#       --------------------
#         >> deviance par rapport au modele sature + ecart en degres de liberte
#------------------------------------------------------------------
# ANOVA FISHER (modeles imbriques)
#------------------------------------------------------------------
#         anova(reg3,reg,test="F")
#         --------------------
#         Analysis of Variance Table
#         
#         Model 1: Consommation ~ Puissance + Poids
#         Model 2: Consommation ~ Prix + Cylindree + Puissance + Poids
#         Res.Df    RSS Df Sum of Sq      F Pr(>F)  
#         1     28 21.077                             
#         2     26 17.365  2    3.7118 2.7788 0.0806 .
#           ---
#           Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#        --------------------
#         Res.Df  = nb. degres de liberte par rapport au modele sature
#         Df      = nb. degres de liberte du test
#         F       = valeur prise par l'estimateur de Fisher
#         Pr(>F)  = p-value
#------------------------------------------------------------------
# ANOVA DEVIANCE & VRAISEMBLANCE (modeles imbriques)
#------------------------------------------------------------------
#         anova(m1, m2, test = "LRT")
#         --------------------
#         Analysis of Deviance Table
#         
#         Model 1: Creditability ~ Account.Balance + Telephone + Guarantors
#         Model 2: Creditability ~ Account.Balance + Duration.of.Credit..month. + 
#           Payment.Status.of.Previous.Credit + Credit.Amount + Value.Savings.Stocks + 
#           Length.of.current.employment + Instalment.per.cent + Sex...Marital.Status + 
#           Guarantors + Most.valuable.available.asset + Concurrent.Credits + 
#           Type.of.apartment + No.of.Credits.at.this.Bank + Telephone + 
#           Foreign.Worker
#         Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#         1       996    1087.47                          
#         2       984     959.32 12   128.15 < 2.2e-16 ***
#           ---
#           Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#######################################################################
# CONDA
# conda activate rstudio
# conda env list
# conda list
# conda update -c main -c r --all
# conda install -c r r-base
# R
#######################################################################
# EXPORT PDF : package LateX
# tinytex::install_tinytex()
#######################################################################
# LIBRAIRIES DIVERSES
#   library("FactoMineR")
#   library("factoextra")
#######################################################################
# PROJET GLM 
#------------------------------------------------------------------
# MODELES A EXPLORER
#------------------------------------------------------------------
# Methodes eligibles
#	  - Bernoulli 	: glm // family = binomial
#	  - Probit 	: glm // family = binomial(link = "probit")
# Methodes non utilisees
#	  - Binomial	: glm + cbind // family = binomial
#	  - Multinomiales : multinom
#	  - Cumulatives 	: clm
#	  - Poisson 	: glm // family = binomial
#------------------------------------------------------------------
# AJOUT DE COVARIABLES
#------------------------------------------------------------------
#	  - max-min pour les features concernees
#	  - cos(julianDay)
#	  - sin(julianDay)
#------------------------------------------------------------------
# QUELLES RECHERCHES DE MODELES
#------------------------------------------------------------------
#   - m1 : toutes les covariables
#   - m2 : covariables avec peu de p values
#   - m3 : covariables interessantes aux vues des plots
#   - m4 : step : both + AIC
#   - m5 : step : both + BIC
#------------------------------------------------------------------
# VALIDATIONS CROISEES
#------------------------------------------------------------------
#   - melange des donnees avant creation des sous-groupes, sinon impact de la saison
#   - creation de 10 sous-groupe
#------------------------------------------------------------------
# PLAN DE TRAVAIL
#------------------------------------------------------------------
#   - changement des features
#   - affichage des plots d'interet
#   - choisir les modeles
#   - faire des acp sur les modeles degrades
#   - faire une validation croisee (melange + division en 10 jeux)
#   - presenter les resultats
#######################################################################

## DATA PREPARATION
#######################################################################

# READ

  d = read.csv("/home/tbianchi/RSTUDIO/M2_REGRESSION_LOGISTIQUE/DM/meteo.train.csv")
  summary(d)
  #class(d)

# FEATURES MODIFICATION

  tmp = do.call(paste, list(d$Month, d$Day, d$Year))
  tmp = as.Date(tmp, format=c("%m %d %Y"))
  d$mycosJD <- abs(cos(as.numeric(format(tmp, "%j"))/365*2*pi))
  d$mysinJD <- abs(sin(as.numeric(format(tmp, "%j"))/365*2*pi))
  
# MODELE PERSO
####################################################################### 
  # d$Mean.Sea.Level.Pressure.daily.mean..MSL.
  # d$Mean.Sea.Level.Pressure.daily.max..MSL.
  # d$Mean.Sea.Level.Pressure.daily.min..MSL.
  # d$Total.Cloud.Cover.daily.mean..sfc.
  # d$Sunshine.Duration.daily.sum..sfc.
  # d$Wind.Direction.daily.mean..10.m.above.gnd.
  # d$Wind.Direction.daily.mean..80.m.above.gnd.
  # d$Wind.Direction.daily.mean..900.mb.
  # d$High.Cloud.Cover.daily.max..high.cld.lay.
  # d$mycosJD
  # d$mysinJD

# MODELE PLEIN
####################################################################### 
  m_full = glm(formula = pluie.demain ~ .,
          family = binomial,
          data = d)
  summary(m_full)
  Sboth=step(glm(pluie.demain~1,data=d), direction = "both") 
  fit1 = glm(pluie.demain ~ ., family = binomial, data = d)
  fit2 = glm(pluie.demain ~ 1, family = binomial, data = d)
  BACK=step(fit1,direction="backward")
  FRWD=step(fit2,direction="forward" ,scope=list(upper=fit1,lower=fit2))
  BOTH=step(fit2,direction="both"    ,scope=list(upper=fit1,lower=fit2))
  
# PRODUCE VISUALISATIONS
#######################################################################

# BOXPLOTS & MOSAIC
  # for(i in 1:ncol(d)){
  #   if(length(unique(d[, i])) >= 2 & colnames(d)[i] != "pluie.demain")
  #   {
  #    if(is.factor(d[, i]))
  #     {
  #        mosaicplot(d[, i] ~ d$pluie.demain, col=c("blue", "orange"),main=colnames(d)[i])
  #     }
  #    else
  #     {
  #       boxplot(d[, i] ~ d$pluie.demain, main=colnames(d)[i])
  #     }
  #   }
  # }
  
# FONCTION DE VISUALISATION GGPLOT
  nextInd = function(d,ind,exportOption){
    # 0. Load library
      library(ggplot2)
      library(egg)
      p = ggplot(d, aes_string(x = colnames(d)[ind], fill = "pluie.demain"))
    # 1. Create histogramme
      p = ggplot(d, aes_string(x = colnames(d)[ind], fill = "pluie.demain"))
      hp = p + labs(title = colnames(d)[ind], x = colnames(d)[ind],  y = "Densité empirique",
                    fill = "Pluie Demain", subtitle = "Histogramme de distribution") +
              geom_density(alpha = 0.4) + # Transparency
              guides(fill = guide_legend(override.aes = list(alpha = 1)))
    # 2. Create a box plot (bp) / dot plot (dp)
      p = ggplot(d, aes_string(x = "pluie.demain", y = colnames(d)[ind]))
      bp = p + geom_boxplot(aes_string(color = "pluie.demain"))
      #dp = p + geom_dotplot(aes_string(color = "pluie.demain", fill = "pluie.demain"), binaxis='y', stackdir='center')
    # 3. Display
      figure = ggarrange(hp, bp, ncol = 2, nrow = 1)
    # 4. Export
      if (exportOption){ggsave(sprintf("meteo_feature_%02d.png",ind), plot = figure, width = 11, height = 8)}
    # 5. Update indice
      return(figure)
  }
  for(i in 1:ncol(d)){
    if(length(unique(d[, i])) >= 2 & colnames(d)[i] != "pluie.demain")
    {
      toto = nextInd(d,i,1)
    }
  }
  
#######################################################################
####################################################################### 
# pt = hist(d$Mean.Sea.Level.Pressure.daily.mean..MSL.[d$pluie.demain=="TRUE" ], plot=F, breaks = seq(975,1040,1))  
# pf = hist(d$Mean.Sea.Level.Pressure.daily.mean..MSL.[d$pluie.demain=="FALSE"], plot=F, breaks = seq(975,1040,1))    
# plot( pt, col=rgb(0,0,1,1/4), xlim=c(975,1040),main = "", xlab = "")
# plot( pf, col=rgb(1,0,0,1/4), xlim=c(975,1040),main = "", xlab = "", add=T)
# title(main = "Histogramme de d$Mean.Sea.Level.Pressure.daily.mean..MSL.", xlab = "MEAN PRESSURE : MEAN")
# 
# 
# pt = hist(d$Low.Cloud.Cover.daily.max..low.cld.lay.[d$pluie.demain=="TRUE"  & d$Low.Cloud.Cover.daily.max..low.cld.lay.>95], plot=F, breaks = seq(95,100,.1))  
# pf = hist(d$Low.Cloud.Cover.daily.max..low.cld.lay.[d$pluie.demain=="FALSE" & d$Low.Cloud.Cover.daily.max..low.cld.lay.>95], plot=F, breaks = seq(95,100,.1))    
# plot( pt, col=rgb(0,0,1,1/4), xlim=c(95,100),main = "", xlab = "")
# plot( pf, col=rgb(1,0,0,1/4), xlim=c(95,100),main = "", xlab = "", add=T)
# title(main = "Histogramme de d$Low.Cloud.Cover.daily.max..low.cld.lay.", xlab = "LOW CLOUD COVER : MAX") 
# 
# pt = hist(d$High.Cloud.Cover.daily.max..high.cld.lay.[d$pluie.demain=="TRUE" ], plot=F, breaks = seq(95,100,1))  
# pf = hist(d$High.Cloud.Cover.daily.max..high.cld.lay.[d$pluie.demain=="FALSE"], plot=F, breaks = seq(95,100,1))    
# plot( pt, col=rgb(0,0,1,1/4), xlim=c(0,100),main = "", xlab = "")
# plot( pf, col=rgb(1,0,0,1/4), xlim=c(0,100),main = "", xlab = "", add=T)
# title(main = "Histogramme de d$High.Cloud.Cover.daily.max..low.cld.lay.", xlab = "HIGH CLOUD COVER : MAX") 
# 
# pt = hist(d$Total.Precipitation.daily.sum..sfc.[d$pluie.demain=="TRUE" ], plot=F, breaks = seq(0,42,1)) 
# pf = hist(d$Total.Precipitation.daily.sum..sfc.[d$pluie.demain=="FALSE"], plot=F, breaks = seq(0,42,1))    
# plot( pt, col=rgb(0,0,1,1/4), xlim=c(0,42),main = "", xlab = "")
# plot( pf, col=rgb(1,0,0,1/4), xlim=c(0,42),main = "", xlab = "", add=T)
# title(main = "Histogramme de d$High.Cloud.Cover.daily.max..low.cld.lay.", xlab = "TOTAL PRECIPITATION : SUM") 
# 
# library(ggplot2)
# p =  ggplot(data = d, mapping = aes(x = mycosJD, y = pluie.demain))
# p + geom_point()
# col = sub_diamond$color)
#
# ggplot(iris, aes( y=Sepal.Length,x=Species, fill=Species, colour=Species))+
#   geom_boxplot(alpha=0.5)+
#   geom_jitter(width=0.25)+
#   ggtitle("Mon super premier boxplot")+
#   ylab("Longueur des sépales")+
#   xlab("Espèces")+
#   theme(legend.position="bottom")
#######################################################################
#######################################################################
## ACP
#install.packages("FactoMineR",dependencies=TRUE)
# library('FactoMineR')
# D = d
# C = c(7,8,9,47)
# N = length(C)
# d3 = d[,C]
# res.pca = PCA(D,quali.sup=c(N),graph=FALSE)
# plot(res.pca,habillage=N)
#######################################################################
#######################################################################
## ACP 3d
# library(pca3d)
# D = d
# N = 47
# pca = prcomp(D[,-N],scale.=TRUE)
# gr  = D[,N]
# pca3d(pca,group=gr)
#######################################################################
#######################################################################