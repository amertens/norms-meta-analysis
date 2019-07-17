

#hbgdki pallets
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

rma_summary <- function(fit, cat){
  if(!is.null(fit)){
    res<-data.frame(cat=cat, est=fit$b, se=fit$se, ci.lb=fit$ci.lb, ci.ub= fit$ci.ub, pval=fit$pval, tau2=fit$tau2)
  }else{
    res<-NULL
  }
  return(res)
}

rma_robust <- function(data, yi, sei, method="REML", measure){
  fit<-NULL
  if(length(is.na(data[[yi]])) > 1){
    if(method=="FE"){
      fit<-rma(yi=data[[yi]], sei=data[[sei]], method="FE", measure="GEN")
    }else{
      try(fit<-rma(yi=data[[yi]], sei=data[[sei]], method="REML", measure="GEN"))
      if(is.null(fit)){try(fit<-rma(yi=data[[yi]], sei=data[[sei]], method="ML", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=data[[yi]], sei=data[[sei]], method="HE", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=data[[yi]], sei=data[[sei]], method="DL", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=data[[yi]], sei=data[[sei]], method="EB", measure="GEN"))}
      if(is.null(fit)){try(fit<-rma(yi=data[[yi]], sei=data[[sei]], method="SJ", measure="GEN"))}
    }
  }
  return(fit)
}



meta_clean <- function(d, strat=T, pool_method="REML"){
  
  d <- d %>% filter(!is.na(est))
  
  #strip carrot from p-value
  d$pval <- gsub("^","",d$pval)
  
  
  #Update calculation of se from sd:
  #https://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean
  
  #Grab sd if no se, grab pval if no sd or se
  table(d$pcat)
  d <- d %>%
    mutate(se=NA,
           se=as.numeric(se),
           sd=as.numeric(sd),
           se_raw=se,
           se = ifelse(is.na(se), sd/sqrt(n), se),
           se = ifelse(is.na(se) & !is.na(tstat), est/tstat, se),
           pval_cat = case_when(pcat == "***" ~ 0.0005,
                            pcat == "**"  ~ 0.0055,
                            pcat == "*"  ~ 0.03,
                            pcat == "ps"  ~ 0.075,
                            pcat == "n.s"  ~ 0.525,
                            pcat == "ns"  ~ 0.525),
           pval = ifelse(is.na(pval), pval_cat, as.numeric(pval)),
           pval_se = abs(est)/abs(qnorm(pval)),
           se = ifelse(is.na(se), pval_se, se))
  summary(d$se)
  summary(d$pval_se)
  
  #Compare reported vs. pvalue calculates SE where possible
  # dcomp <- d %>% filter(!is.na(se_raw) & !is.na(pval_se)) %>% select(se_raw, pval_se)
  # dcomp
  
  #Pool estimates
  if(strat){
    res <- NULL
    stratcats<-unique(d$normcat)
    
    for(i in 1:length(stratcats)){
      
      dsub <- d[d$normcat==stratcats[i],]
      
      fit <- rma_robust(data=dsub, yi="est", sei="se", method=pool_method, measure="GEN")
      
      dsub <- dsub %>% group_by(cat) %>% mutate(N=n()) %>% ungroup() %>% filter(N>3)

      fit_FFA<-fit_GC<-fit_HC<-fit_CA<-fit_EPC<-NULL
      
      try(fit_FFA<-rma_robust(data=dsub[dsub$cat=="FFA" & !is.na(dsub$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
      try(fit_GC<-rma_robust(data=dsub[dsub$cat=="Green consumerism" & !is.na(dsub$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
      try(fit_HC<-rma_robust(data=dsub[dsub$cat=="Household conservation" & !is.na(dsub$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
      try(fit_CA<-rma_robust(data=dsub[dsub$cat=="Collective Action" & !is.na(dsub$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
      try(fit_EPC<-rma_robust(data=dsub[dsub$cat=="Everyday Public Conservation" & !is.na(dsub$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
      
      
      restemp <- rbind(
        rma_summary(fit, "Overall"),
        rma_summary(fit_FFA, "FFA"),
        rma_summary(fit_GC, "Green consumerism"),
        rma_summary(fit_HC, "Household conservation"),
        rma_summary(fit_CA, "Collective action"),
        rma_summary(fit_EPC, "Everyday Public Conservation")
      )
      restemp$normcat = stratcats[i]
      res <- rbind(res, restemp)
    }
    
    #Create plotting data.frames
    study_res <- d %>% mutate(
      ci.lb = est - 1.96 * se,
      ci.ub = est + 1.96 * se) %>%
      select(cat,normcat, est,ci.lb,ci.ub,pval, study) %>%
      filter(cat %in% c("FFA", "Green consumerism", "Household conservation", "Collective Action", "Everyday Public Conservation"))
    res$study <- "pooled"
    study_res_pooled <- study_res %>% mutate(cat="Overall")
    
    df <- bind_rows(res, study_res, study_res_pooled)
    
    df <- df %>% group_by(normcat, cat) %>% mutate(N=n()) %>% ungroup() 
    
  }else{
    
    fit<-rma_robust(data=d, yi="est", sei="se", method=pool_method, measure="GEN")
  
    fit_FFA<-fit_GC<-fit_HC<-fit_CA<-fit_EPC<-NULL
    
    try(fit_FFA<-rma_robust(data=d[d$cat=="FFA" & !is.na(d$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
    try(fit_GC<-rma_robust(data=d[d$cat=="Green consumerism" & !is.na(d$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
    try(fit_HC<-rma_robust(data=d[d$cat=="Household conservation" & !is.na(d$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
    try(fit_CA<-rma_robust(data=d[d$cat=="Collective Action" & !is.na(d$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
    try(fit_EPC<-rma_robust(data=d[d$cat=="Everyday Public Conservation" & !is.na(d$cat),], yi="est", sei="se", method=pool_method, measure="GEN"))
    
  
    res <- rbind(
      rma_summary(fit, "Overall"),
      rma_summary(fit_FFA, "FFA"),
      rma_summary(fit_GC, "Green consumerism"),
      rma_summary(fit_HC, "Household conservation"),
      rma_summary(fit_CA, "Collective action"),
      rma_summary(fit_EPC, "Everyday Public Conservation")
    )
    
    #Create plotting data.frames
    study_res <- d %>% mutate(
      ci.lb = est - 1.96 * se,
      ci.ub = est + 1.96 * se) %>%
      select(cat,est,ci.lb,ci.ub,pval, study) %>%
      filter(cat %in% c("FFA", "Green consumerism", "Household conservation", "Collective Action", "Everyday Public Conservation")) 
    
    
    res$study <- "pooled"
    study_res_pooled <- study_res %>% mutate(cat="Overall")
    
    df <- bind_rows(res, study_res, study_res_pooled)
  
    
    df <- df %>% group_by(cat) %>% mutate(N=n()) %>% ungroup() %>% filter(N>3)
    
  }
  
  


  df$pooled <- ifelse(df$study=="pooled", "pooled","study")
  df <- df %>% arrange(pooled, est)
  df$study_num <-as.numeric(factor(df$study, levels=unique(df$study)))
  df$study <- factor(df$study, levels=unique(df$study))
  df$study_num <- factor(df$study_num, levels=unique(df$study_num))
  df$pooled_label <- ifelse(df$pooled=="pooled",as.character(round(df$est,2)),"")
  
  return(list(fullres=res, plotdf=df))
}
  
  

#Forest plot function

forest_plot <- function(df, title="Subjective Norms", strat=T){
  p <- ggplot(df, aes(x=study_num)) + 
    geom_point(aes(y=est,  color=pooled), size = 4) +
    geom_linerange(aes(ymin=ci.lb, ymax=ci.ub, color=pooled)) +
    coord_flip(ylim=c(-1, 1)) +
    labs(x = "", y = "Standardized coefficient") +
    geom_hline(yintercept = 0) +
    geom_text(aes(y=est+0.15, label=pooled_label)) +
    scale_shape_manual(values=c(21, 23)) +
    scale_colour_manual(values=tableau10, name = "Pooled", drop=F) +
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    ) +
    ggtitle(title) + 
    guides(color=FALSE, shape=FALSE) 
  
  if(strat){
    p <- p + facet_wrap(cat~normcat, scales="free", nrow=2)
  }else{
    p <- p + facet_wrap(~cat, scales="free", nrow=2)
  }
  
  return(p)
}






#Funnel plots
# funnel(fit_FFA, main="FFA")
# funnel(fit_GC, main="GC")
# funnel(fit_HC, main="HC")
# 
# funnel(fit_FFA_sens, main="FFA")
# funnel(fit_GC_sens, main="GC")
# funnel(fit_HC_sens, main="HC")




# #Sensitivity analysis - only use recorded SE
# fit_FFA_sens<-rma(data=d[d$cat=="FFA" & !is.na(d$cat),], yi=est, sei=se_raw, method="REML", measure="GEN")
# fit_GC_sens<-rma(data=d[d$cat=="Green consumerism" & !is.na(d$cat),], yi=est, sei=se_raw, method="REML", measure="GEN")
# fit_HC_sens<-rma(data=d[d$cat=="Household conservation" & !is.na(d$cat),], yi=est, sei=se_raw, method="REML", measure="GEN")
# 
# #Sensitivity analysis - FE
# fit_FFA_FE<-rma(data=d[d$cat=="FFA" & !is.na(d$cat),], yi=est, sei=se, method="FE", measure="GEN")
# fit_GC_FE<-rma(data=d[d$cat=="Green consumerism" & !is.na(d$cat),], yi=est, sei=se, method="FE", measure="GEN")
# fit_HC_FE<-rma(data=d[d$cat=="Household conservation" & !is.na(d$cat),], yi=est, sei=se, method="FE", measure="GEN")
# 
# fit_FFA_FE_sens<-rma(data=d[d$cat=="FFA" & !is.na(d$cat),], yi=est, sei=se_raw, method="FE", measure="GEN")
# fit_GC_FE_sens<-rma(data=d[d$cat=="Green consumerism" & !is.na(d$cat),], yi=est, sei=se_raw, method="FE", measure="GEN")
# fit_HC_FE_sens<-rma(data=d[d$cat=="Household conservation" & !is.na(d$cat),], yi=est, sei=se_raw, method="FE", measure="GEN")
