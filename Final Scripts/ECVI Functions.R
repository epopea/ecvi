#----------Functions-------------------

ecvi <- function(data=NULL,
                 formula=NULL,
                 subgroup=NULL,
                 cutoffs=NULL,
                 weight=NULL,
                 dimw=NULL,
                 k=NULL,
                 g=0,
                 na.rm=TRUE,
                 index=TRUE,
                 decompose=TRUE,
                 decomp.complete=TRUE,
                 ...){
  myPackages <- c("convey", "survey")
  new.packages <- myPackages[!(myPackages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(myPackages, require, character.only = TRUE)
  
  output <- list(Index="",
                 Decomposition="",
                 Detailed.Decomposition="")
  
  temp <- function(x){
    A <- bind_cols("Index"="ECVI",
                   "Estimate"=unlist(x)[1],
                   "SE"=print(x)[1,2])
                   
    B <- bind_cols("Index"="H",
                   "Estimate"=attr(x, "extra")[1,1],
                   "SE" = attr(x, "extra")[1,2])
                   
    C <- bind_cols("Index"="A",
                   "Estimate"=attr(x, "extra")[2,1],
                   "SE" = attr(x, "extra")[2,2])
                   
    z <- bind_rows(A,
                   B,
                   C)
    
    return(z)
  }
  
  temp2 <- function(x,length=NULL){
    A = bind_cols("Exposure"=round(sum(x$`percentual contribution per dimension`[1:length[1]])*100,2),
                  "Suceptibility"=round(sum(x$`percentual contribution per dimension`[sum(length[1],1):sum(length[1],length[2])])*100,2),
                  "Adaptive Capacity"=round(sum(x$`percentual contribution per dimension`[sum(length[1],length[2],1):sum(length[1],length[2],length[3])])*100,2))
    
    return(A)
  }
  
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  if(index==TRUE){
    ecvi <- svyafc(formula = formula,
                   design = data,
                   cutoffs = as.list(cutoffs),
                   subgroup = subgroup,
                   weigth = weigth,
                   dimw=dimw,
                   k = k,
                   g = g,
                   na.rm = na.rm
    )
    indexed <- quiet(temp(ecvi))
    output[[1]] <- indexed
  }
  
  if(decompose==TRUE){
    ecvi_dec <- svyafcdec(formula = formula,
                          design = data,
                          cutoffs = as.list(cutoffs),
                          subgroup = subgroup,
                          weigth = weigth,
                          dimw=dimw,
                          k = k,
                          g = g,
                          na.rm = na.rm
    )
    decomposed <- quiet(temp2(ecvi_dec, ...))
    output[[2]] <- decomposed
  }
  
  if(decomp.complete==TRUE){
    output[[3]] <- ecvi_dec
  }
  return(output)
}

# Example of use of the ecvi function
#ecvi(data= cdata, formula = formula, cutoffs = cutoffs,
#     subgroup = ~Regiao, k= .2, g= 0, decompose = TRUE, 
#     index = TRUE, length = c(7, 4, 4))

sensitivity <- function(data=NULL,
                        grouping.var=NULL,
                        weights = 1,
                        ids = 1,
                        formula = formula,
                        cutoffs = NULL,
                        dimw=NULL,
                        k=NULL,
                        g=0,
                        na.rm=TRUE,
                        sensitivity=TRUE,
                        sensitivity.plot=TRUE,
                        dominance=TRUE,
                        ...){ # passar argumentos do stargazer
  
  
  myPackages <- c("convey", "survey", "stargazer", "dplyr")
  new.packages <- myPackages[!(myPackages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(myPackages, require, character.only = TRUE)
  
  
  cdata <- list()
  ecvi <- list()
  sensitivity_list <- list()
  
  for(i in 1:nrow(unique(data[grouping.var]))){
    cdata[[i]] <- data[pull(data, grouping.var) == unique(pull(data, grouping.var))[i], ] %>% 
      svydesign(ids = as.formula(paste0("~", ids)),
                weights = as.formula(paste0("~", weights)),
                data = .) %>% 
      convey_prep()
    for(ks in seq(0.05, 1, 0.05)){
      ecvi[[ks*20]] <-
        svyafc(
          formula,
          design = cdata[[i]],
          cutoffs = as.list(cutoffs),
          subgroup = as.formula(paste0("~", grouping.var)),
          dimw = dimw,
          k = ks, g = g,
          na.rm = na.rm
        )
      if(ks == 0.05){
        sensitivity_list[[i]] <- data.frame( 
          "k" = ks , 
          "ECVI Estimate" = coef( ecvi[[ks*20]] ) ,
          "ECVI Std. Error" = SE( ecvi[[ks*20]] ) ,
          "Headcount Estimate" = attr( ecvi[[ks*20]] , "extra" )[ 1 , 1 ] ,
          "Headcount Std Error" = attr( ecvi[[ks*20]] , "extra" )[ 1 , 2 ] ,
          "Intensity Estimate" = attr( ecvi[[ks*20]] , "extra" )[ 2 , 1 ] ,
          "Intensity Std Error" = attr( ecvi[[ks*20]] , "extra" )[ 2 , 2 ]
        )
      } else{
        sensitivity_list[[i]] <- rbind(
          sensitivity_list[[i]] ,
          data.frame( 
            "k" = ks , 
            "ECVI Estimate" = coef( ecvi[[ks*20]] ) ,
            "ECVI Std. Error" = SE( ecvi[[ks*20]] ) ,
            "Headcount Estimate" = attr( ecvi[[ks*20]] , "extra" )[ 1 , 1 ] ,
            "Headcount Std Error" = attr( ecvi[[ks*20]] , "extra" )[ 1 , 2 ] ,
            "Intensity Estimate" = attr( ecvi[[ks*20]] , "extra" )[ 2 , 1 ] ,
            "Intensity Std Error" = attr( ecvi[[ks*20]] , "extra" )[ 2 , 2 ]
          )
        )
      }
      
      
    }
    
    sensitivity_list[[i]]$Intensity.Estimate[is.na(sensitivity_list[[i]]$Intensity.Estimate)] <- 0
    sensitivity_list[[i]]$Intensity.Std.Error[is.na(sensitivity_list[[i]]$Intensity.Std.Error)] <- 0
    
  }
  
  overall_data <- data %>% 
    svydesign(ids = as.formula(paste0("~", ids)),
              weights = as.formula(paste0("~", weights)),
              data = .) %>% 
    convey_prep()
  sensitivity_overall <- NULL
  for(ks in seq(0.05, 1, 0.05)){
    ecvi_overall <-
      svyafc(
        formula,
        design = overall_data,
        cutoffs = as.list(cutoffs),
        subgroup = as.formula(paste0("~factor(", grouping.var, ")")),
        dimw = dimw,
        k = ks, g = g,
        na.rm = na.rm
      )
    
    sensitivity_overall <- rbind(
      sensitivity_overall ,
      data.frame( 
        "k" = ks , 
        "ECVI Estimate" = coef( ecvi_overall ) ,
        "ECVI Std. Error" = SE( ecvi_overall ) ,
        "Headcount Estimate" = attr( ecvi_overall , "extra" )[ 1 , 1 ] ,
        "Headcount Std Error" = attr( ecvi_overall , "extra" )[ 1 , 2 ] ,
        "Intensity Estimate" = attr( ecvi_overall , "extra" )[ 2 , 1 ] ,
        "Intensity Std Error" = attr( ecvi_overall , "extra" )[ 2 , 2 ]
      )
    )
  }
  sensitivity_overall$Intensity.Estimate[is.na(sensitivity_overall$Intensity.Estimate)] <- 0
  sensitivity_overall$Intensity.Std.Error[is.na(sensitivity_overall$Intensity.Std.Error)] <- 0
  
  estimate = NULL
  
  for(i in 1:length(sensitivity_list)){
    estimate = c(estimate, sensitivity_list[[i]]$ECVI.Estimate)
    max_bound <- seq_along(sensitivity_list[[i]]$ECVI.Estimate)[which.min(sensitivity_list[[i]]$ECVI.Estimate) - 1]
  }
  
  vars = as.data.frame(matrix(data = NA, nrow = (max_bound-k), ncol = length(sensitivity_list)))
  for(i in 1:length(sensitivity_list)){
    assign(paste0("vars_", i), NULL)
    for(j in 1:(max_bound - k)){
      assign(paste0("vars_", i), c(get(paste0("vars_", i)), var(sensitivity_list[[i]]$ECVI.Estimate[j:(j+k)])))
    }
    vars[, i] <- get(paste0("vars_", i))
  }
  
  means1 = rowSums(vars)/ncol(vars)
  
  best_k_reg = seq(0.05, 1, 0.05)[which.min(means1)]
  min = min(estimate, na.rm = T)
  max = max(estimate, na.rm = T)
  
  vars_overall = as.data.frame(matrix(data = NA,
                                      nrow = seq_along(sensitivity_overall$ECVI.Estimate)[which.min(sensitivity_overall$ECVI.Estimate) - 1] - k,
                                      ncol = 3))
  
  for(j in 1:nrow(vars_overall)){
    vars_overall[j, 1] <- var(sensitivity_overall$ECVI.Estimate[j:(j+k)])
    vars_overall[j, 2] <- var(sensitivity_overall$Headcount.Estimate[j:(j+k)])
    vars_overall[j, 3] <- var(sensitivity_overall$Intensity.Estimate[j:(j+k)])
  }
  
  
  
  means_vars_overall = rowSums(vars_overall)/ncol(vars)
  
  best_k_ovr = seq(0.05, 1, 0.05)[which.min(means_vars_overall)]
  
  if(dominance == TRUE){
    tiff(paste("dominance.tiff", sep = ""), width =6, height = 4,
         units="in", res=300)
    plot(x=sensitivity_list[[1]]$k,
         y=sensitivity_list[[1]]$ECVI.Estimate, type="l",col=1,lwd=2,
         main="Sensitivity Analysis of the Deprivation Cuttoff\nfor ECVI across Macroregions",
         ylab="Index Estimate",
         ylim=c(min, max),
         xlab="Cuttoff (k)",
         xaxt="n")
    for(i in 2:length(sensitivity_list)){
      lines(x=sensitivity_list[[i]]$k,y=sensitivity_list[[i]]$ECVI.Estimate, type="l", col=i,lwd=2)
    }
    axis(1, at=seq(0,1,0.05),las=2)
    legend("topright",legend= unique(pull(data, grouping.var)),
           col=c(1:length(sensitivity_list)),lwd=rep(2,length(sensitivity_list)))
    abline(v=c(best_k_reg, best_k_reg+0.05),lty=c(1,1), lwd=1.5)
    dev.off()
    cat("\n A file named dominance.tiff has been saved in your working directory")
  }
  
  if(sensitivity.plot == TRUE){
    tiff("sensitivity.tiff", width = 6, height = 4,
         units="in", res=300)
    plot(x=sensitivity_overall$k,
         y=sensitivity_overall$ECVI.Estimate, type="l",col="black",lwd=2,
         #main="Sensitivity Analysis of the Deprivation Cuttoff",
         ylab="Index Value",
         xaxt="n",
         ylim=c(min(sensitivity_overall$Intensity.Estimate,
                    sensitivity_overall$Headcount.Estimate,
                    sensitivity_overall$ECVI.Estimate, na.rm = TRUE),
                max(sensitivity_overall$Intensity.Estimate,
                    sensitivity_overall$Headcount.Estimate,
                    sensitivity_overall$ECVI.Estimate, na.rm = TRUE)),
         xlab="Cuttoff (k)")
    axis(1, at=seq(0,1,0.05),las=2)
    lines(x=sensitivity_overall$k,y=sensitivity_overall$Headcount.Estimate, type="l", col="grey40",lwd=2)
    lines(x=sensitivity_overall$k,y=sensitivity_overall$Intensity.Estimate, type="l",col="grey70",lwd=2)
    legend("topright",legend=c("ECVI","Censored Headcount","Vulnerability Intensity"),
           col=c("black","grey40","grey70"),lwd=rep(2,3), cex=0.7,
           bty='n')
    abline(v=c(best_k_ovr,best_k_ovr+0.05),lty=c(1,2))
    
    dev.off()
    cat("\n A file named sensitivity.tiff has been saved in your working directory")
  }
  
  if(sensitivity == TRUE){
    cat("\n Vulnerability estimates by varying cutoff levels - Overall study area, Brazil, 2010")
    stargazer::stargazer(sensitivity_overall, ...)
    for(i in 1:length(sensitivity_list)){
      cat(paste0("\n Vulnerability estimates by varying cutoff levels - ",
                 unique(data[grouping.var])[i,1],
                 ", Brazil, 2010"))
      stargazer::stargazer(sensitivity_list[[i]], ... )
    }
  }
  
}

# Example of use of the sensitivity function
#sensitivity(data = data, grouping.var = as.name("Regiao"), weights = 1,ids = 1, 
#            formula = ~ TXxd + TNxd + TX90pd + TN90pd + DTRd + Cddd + R99pd +
#              elderly + children + pcincome + literate +
#              sewage + water + garbage + urb,
#            cutoffs = c(expcut, suscut, adapcut),
#            dimw = c(rep(0.04761905,7),rep(0.08333333,8)),
#            g = 0,
#            na.rm = T, sensitivity = FALSE, sensitivity.plot = TRUE, dominance = TRUE, summary = FALSE, type = "text")

