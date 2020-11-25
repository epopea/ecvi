#----------Functions-------------------

cevi <- function(data=NULL,
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
    A <- bind_cols("Index"="CEVI",
                   "Estimate"=unlist(x)[1],
                   "SE"=print(x)[1,2])
                   #"95% CI - LB"=unlist(x)[1]-print(x)[1,2]*1.96,
                   #"UB"=unlist(x)[1]+print(x)[1,2]*1.96)
    B <- bind_cols("Index"="H",
                   "Estimate"=attr(x, "extra")[1,1],
                   "SE" = attr(x, "extra")[1,2])
                   #"95% CI - LB"=attr(x, "extra")[1,1] - attr(x, "extra")[1,2]*1.96,
                   #"UB"=attr(x, "extra")[1,1]+attr(x, "extra")[1,2]*1.96)
    C <- bind_cols("Index"="A",
                   "Estimate"=attr(x, "extra")[2,1],
                   "SE" = attr(x, "extra")[2,2])
                   #"95% CI - LB"=attr(x, "extra")[2,1] - attr(x, "extra")[2,2]*1.96,
                   #"UB"=attr(x, "extra")[2,1]+attr(x, "extra")[2,2]*1.96)
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
    cevi <- svyafc(formula = formula,
                   design = data,
                   cutoffs = as.list(cutoffs),
                   subgroup = subgroup,
                   weigth = weigth,
                   dimw=dimw,
                   k = k,
                   g = g,
                   na.rm = na.rm
    )
    indexed <- quiet(temp(cevi))
    output[[1]] <- indexed
  }
  
  if(decompose==TRUE){
    cevi_dec <- svyafcdec(formula = formula,
                          design = data,
                          cutoffs = as.list(cutoffs),
                          subgroup = subgroup,
                          weigth = weigth,
                          dimw=dimw,
                          k = k,
                          g = g,
                          na.rm = na.rm
    )
    decomposed <- quiet(temp2(cevi_dec, ...))
    output[[2]] <- decomposed
  }
  
  if(decomp.complete==TRUE){
    output[[3]] <- cevi_dec
  }
  return(output)
}

# Example of use of the cevi function
#cevi(data= cdata, formula = formula, cutoffs = cutoffs,
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
  cevi <- list()
  sensitivity_list <- list()
  
  for(i in 1:nrow(unique(data[grouping.var]))){
    data[pull(data, grouping.var) == unique(pull(data, grouping.var))[i], ] %>% 
      svydesign(ids = as.formula(paste0("~", ids)),
                weights = as.formula(paste0("~", weights)),
                data = .) %>% 
      convey_prep() -> cdata[[i]]
    for(ks in seq(0.05, 1, 0.05)){
      cevi[[ks*20]] <-
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
          "CEVI Estimate" = coef( cevi[[ks*20]] ) ,
          "CEVI Std. Error" = SE( cevi[[ks*20]] ) ,
          "Headcount Estimate" = attr( cevi[[ks*20]] , "extra" )[ 1 , 1 ] ,
          "Headcount Std Error" = attr( cevi[[ks*20]] , "extra" )[ 1 , 2 ] ,
          "Intensity Estimate" = attr( cevi[[ks*20]] , "extra" )[ 2 , 1 ] ,
          "Intensity Std Error" = attr( cevi[[ks*20]] , "extra" )[ 2 , 2 ]
        )
      } else{
        sensitivity_list[[i]] <- rbind(
          sensitivity_list[[i]] ,
          data.frame( 
            "k" = ks , 
            "CEVI Estimate" = coef( cevi[[ks*20]] ) ,
            "CEVI Std. Error" = SE( cevi[[ks*20]] ) ,
            "Headcount Estimate" = attr( cevi[[ks*20]] , "extra" )[ 1 , 1 ] ,
            "Headcount Std Error" = attr( cevi[[ks*20]] , "extra" )[ 1 , 2 ] ,
            "Intensity Estimate" = attr( cevi[[ks*20]] , "extra" )[ 2 , 1 ] ,
            "Intensity Std Error" = attr( cevi[[ks*20]] , "extra" )[ 2 , 2 ]
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
    cevi_overall <-
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
        "CEVI Estimate" = coef( cevi_overall ) ,
        "CEVI Std. Error" = SE( cevi_overall ) ,
        "Headcount Estimate" = attr( cevi_overall , "extra" )[ 1 , 1 ] ,
        "Headcount Std Error" = attr( cevi_overall , "extra" )[ 1 , 2 ] ,
        "Intensity Estimate" = attr( cevi_overall , "extra" )[ 2 , 1 ] ,
        "Intensity Std Error" = attr( cevi_overall , "extra" )[ 2 , 2 ]
      )
    )
  }
  sensitivity_overall$Intensity.Estimate[is.na(sensitivity_overall$Intensity.Estimate)] <- 0
  sensitivity_overall$Intensity.Std.Error[is.na(sensitivity_overall$Intensity.Std.Error)] <- 0
  
  estimate = NULL
  
  for(i in 1:length(sensitivity_list)){
    estimate = c(estimate, sensitivity_list[[i]]$CEVI.Estimate)
    max_bound <- seq_along(sensitivity_list[[i]]$CEVI.Estimate)[which.min(sensitivity_list[[i]]$CEVI.Estimate) - 1]
  }
  
  vars = as.data.frame(matrix(data = NA, nrow = (max_bound-k), ncol = length(sensitivity_list)))
  for(i in 1:length(sensitivity_list)){
    assign(paste0("vars_", i), NULL)
    for(j in 1:(max_bound - k)){
      assign(paste0("vars_", i), c(get(paste0("vars_", i)), var(sensitivity_list[[i]]$CEVI.Estimate[j:(j+k)])))
    }
    vars[, i] <- get(paste0("vars_", i))
  }
  #x <- as.data.frame(matrix(data = NA, nrow = (max_bound-4), ncol = length(sensitivity_list)))
  
  #for(i in 1:length(sensitivity_list)){
  #  for(j in 1:(max_bound-4)){
  #    x[j, i] <- sensitivity_list[[i]]$CEVI.Estimate[j]
  #  }
  #}
  #diffs <- as.data.frame(matrix(data = NA, nrow = (max_bound-4), ncol = (length(sensitivity_list) -1)))
  #for(i in 1:(max_bound-4)){
  #  x[i, ] <- sort(x[i, ], decreasing = TRUE)
  #  for(j in 1:(length(sensitivity_list) - 1)){
  #    diffs[i, j] <- x[i, j] - x[i, j+1]
  #  }
  #}
  
  #means0 = rowSums(diffs)/ncol(diffs)
  means1 = rowSums(vars)/ncol(vars)
  #means = vector(length = max_bound-4)
  #for(i in 1:length(means)){
  #  means[i] = mean(means0[i], means1[i])
  #}
  #best_k_reg = seq(0.05, 1, 0.05)[which.min(means)]
  best_k_reg = seq(0.05, 1, 0.05)[which.min(means1)]
  min = min(estimate, na.rm = T)
  max = max(estimate, na.rm = T)
  
  vars_overall = as.data.frame(matrix(data = NA,
                                      nrow = seq_along(sensitivity_overall$CEVI.Estimate)[which.min(sensitivity_overall$CEVI.Estimate) - 1] - k,
                                      ncol = 3))
  
  for(j in 1:nrow(vars_overall)){
    vars_overall[j, 1] <- var(sensitivity_overall$CEVI.Estimate[j:(j+k)])
    vars_overall[j, 2] <- var(sensitivity_overall$Headcount.Estimate[j:(j+k)])
    vars_overall[j, 3] <- var(sensitivity_overall$Intensity.Estimate[j:(j+k)])
  }
  
  
  #diffs_overall <- as.data.frame(matrix(data = NA,
  #                                      nrow = seq_along(sensitivity_overall$CEVI.Estimate)[which.min(sensitivity_overall$CEVI.Estimate) - 1] - 4,
  #                                      ncol = 2))
  #for(j in 1:nrow(vars_overall)){
  #  x <- sort(c(sensitivity_overall$CEVI.Estimate[j], 
  #              sensitivity_overall$Headcount.Estimate[j],
  #              sensitivity_overall$Intensity.Estimate[j]))
  #  diffs_overall[j, ] <- c(x[3]-x[2], x[2] - x[1])
  #}
  #means_diffs_overall = rowSums(diffs_overall)/2
  means_vars_overall = rowSums(vars_overall)/ncol(vars)
  #means_overall = vector(length = nrow(vars_overall))
  #for(i in 1:nrow(vars_overall)){
  #  means_overall[i] <- mean(means_diffs_overall[i], means_vars_overall[i])
  #}
  #best_k_ovr = seq(0.05, 1, 0.05)[which.min(means_overall)]
  best_k_ovr = seq(0.05, 1, 0.05)[which.min(means_vars_overall)]
  
  if(dominance == TRUE){
    tiff(paste("dominance.tiff", sep = ""), width =6, height = 4,
         units="in", res=300)
    plot(x=sensitivity_list[[1]]$k,
         y=sensitivity_list[[1]]$CEVI.Estimate, type="l",col=1,lwd=2,
         main="Sensitivity Analysis of the Deprivation Cuttoff\nfor CEVI across Macroregions",
         ylab="Index Estimate",
         ylim=c(min, max),
         xlab="Cuttoff (k)",
         xaxt="n")
    for(i in 2:length(sensitivity_list)){
      lines(x=sensitivity_list[[i]]$k,y=sensitivity_list[[i]]$CEVI.Estimate, type="l", col=i,lwd=2)
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
         y=sensitivity_overall$CEVI.Estimate, type="l",col="blue",lwd=2,
         #main="Sensitivity Analysis of the Deprivation Cuttoff",
         ylab="Index Value",
         xaxt="n",
         ylim=c(min(sensitivity_overall$Intensity.Estimate,
                    sensitivity_overall$Headcount.Estimate,
                    sensitivity_overall$CEVI.Estimate, na.rm = TRUE),
                max(sensitivity_overall$Intensity.Estimate,
                    sensitivity_overall$Headcount.Estimate,
                    sensitivity_overall$CEVI.Estimate, na.rm = TRUE)),
         xlab="Cuttoff (k)")
    axis(1, at=seq(0,1,0.05),las=2)
    lines(x=sensitivity_overall$k,y=sensitivity_overall$Headcount.Estimate, type="l", col="red",lwd=2)
    lines(x=sensitivity_overall$k,y=sensitivity_overall$Intensity.Estimate, type="l",col="green",lwd=2)
    legend("topright",legend=c("CEVI","Censored Headcount","Vulnerability Intensity"),
           col=c("blue","red","green"),lwd=rep(2,3), cex=0.7,
           bty='n')
    abline(v=c(best_k_ovr,best_k_ovr+0.05),lty=c(1,2))
    #arrows(x0 = 0.35,y0 = 0.2,x1 = 0.275, y1=0.09, lwd=2)
    #text(x= 0.5, y= 0.21, "Stabilizing region")
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

