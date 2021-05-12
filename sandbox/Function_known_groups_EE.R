
#general known groups validity function
#created 8.14.2020 Ella


#WARNING: uses 3 of Charlie/Ella's functions. must make sure the filepaths are
#         specified correctly before using this function



#ARGUMENTS:
#dat: a df that has USUBJID, PRO score, and known groups columns
#levels: a char vector that specifies the order of the known groups
#contrasts: specifies what type of contrasts you want. this function will only take one of the following 2 args:
#       "pairwise" - pairs(, reverse=T) emmeans function 
#       "trt.vs.ctrl" (default) - contr argument for contrast() emmeans func
#mult_adj: specifies if you want to adjust p vals and confints for multiplicity
#       T (default) - use the multiplicity adjustment (dunnett for trt.vs.ctrl and tukey for pairwise)
#       F - do not adjust 
#output_file = T/F: whether results should be output to word file instead of as df
#file.name, print.dir, table.title: args to pass for outputting to word file, if that option is chosen


#OUTPUT:
#for each known group: emmeans, n, difference in emmean from ref group, and confints/p-vals for the differences
#omega sq effect size for the model






known_groups_EE <- function(dat,
                            PRO_score,
                            group,
                            levels, 
                            contr  = "trt.vs.ctrl",
                            mult_adj = F,
                            output_file  = F,
                            file.name = NULL,#args to pass to Charlie's dump_df_mat_to_file function
                            print.dir = NULL, 
                            table.title = NULL,
                            NA.string = '-', 
                            footnotes = NULL){
  
  library(emmeans)
  
  
  #groups are factors in the correct order, with reference group listed as first one in levels
  dat[,group] <- factor(dat[,group], levels = levels, ordered=F)
  
  
  #fit linear model to the PRO scores with known groups as predictors 
  lm<- lm(as.formula(paste0(PRO_score, '~', group)), data = dat)
  
  
  #get emmeans
  emms <- emmeans(lm, specs = group, calc = c(n = ~.wgt.))
  
  
  #get values for output, depending on what contrasts and adjustment specified
  if (contr == "trt.vs.ctrl") {
    
    contr.est <- "Difference from Reference" #name what the contrast is
    
    #get the contrasts and null hypothesis based on whether a multiplicity adjustment is specified
    if (mult_adj) {
      diffs<- contrast(emms, "trt.vs.ctrl", infer = c(T,T)) 
      null.hyp <- "the effect groups are not different from the reference group. Multiplicity adjustments made using Dunnett method."
      
    } else {
      diffs<- contrast(emms, "trt.vs.ctrl", infer = c(T,T), adjust = "none")
      null.hyp <- "the effect group is not different from the reference group. Not adjusted for multiplicity."
    }
    
    #get the output from emms and diffs
    out<- summary(emms)[,c(group,"n","emmean")]
    out[,c('diff', 'lower.CL', 'upper.CL', 'p.val')]<-
      rbind.data.frame( c(NA,NA,NA,NA),summary(diffs)[,c("estimate","lower.CL", "upper.CL", "p.value")] )
    colnames(out)<-c('Group','N', 'Group Mean',contr.est,'Lower','Upper','P-value')

    
    #end trt vs control contr code#####
    
    
    
    
    
  } else if (contr == "pairwise") {
    
    contr.est <- "Difference Between Groups" #name what the contrast is
    
    #get contrast and null hyp depending on whether you adjust for multiplicity
    if (mult_adj){
      pairs<- pairs(emms, infer = c(T,T), reverse=T)
      null.hyp <- "all of the groups are not different from eachother. Multiplicity adjustments made using Tukey HSD method."
      
    } else {
      pairs<- pairs(emms, infer = c(T,T), reverse=T, adjust = "none")
      null.hyp <- "the two groups are not different. Not adjusted for multiplicity."
      
    }
    
    #get output from emms and pairs
    out<- summary(emms)[,c(group,"n","emmean")]
    numExtraRows <- length(summary(pairs(emms))$contrast) - length(levels(dat[,group]))
    if (numExtraRows>0) {
      for (row in 1: numExtraRows) {out<- rbind.data.frame(out, c(NA,NA,NA))}
    } 
    out[,c("contrast","estimate","lower.CL", "upper.CL", "p.value")] <-
      summary(pairs)[,c("contrast","estimate","lower.CL", "upper.CL", "p.value")]
    colnames(out)<-c('Group','N','Group Mean','Groups Compared',contr.est,'Lower','Upper','P-value')
 
    
    #end pairwise code#######
    
    
    
    
    
  } else {
    
    #contr argument was not specified correctly
    return("known_groups_EE will only accept contr = trt.vs.ctrl or pairwise")
    
  } #end of getting values, depending on what type of contrast

  
  
  
  #get omega squared effect size
  contrasts(dat[,group]) <- contr.sum(length(levels(dat[,group])))
  lm2<- lm(as.formula(paste0(PRO_score, '~', group)), data = dat)
  source("C:/Users/eengstrom/OneDrive - Pharmerit International/Documents/R/R Code Library/Function_omega_sp.R")
  out$omega.sq <- NA
  out <- rbind.data.frame(out, c(rep(NA,  length(out[1,])-1), omega_sp(lm2, all=F)))
  colnames(out) <- 
    sub(pattern = "omega.sq", replacement = "Model Semi-Partial Omega Squared", x= colnames(out))
  
  
  

  #output to word file if asked:
  if (output_file) {
    
    #output to file
    
    
    #load our functions
    source("C:/Users/eengstrom/OneDrive - Pharmerit International/Documents/R/R Code Library/Function_format_low_up.R")
    source("C:/Users/eengstrom/OneDrive - Pharmerit International/Documents/R/R Code Library/Function_dump_df_mat_to_file_RTF.R")
    source("C:/Users/eengstrom/OneDrive - Pharmerit International/Documents/R/R Code Library/Function_format_pVals.R")

    #format the change estimates and confidence intervals as one "est (lower, upper)" character string column
    out <- format_low_up(out,contr.est,'Lower','Upper',2, 
                         NA.string=NA.string, rename =T, 
                         newName = paste0(contr.est, " (95% CI)"))
    
    #format p-values
    out <- format_pVals(df=out, pName = 'P-value',decimals = 3,NA.string=NA.string)
    
    #default footnote
    if (is.null(footnotes)) {
      footnotes <- sprintf(paste0(
        "*Scores have a range of ",
        as.character(min(dat[, PRO_score] ,na.rm=T)),
        " to ",
        as.character(max(dat[, PRO_score] ,na.rm=T)),
        "\n", 
        "P-value  and confidence intervals correspond to null hypothesis that ",
        null.hyp))
    }
    
    
    #default title
    if (is.null(table.title)) {
      table.title <- paste0("Known Groups Validity of ",
                           PRO_score, 
                           " for ",
                           group)
    }

    #output to word file
    dump_df_mat_to_file(out, 
                        print.dir = print.dir, 
                        file.name=file.name, 
                        cols = c(2,3,6),
                        decimals=c(0,2,2),
                        NA.string =NA.string,
                        table.title = table.title,
                        table.footnote = footnotes)
    
  } else {
    #return the df
    return(out)
  }
  
  
  
}#end function










