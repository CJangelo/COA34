# Cole Ayasse
# 30 September 2020

# Known groups validity function

# NOTE: this will need to have semi-partial omega squared calculations added!

known.groups.fntn <- function(data,score.var,groups.var.list,
                              file.name="KnownGroupsValidity",
                              subject.id.var,model.eff.size="omega",
                              write.to.table=T) {
  
  # groups.var.list may be a single grouping variable, or a list of several grouping variables
  # note 1: assumes long form data
  # note 2: Factors will be ordered in the way R automatically does unless
  #         done beforehand. R will order alphabetically or small to large.
  
  # Read in the S-P Omega Sq function: ####
  
  sp.omega2.fntn <- function(input.model,write.to.table=T,
                             file.name="SemiPartialOmegaSq") {
    # input.model is the model to be calculated from
    
    # load functions:
    library(rtf) #for .doc formatted tables
    library(broom) #allows one to snag the omnibus p-value for lm
    library(car) #allows one to use car::Anova to get type III SS (need for matching on SAS)
    
    table.out <- as.data.frame(matrix(ncol=5,nrow=0)) #initialize a table for the output
    colnames(table.out) <- c("Effect","Omega2_SP","NumDF","F-stat","P-val") #give columns names
    
    if (class(input.model)[1]=="lm") { #if model is lm
      
      # Omnibus: calc the hypothesis that jointly all estimates==0 
      summary.out.f <- summary(input.model)$fstatistic 
      #   grab F-stat and degrees of freedom from that:
      F.omni <- summary.out.f['value']
      Df.res.omni <- summary.out.f['dendf']
      Df.num.omni <- summary.out.f['numdf']
      # calc sp omega sq for omni:
      sp.omega2.omni <- ((F.omni*Df.num.omni-Df.num.omni) / 
                           (F.omni*Df.num.omni +Df.res.omni +1))
      # put output into table:
      table.out[1,"Effect"] <- "Omnibus"
      table.out[1,"Omega2_SP"] <- format(round(sp.omega2.omni,
                                               digits=3),nsmall=3)
      table.out[1,"NumDF"] <- Df.num.omni
      table.out[1,"F-stat"] <- format(round(F.omni,digits=3),nsmall=3)
      table.out[1,"P-val"] <- format(round(glance(input.model)$p.value,digits=3),
                                     nsmall=3)
      # get SS total to use later:
      SS.Tot=F.omni*Df.num.omni + Df.res.omni
      
      # Calculate for each separate variable or interaction:
      anova.out <- car::Anova(input.model,type=3) #anova will give f-values and numerator df (still need denominator df)
      list.vars.intxns <- rownames(anova.out) #grab a list of the variables and interactions used (plus residuals at end)
      for (var in 2:(length(list.vars.intxns)-1)) { #loop through the vars and intxns in model 
        #   ^notes:(-1 b/c residuals is at end); 2: since intercept is first
        
        # print(var)
        # print(table.out)
        
        # grab F-stat and degrees of freedom for this var/intxn:
        F.curvar <- anova.out[list.vars.intxns[var],'F value']
        Df.res.curvar <- anova.out['Residuals','Df']
        Df.num.curvar <- anova.out[list.vars.intxns[var],'Df']
        # calc sp omega sq for current var/intxn:
        sp.omega2.curvar <- (F.curvar*Df.num.curvar-Df.num.curvar) / (SS.Tot + 1) #diff formula than omnibus
        # put output into table:
        table.out[var,"Effect"] <- list.vars.intxns[var] #note: don't need var+1 since starting at 2 to skip the intercept
        table.out[var,"Omega2_SP"] <- format(round(sp.omega2.curvar,
                                                   digits=3),nsmall=3)
        table.out[var,"NumDF"] <- Df.num.curvar
        table.out[var,"F-stat"] <- format(round(F.curvar,digits=3),nsmall=3)
        table.out[var,"P-val"] <- format(round(anova.out[list.vars.intxns[var],
                                                         'Pr(>F)'],digits=3),
                                         nsmall=3)
        
        # print(table.out)
        
      }
      
    } else if (class(input.model)[1]=="lmerMod") { #if model is lmer (lme4 pkg)
      
      warning("In order to match SAS output, lmer from lme4 must be run with contrasts=list(factor1='contr.sum',factor2='contr.sum)")
      
      # NOTE: 
      #     lmer.pr.t3 <- lme4::lmer(y ~ Gender.Cat*Age.Cat+(1|Person), data=potroy.dat, 
      #       control=lmerControl(optimizer="bobyqa"), REML=TRUE, #REML
      #       contrasts=list(Gender.Cat='contr.sum',Age.Cat='contr.sum')) 
      #     anova.out.lmer.t3 <- car::Anova(lmer.pr.t3,type="III",test.statistic="F",error.df=T)
      #   ^this is what gives output that will match SAS
      
      list.fix.eff <- fixef(input.model) #save the list of fixed effects, which has the names of contrasts
      contrast.names <- names(list.fix.eff)[-1] #save the names of contrasts, minus the first which is intercept
      contrast.names <- paste0(contrast.names,"=0") #put "=0" on the end of each name to work to put in linearHypothesis fntn
      
      # Omnibus: calc the hypothesis that jointly all estimates==0 
      lh.out.omni <- linearHypothesis(input.model,contrast.names,test="F") 
      #   grab F-stat and degrees of freedom:
      F.omni <- lh.out.omni$F[2]
      Df.res.omni <- lh.out.omni$Res.Df[2]
      Df.num.omni <- lh.out.omni$Df[2]
      # calc sp omega sq for omni:
      sp.omega2.omni <- ((F.omni*Df.num.omni-Df.num.omni) / 
                           (F.omni*Df.num.omni +Df.res.omni +1))
      # put output into table:
      table.out[1,"Effect"] <- "Omnibus"
      table.out[1,"Omega2_SP"] <- format(round(sp.omega2.omni,digits=3),nsmall=3)
      table.out[1,"NumDF"] <- Df.num.omni
      table.out[1,"F-stat"] <- format(round(F.omni,digits=3),nsmall=3)
      table.out[1,"P-val"] <- format(round(lh.out.omni$`Pr(>F)`[2],digits=3),
                                     nsmall=3)
      # get SS total to use later:
      SS.Tot=F.omni*Df.num.omni + Df.res.omni
      
      # Calculate for each separate variable or interaction:
      anova.out <- car::Anova(input.model,type="III",test.statistic="F",error.df=T) #anova will give f-values and numerator df (still need denominator df)
      list.vars.intxns <- rownames(anova.out) #grab a list of the variables and interactions used
      for (var in 2:length(list.vars.intxns)) { #loop through the vars and intxns in model
        #     ^start at 2 to skip the "Intercept" row (which is first)
        
        # extract F-stat and degrees of freedom:
        F.curvar <- anova.out[list.vars.intxns[var],'F'] #get f-val from car::Anova output (since lh and anova doesn't match SAS)
        Df.res.curvar <- anova.out[list.vars.intxns[var],'Df.res']
        Df.num.curvar <- anova.out[list.vars.intxns[var],'Df']
        # calc sp omega sq for current var/intxn:
        sp.omega2.curvar <- (F.curvar*Df.num.curvar-Df.num.curvar) / (SS.Tot + 1) #diff formula than omnibus
        # put output into table:
        table.out[var,"Effect"] <- list.vars.intxns[var] #note: don't need var+1, just var, since starting at 2 (to skip intercept)
        table.out[var,"Omega2_SP"] <- format(round(sp.omega2.curvar,
                                                   digits=3),nsmall=3)
        table.out[var,"NumDF"] <- Df.num.curvar
        table.out[var,"F-stat"] <- format(round(F.curvar,digits=3),nsmall=3)
        table.out[var,"P-val"] <- format(round(
          anova.out[list.vars.intxns[var],'Pr(>F)'],digits=3),nsmall=3)
        
      }
      
    } else if (class(input.model)[1]=="lmerModLmerTest") { #if model is from lmerTest pkg lmer
      
      list.fix.eff <- fixef(input.model) #save the list of fixed effects, which has the names of contrasts
      contrast.names <- names(list.fix.eff)[-1] #save the names of contrasts, minus the first which is intercept
      contrast.names <- paste0(contrast.names,"=0") #put "=0" on the end of each name to work to put in linearHypothesis fntn
      
      # Omnibus: calc the hypothesis that jointly all estimates==0 
      lh.out.omni <- linearHypothesis(input.model,contrast.names,test="F") 
      #   grab F-stat and degrees of freedom:
      F.omni <- lh.out.omni$F[2]
      Df.res.omni <- lh.out.omni$Res.Df[2]
      Df.num.omni <- lh.out.omni$Df[2]
      # calc sp omega sq for omni:
      sp.omega2.omni <- ((F.omni*Df.num.omni-Df.num.omni) / 
                           (F.omni*Df.num.omni +Df.res.omni +1))
      # put output into table:
      table.out[1,"Effect"] <- "Omnibus"
      table.out[1,"Omega2_SP"] <- format(round(sp.omega2.omni,digits=3),nsmall=3)
      table.out[1,"NumDF"] <- Df.num.omni
      table.out[1,"F-stat"] <- format(round(F.omni,digits=3),nsmall=3)
      table.out[1,"P-val"] <- format(round(lh.out.omni$`Pr(>F)`[2],digits=3),
                                     nsmall=3)
      # get SS total to use later:
      SS.Tot=F.omni*Df.num.omni + Df.res.omni
      
      # Calculate for each separate variable or interaction:
      anova.out <- anova(input.model,type=3,ddf="Kenward-Roger") #anova will give f-values and numerator df (still need denominator df)
      list.vars.intxns <- rownames(anova.out) #grab a list of the variables and interactions used
      for (var in 1:length(list.vars.intxns)) { #loop through the vars and intxns in model
        
        # extract F-stat and degrees of freedom:
        F.curvar <- anova.out[list.vars.intxns[var],'F value'] #get f-val from anova output (since lh doesn't match SAS)
        Df.res.curvar <- anova.out[list.vars.intxns[var],'DenDF']
        Df.num.curvar <- anova.out[list.vars.intxns[var],'NumDF']
        # calc sp omega sq for current var/intxn:
        sp.omega2.curvar <- (F.curvar*Df.num.curvar-Df.num.curvar) / (SS.Tot + 1) #diff formula than omnibus
        # put output into table:
        table.out[1+var,"Effect"] <- list.vars.intxns[var]
        table.out[1+var,"Omega2_SP"] <- format(round(sp.omega2.curvar,
                                                     digits=3),nsmall=3)
        table.out[1+var,"NumDF"] <- Df.num.curvar
        table.out[1+var,"F-stat"] <- format(round(F.curvar,digits=3),nsmall=3)
        table.out[1+var,"P-val"] <- format(round(
          anova.out[list.vars.intxns[var],'Pr(>F)'],digits=3),nsmall=3)
        
      }
      
    } else if (class(input.model)[1]=="lme") { #if model is lme (nlme pkg)
      
      warning("Currently, inputted models that are lme require being re-run in lmerTest. It is suggested to simply run the model in lmerTest. Currently cannot match optimizers between lme and lmer so may have some differences in the model.")
      
      # stop("Currently unable to perform s-p omega sq on lme models, cannot calc a denominator df")
      
      library(lmerTest) #will need lmerTest internally
      
      # extract necessary elements of the formula in order to re-run the model:
      fixed.call <- deparse(input.model$call$fixed) #grab the fixed part of the formula and "deparse" to make it a string (easier to work with)
      random.call <- deparse(input.model$call$random)
      random.call <- gsub(pattern="~",replacement="",x=random.call,fixed=T) #get rid of the "~" since this is not included for lmer
      
      # extract the method (REML or ML):
      model.method <- input.model$method #is already a character
      # extract the na.action method:
      model.na.action <- toString(input.model$call$na.action) #make it a string
      
      # run the new model with this info:
      model.formula <- paste0(fixed.call," +(",random.call,")")
      if (model.method=="REML") {
        new.model <- lmerTest::lmer(as.formula(model.formula),data=input.model$data,
                                    control=lmerControl(optimizer="bobyqa"),
                                    REML=TRUE,na.action=model.na.action)
      } else if (model.method=="ML") {
        new.model <- lmerTest::lmer(as.formula(model.formula),data=input.model$data,
                                    control=lmerControl(optimizer="bobyqa"),
                                    REML=FALSE,na.action=model.na.action)
      } #ML or REML are the only options
      
      
      # Now calculate semi-partial omega squared with the newly-run model:
      
      list.fix.eff <- fixef(new.model) #save the list of fixed effects, which has the names of contrasts
      contrast.names <- names(list.fix.eff)[-1] #save the names of contrasts, minus the first which is intercept
      contrast.names <- paste0(contrast.names,"=0") #put "=0" on the end of each name to work to put in linearHypothesis fntn
      
      # Omnibus: calc the hypothesis that jointly all estimates==0 
      lh.out.omni <- linearHypothesis(new.model,contrast.names,test="F") 
      #   grab F-stat and degrees of freedom:
      F.omni <- lh.out.omni$F[2]
      Df.res.omni <- lh.out.omni$Res.Df[2]
      Df.num.omni <- lh.out.omni$Df[2]
      # calc sp omega sq for omni:
      sp.omega2.omni <- ((F.omni*Df.num.omni-Df.num.omni) / 
                           (F.omni*Df.num.omni +Df.res.omni +1))
      # put output into table:
      table.out[1,"Effect"] <- "Omnibus"
      table.out[1,"Omega2_SP"] <- format(round(sp.omega2.omni,digits=3),nsmall=3)
      table.out[1,"NumDF"] <- Df.num.omni
      table.out[1,"F-stat"] <- format(round(F.omni,digits=3),nsmall=3)
      table.out[1,"P-val"] <- format(round(lh.out.omni$`Pr(>F)`[2],digits=3),
                                     nsmall=3)
      # get SS total to use later:
      SS.Tot=F.omni*Df.num.omni + Df.res.omni
      
      # Calculate for each separate variable or interaction:
      anova.out <- anova(new.model,type=3,ddf="Kenward-Roger") #anova will give f-values and numerator df (still need denominator df)
      list.vars.intxns <- rownames(anova.out) #grab a list of the variables and interactions used
      for (var in 1:length(list.vars.intxns)) { #loop through the vars and intxns in model
        
        # extract F-stat and degrees of freedom:
        F.curvar <- anova.out[list.vars.intxns[var],'F value'] #get f-val from anova output (since lh doesn't match SAS)
        Df.res.curvar <- anova.out[list.vars.intxns[var],'DenDF']
        Df.num.curvar <- anova.out[list.vars.intxns[var],'NumDF']
        # calc sp omega sq for current var/intxn:
        sp.omega2.curvar <- (F.curvar*Df.num.curvar-Df.num.curvar) / (SS.Tot + 1) #diff formula than omnibus
        # put output into table:
        table.out[1+var,"Effect"] <- list.vars.intxns[var]
        table.out[1+var,"Omega2_SP"] <- format(round(sp.omega2.curvar,
                                                     digits=3),nsmall=3)
        table.out[1+var,"NumDF"] <- Df.num.curvar
        table.out[1+var,"F-stat"] <- format(round(F.curvar,digits=3),nsmall=3)
        table.out[1+var,"P-val"] <- format(round(
          anova.out[list.vars.intxns[var],'Pr(>F)'],digits=3),nsmall=3)
        
      }
      
      
    } #notes: With lme, linearHypothesis only seems to give the Chisq 
    #         (and so only the num.df)
    #         It may be useful that lme will give the full dataset, 
    #         so could re-run the lmer model based on that, but would require re-configuring the formula
    #         With lmer (via lme4 not lmerTest), cannot find a way INTERNALLY to both get
    #         the correct denominator/residual df (that matches SAS) AND get the
    #         correct TypeIII F-stat (tried via car::Anova, via linearHypothesis, etc)
    #         -> lmer needs to be run correctly externally
    
    if (write.to.table==T) {
      # write to .doc:
      rtf<-RTF(file=paste0(file.name,".doc"),width=8.5,height=11,font.size=12,
               omi=c(1,1,1,1))
      addTable(rtf,table.out,font.size=12,
               row.names=FALSE,NA.string="-")
      done(rtf)
    }
    
    table.out <<- table.out #makes it globally available (for some reason without this, it will overwrite line 1 with NA's, but leave the rest...)
    return(table.out)
    
    
  }
  
  
  
  # Continue with KG function: ####
  
  # set up the output table:
  if (model.eff.size=="omega") {
    tbl.colnames <- c("Variable","Group","N","Group Mean",
                      "Difference from Reference (95% CI)","p-value",
                      "Model Omega2_SP")
  } else if (model.eff.size=="r2") {
    tbl.colnames <- c("Variable","Group","N","Group Mean",
                      "Difference from Reference (95% CI)","p-value",
                      "Model R2")
  }
  out.table <- data.frame(matrix(ncol=length(tbl.colnames),nrow=0)) #since each predictor may have any number of levels, need to rbind
  colnames(out.table) <- tbl.colnames
  
  # loop through the grouping variables:
  for (var in seq(from=1,to=length(groups.var.list),by=1)) { #just go by each predictor (does not need seq anymore, left in case architecture changes)
    
    # get current grouping var name:
    current.group <- groups.var.list[var] #if sequence is going by 2's or 3's, need to calculate current predictor
    # make sure grouping var is a factor:
    data[[current.group]] <- as.factor(data[[current.group]])
    
    # fit the model, predicting "score" by "groups.var":
    formula <- paste0(score.var," ~ ",current.group)
    lm.fit <- lm(as.formula(formula),data=data)
    
    # calculate semi-partial omega squared for the model:
    table.out <- sp.omega2.fntn(input.model=lm.fit,write.to.table=F)
    omega2_sp <- table.out[2,'Omega2_SP'] #grab the second omega2_sp value
    #     ^note: First is the omnibus, the second row is the group. 
    #            For one variable, as in known-groups, these will be the same.
    
    # put pieces into the table:
    #   create a temporary table to rbind to the overall out.table:
    temp.table <- data.frame(matrix(ncol=dim(out.table)[2],nrow=0)) #since each predictor may have any number of levels, need to rbind
    colnames(temp.table) <- colnames(out.table) #make columns same as out.table (overall table)
    temp.table[1,"Variable"] <- current.group #put grouping var name in top row
    # get each level of the current grouping variable:
    grp.levels <- levels(data[[current.group]])
    # loop through to include info for each level of grouping var:
    for (level in 1:length(grp.levels)) {
      
      # put level name in:
      temp.table[level,"Group"] <- 
        levels(data[[current.group]])[level]
      # put N for each group level in:
      temp.table[level,"N"] <- 
        length(unique(filter(data,data[[current.group]]==grp.levels[level])[[
          subject.id.var]]))
      # put group mean for each group level in:
      temp.table[level,"Group Mean"] <- 
        format(round(mean(filter(data,data[[current.group]]==grp.levels[level])[[
          score.var]],na.rm=TRUE),digits=2),nsmall=2) #get mean (without NA's) of the score var at current group level
      
      # perform only if NOT at reference group:
      if (level > 1) {
        # put in the estimate (at each estimated group level):
        temp.table[level,"Difference from Reference (95% CI)"] <- paste0(
          format(round(summary(lm.fit)$coef[level,'Estimate'],digits=2),
                 nsmall=2), " (",format(round(confint(lm.fit)[
                   level,1],digits=2),nsmall=2), ",", format(round(confint(lm.fit)[
                     level,2],digits=2),nsmall=2), ")")
        #     ^extracting at row=level should work, given that the output will 
        #       start at "(Intercept)", so row 2 will be the first estimated 
        #       group (skipping reference group)
        # # put in the lower and upper 95% CI (at each estimated group level):
        # temp.table[level,"95% CI"] <- paste0(format(round(confint(lm.fit)[
        #   level,1],digits=2),nsmall=2), ", ", format(round(confint(lm.fit)[
        #     level,2],digits=2),nsmall=2))
        # put in the p-value (rounded to 3 decimals):
        if (as.numeric(summary(lm.fit)$coef[level,'Pr(>|t|)']) >= 0.001) {
          temp.table[level,"p-value"] <- format(round(summary(lm.fit)$coef[
            level,'Pr(>|t|)'],digits=3),nsmall=3)
        } else {
          temp.table[level,"p-value"] <- "< 0.001" #put p-value as < 0.001 if that is true
        }
        
        # put model r2 or omega2_sp only at final level PLUS ONE (to put in its own level):
        if (level==length(grp.levels)) {
          if (model.eff.size=="r2") {
            # put in the Model R squared (rounded to 2 decimals):
            temp.table[level+1,length(tbl.colnames)] <- format(round(summary(
              lm.fit)$r.squared,digits=2),nsmall=2)
          } else if (model.eff.size=="omega") {
            # put in the omega2_sp (already rounded to 3 decimals within omega fntn):
            temp.table[level+1,length(tbl.colnames)] <- omega2_sp
          }
        }
      }
      
    }
    
    out.table <- rbind(out.table,temp.table)
    
  }
  
  if (write.to.table==T){
    # save as table for .doc:
    rtf<-RTF(paste0(file.name,".doc"),width=8.5,height=11,font.size=12,
             omi=c(1,1,1,1))
    addTable(rtf,as.data.frame(out.table),font.size=12,
             row.names=FALSE,NA.string="-")
    done(rtf)
  } else {
    out.table <<- out.table
    return(out.table)
  }
  
  
  
}