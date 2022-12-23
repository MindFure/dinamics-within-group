library(DescTools)
library(IDPmisc)
library(rlist)
dynamics <- function(data, id_groups, combination,columns.list,id.group,PATH,FILENAME){
  result_list <- list()
  res_ls <- list()
  comb_list <- lapply(combination,combn,2,simplify=FALSE)
  for(c in 1:length(comb_list)){
    for(g in id_groups){
      
      # data$null <- NA
      x = unlist(columns.list, recursive = TRUE)
      x.length = length(x)
      id.columns   = x[seq(1,x.length,2)]
      type.columns = x[seq(2,x.length,2)]
      id.columns   = as.integer(id.columns)
      type.columns = as.character(type.columns)
      #print(comb)
      columns <- id.columns
      print(paste(id.columns,type.columns))
      
      #----------------------------
      #combination <- lapply(combination,combn,2,simplify=FALSE)
      comb_list_N_B_C <- combination#comb_list
      
      comb_test <- unlist(comb_list_N_B_C[c])
      comb_test1 <-unlist(comb_list_N_B_C,combn,recursive = FALSE)
      print(paste("33",comb_test1))
      
      
      df = as.matrix(data[data[[id.group]] %in% g,])
      #df <- as.matrix(apply(df, 2, as.numeric))
      df <- df[,unique(unlist(comb_list[c]))]
      
      col.list <- dplyr::bind_rows(columns.list)
      
      col_type_list <-lapply(comb_test, function(x) col.list$type[match(x, col.list$id)])
      col_type_list <- unlist(col_type_list)
      
      if(is.matrix(df)){
        df <- t(apply(df, 1, function(x) {
          if(any(is.na(x))) x[1:length(x)] <- NA
          x
        }))
      }
      else{
        df <- as.data.frame(t(df))
        df <- t(apply(df, 1, function(x) {
          if(any(is.na(x))) x[1:length(x)] <- NA
          x
        }))
        df <- as.vector(df)
      }
      
      if(all(col_type_list=="num")){
        if(is.matrix(df)==TRUE){
          df <- as.matrix(apply(df, 2, as.numeric))
          
          med <- paste(apply(df, 2,median.my),
                       apply(df,2,function(x){ifelse(any(!is.na(x)),quantile.interval.my(x),"")}),"\n",round(apply(df, 2,mean,na.rm=TRUE),2),"±",round(apply(df, 2,sd,na.rm=TRUE),2),"\n","(",round(apply(df, 2,min.my,na.rm=TRUE),2),"-",round(apply(df, 2,max.my,na.rm=TRUE),2),")")
        }
        else{
          med <- lapply(df,'*',NA)
        }
      }
      if(all(col_type_list=="bin")){
        df <- as.matrix(apply(df, 2, as.numeric))
        med <- apply(df, 2,bin.my)
        
      }
      if(all(col_type_list=="cat")){
        #med <- table(df)
        
        #df.5 <- prop.table(table(df))
        #med <- mapply(paste, apply(df, 2, table),apply(df, 2, paste),collapse="\n")
        med <- apply(df,2, f)
        #med <- apply(med,2,paste,collapse="\n")
        
        
      }
      
      data_df = as.matrix(data[data[[id.group]] %in% g,])
      #data_df <- as.matrix(apply(data_df, 2, as.numeric))
      data_df_naomit = data_df[,unique(unlist(comb_list[c]))]
      df.matrix <- ifelse(is.matrix(data_df_naomit),na.count.string(data_df_naomit[,1]),na.count.string(data_df_naomit[,1]))
      #df.matrix <- na.count.string(df[,1])
      #print(paste(df.matrix))
      result <- data.frame(matrix(NA,ncol=length(med)+2,nrow=1))
      result[1,] <- c(paste0(g,"\nN=",df.matrix),med,NA)
      
      for(k in 1:length(comb_list[[1]])){
        #отсюда num
        
        i <- comb_list[[c]][[k]][1]
        j <- comb_list[[c]][[k]][2]
        if(all(col_type_list=="num")){
          # data_df[,i] <- as.numeric(data_df[,i])
          # data_df[,j] <- as.numeric(data_df[,j])
          data_df <- as.matrix(apply(data_df, 2, as.numeric))
          if((all(is.na(data_df[,i])) == FALSE) & (all(is.na(data_df[,j])) == FALSE)){
            unique.flag = 0
            if ( length(unique(data_df[,i])) < 3  )
            {
              
              cat ("\nunique values in first group too small")
              unique.flag = 1
              
            }
            if ( length(unique(data_df[,j])) < 3  )
            {
              
              cat ("\nunique values in first group too small")
              unique.flag = 1
              
            }
            
            if(unique.flag == 1 ) {
              test = NULL
            } else{
              test <- wilcox.test(data_df[,i],data_df[,j],conf.int=TRUE)
            }
          }
          #----------------------------------------------------------------
          if((all(is.na(data_df[,i])) == TRUE) & (all(is.na(data_df[,j])) == FALSE)){
            unique.flag = 0
            if ( length(unique(data_df[,i])) < 3  )
            {
              
              cat ("\nunique values in first group too small")
              unique.flag = 1
              
            }
            if ( length(unique(data_df[,j])) < 3  )
            {
              
              cat ("\nunique values in first group too small")
              unique.flag = 1
              
            }
            
            if(unique.flag == 1 ) {
              test = NULL
            } else{
              
              test <- wilcox.test(data_df[,i],data_df[,j],conf.int=TRUE)
              
            }
          }
          #--------------------------------------------------------------------
          if((all(is.na(data_df[,i])) == FALSE) & (all(is.na(data_df[,j])) == TRUE)){
            unique.flag = 0
            if ( length(unique(data_df[,i])) < 3  )
            {
              
              cat ("\nunique values in first group too small")
              unique.flag = 1
              
            }
            if ( length(unique(data_df[,j])) < 3  )
            {
              
              cat ("\nunique values in first group too small")
              unique.flag = 1
              
            }
            
            if(unique.flag == 1 ) {
              test = NULL
            } else{
              test <- wilcox.test(data_df[,i],data_df[,j],conf.int=TRUE)
            }
          }
        }
        if(all(col_type_list=="bin")){
          data_df <- as.matrix(apply(data_df, 2, as.numeric))
          #if(all(is.na(data_df[,i])==T))
          if(all(data_df[,i]=="1")|all(data_df[,j]=="1")){
            test1 <- fisher.test(list.append(data_df[,i],0),list.append(data_df[,j],0))
            test <- mcnemar.test(list.append(data_df[,i],0),list.append(data_df[,j],0),correct = TRUE)
          }else if(all(data_df[,i]=="0")|all(data_df[,j]=="0")){
            test1 <- fisher.test(list.append(data_df[,i],1),list.append(data_df[,j],1))
            test <- mcnemar.test(list.append(data_df[,i],1),list.append(data_df[,j],1),correct = TRUE)
          }else{
            test1 <- fisher.test(data_df[,i],data_df[,j])
            test <- mcnemar.test(data_df[,i],data_df[,j],correct = TRUE)
          }
        }
        if(all(col_type_list=="cat")){
          contingency.table.1 = table(data_df[,i],data_df[,j])
          contingency.table.1 <- make_symmetric_matrix(contingency.table.1)
          contingency.table.1[row(contingency.table.1) == 
                                col(contingency.table.1) & contingency.table.1 == 0] <- 1
          sym_mat <- contingency.table.1  
          contingency.table.1[sym_mat == t(sym_mat) & sym_mat == 0] <- 1
          print(contingency.table.1)
          test <- nominalSymmetryTest(contingency.table.1,
                                      digits     = 3,
                                      MonteCarlo = TRUE,
                                      ntrial     = 1000000)$Global.test.for.symmetry
          test <- p_value_formatted(test$p.value)
        }
        #------------------------------------------------------------
        if(all(col_type_list=="num")){
          if(!is.null(test)){
            diff.1 <- round(data_df[,i]-data_df[,j],2)
            diff.1 <- NaRV.omit(diff.1)
            diff.p <- MedianCI(outer(diff.1,diff.1,"+")/2,sides="two.sided",method="exact",na.rm=TRUE)
            diff.p.1 <- round(diff.p[[1]], 2)
            
            res.ci      = paste(c( '[',
                                   as.character(round(diff.p[[2]],2)),
                                   '; ',
                                   as.character(round(diff.p[[3]],2)),
                                   ']'), collapse=''
            )
            
            smd = epi.smd(mean.trt = mean(data_df[,i], na.rm =T), 
                          sd.trt = sd(data_df[,i], na.rm = T), 
                          n.trt = length(data_df[,i]),
                          mean.ctrl = mean(data_df[,j], na.rm =T),
                          sd.ctrl = sd(data_df[,j], na.rm = T),
                          n.ctrl = length(data_df[,j]),
                          names  = c('data_df[,i]','data_df[,j]'),
                          method = "cohens",
                          conf.level = 0.95)
            res.smd     = smd$md[['est']] %>% round(2)
            res.smd.ci  = paste0("[", smd$md[['lower']]%>% round(2), 
                                 "; ", smd$md[['upper']]%>% round(2), "]")
            
            p.value <- p_value_formatted(round(test$p.value,3))
            result[k, length(med)+2] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",paste(diff.p.1,res.ci,"\n",res.smd," ",res.smd.ci))
            result[k, length(med)+3] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",p.value)
          }
          if(is.null(test)){
            diff.1 <- "NA"
            p.value <- "NA"
            result[k, length(med)+2] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",diff.1)
            result[k, length(med)+3] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",p.value)
          }
        }
        if(all(col_type_list=="bin")){
          if(!is.null(test)){
            
            
            p.value <- p_value_formatted(round(test$p.value,3))
            result[k, length(med)+2] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",test1$estimate," [",
                                               round(test1$conf.int[1],1),
                                               "; ",
                                               round(test1$conf.int[2],1),
                                               "]")
            result[k, length(med)+3] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",p.value)
          }
          if(is.null(test)){
            diff.1 <- "NA"
            p.value <- "NA"
            result[k, length(med)+2] <- diff.1
            result[k, length(med)+3] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",p.value)
          }
        }
        if(all(col_type_list=="cat")){
          if(!is.null(test)){
            
            
            p.value <- test#p_value_formatted(round(test$p.value,3))
            result[k, length(med)+2] <- ""#paste0(i,"-",j,": ","")
            result[k, length(med)+3] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",p.value)
          }
          if(is.null(test)){
            diff.1 <- "NA"
            p.value <- "NA"
            result[k, length(med)+2] <- ""#paste0(i,"-",j,": ",diff.1)
            result[k, length(med)+3] <- paste0(colnames(data_df)[i],"-",colnames(data_df)[j],": ",p.value)
          }
        }
        #досюда num
        
        
      }
      
      result[1, length(med)+2]=paste(result[,length(med)+2],collapse = "\n")
      result[1, length(med)+3]=paste(result[,length(med)+3],collapse = "\n")
      result_list[[g]] <- result[1,]
    }
    
    
    result_new <- do.call(rbind,result_list)
    hh <- as.numeric(unique(unlist(comb_list[c])))
    #print(paste(comb_test))
    names(result_new) <- c("GROUP",colnames(data[,comb_test]),"dif","p.value")
    res_ls[[c]] <- result_new
  }
  result_new1 <-do.call(rbind.data.frame, lapply(res_ls, function(x) unname(rbind(names(x), as.matrix(x)))))#rbindlist(res_ls)
  names(result_new1) <- result_new1[1,]
  result_new1 <- result_new1[-1,]
  #rbind(res_ls[[1]],names(res_ls[[2]]),setNames(res_ls[[2]],names(res_ls[[1]])))#do.call(rbind,res_ls)
  # names(result_new) <- c("GROUP",as.character(unique(unlist(colnames(data[,comb_test])))),"dif","p.value")
  
  path.data = paste0(PATH, FILENAME,".csv")
  
  write.table(result_new1,file=path.data,sep=";",dec=",",na="",col.names = T,row.names = F)
  
  
  return(result_new1)
}

f <- function(x) {
  tx <- table(x)
  px <- proportions(tx)
  paste(sprintf('%s - %s (%s%%)', names(tx), tx, round(px*100, 1)), collapse='\n')
}
