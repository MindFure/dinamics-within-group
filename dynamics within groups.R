library(DescTools)
library(IDPmisc)
library(rlist)
library(flextable)
library(officer)
dynamics <- function(data, id_groups, combination,columns.list,id.group,PATH,FILENAME,comp.flag=T,names.col,list_its){
  result_list <- list()
  res_ls <- list()
  #comb_list <- lapply(combination,combn,2,simplify=FALSE)
  item_index <- 1
  #for(c in 1:length(comb_list)){
  for(item in list_its){
    
    id_groups <- item$id_groups
    
    comb_list <- combn(item$id.bin,2,simplify = FALSE)
    current_index <- item_index
    
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
    
    comb_test <- unlist(comb_list)
    print(paste("22",comb_test))
    # comb_test1 <-unlist(comb_list_N_B_C,combn,recursive = FALSE)
    # print(paste("33",comb_test1))
    
    
    df <- as.matrix(data[data[[item$id.group]] %in% g,])
    #df <- as.matrix(apply(df, 2, as.numeric))
    df <- df[,unique(unlist(comb_list))]
    #df.comp <- as.data.frame(df)
    col.list <- dplyr::bind_rows(columns.list)

    col_type_list <-lapply(comb_test, function(x) col.list$type[match(x, col.list$id)])
    
    col_type_list <- unlist(col_type_list)
    print(col_type_list)
    common.list <- lapply(1:length(comb_test), function(x) list(id = comb_test[[x]], type = col_type_list[[x]]))
    print(paste("1111",common.list))
    #-----------------------------------------------------------------
    mark.list <- c()
      mark.list.1 <- c()
      mark.list.met <- c()
      mark.met <- c()
    if(comp.flag==T){
    sr <- cmp.group.all(id.group     = id.group,
                  group.labels = id_groups,
                  columns.list = common.list,
                  data         = data,
                  save.file.cmp.2groups    = F,
                  PATH         = path.dir.out,
                  FILENAME     = paste('не открывать',c),
                  result = NULL)
    rr <- as.matrix(sr[,5])
    rr <- as.vector(rr)
    }
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
    
    data_df = as.matrix(data[data[[item$id.group]] %in% g,]) ######//////
    #-----------------------------------------------------
    #data_df <- as.matrix(do.call(qpcR:::cbind.na, lapply(combination, function(i)data_df[complete.cases(data_df[c(i)]),i])))
    #-----------------------------------------------------
    #data_df <- as.matrix(apply(data_df, 2, as.numeric))
    data_df_naomit = data_df[,unique(unlist(comb_list))]
    df.matrix <- ifelse(is.matrix(data_df_naomit),na.count.string(data_df_naomit[,1]),na.count.string(data_df_naomit[,1]))
    #df.matrix <- na.count.string(df[,1])
    #print(paste(df.matrix))
    result <- data.frame(matrix(NA,ncol=length(med)+2,nrow=1))
    result[1,] <- c(paste0(g,"\nN=",df.matrix),med,"")
    if(comp.flag==T){
    if(g==tail(id_groups, n=1)){
    result[ncol(result)+1-3+1,] <- c(paste("Сравнение"),rr,"")
    result[ncol(result)+1-3+1,ncol(result)+1] <- ""
    }
    }
    list_z <- unlist(lapply(ncol(df), combn, 2, simplify=FALSE), recursive=FALSE) 
    for(k in 1:length(comb_list)){
      #отсюда num
      #  
      # i <- combination[[c]][[k]][1]
      # j <- combination[[c]][[k]][2]
      i <- list_z[[k]][[1]]
      j <- list_z[[k]][[2]]
      #data_df <- as.matrix(do.call(qpcR:::cbind.na, lapply(combination, function(i)data_df[complete.cases(data_df[c(i)]),i])))
      if(all(col_type_list=="num")){
      # data_df[,i] <- as.numeric(data_df[,i])
      # data_df[,j] <- as.numeric(data_df[,j])
        #data_df <- as.matrix(apply(data_df, 2, as.numeric))
      data_df <- as.matrix(apply(data_df, 2, as.numeric))
      #data_df <- do.call(qpcR:::cbind.na, lapply(combination, function(i)data_df[complete.cases(data_df[c(i)]),i]))
      if((all(is.na(df[,i])) == FALSE) & (all(is.na(df[,j])) == FALSE)){
                    unique.flag = 0
                    if ( length(unique(df[,i])) < 3  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(unique(df[,j])) < 3  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test = NULL
                    } else{
                        test <- wilcox.test(df[,i],df[,j],conf.int=TRUE,paired=T)
                    }
      }
#----------------------------------------------------------------
      if((all(is.na(df[,i])) == TRUE) & (all(is.na(df[,j])) == FALSE)){
                    unique.flag = 0
                    if ( length(unique(df[,i])) < 3  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(unique(df[,j])) < 3  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test = NULL
                    } else{
                      
                        test <- wilcox.test(df[,i],df[,j],conf.int=TRUE,paired=T)
                      
                    }
      }
#--------------------------------------------------------------------
      if((all(is.na(df[,i])) == FALSE) & (all(is.na(df[,j])) == TRUE)){
                    unique.flag = 0
                    if ( length(unique(df[,i])) < 3  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(unique(df[,j])) < 3  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test = NULL
                    } else{
                        test <- wilcox.test(df[,i],df[,j],conf.int=TRUE,paired=T)
                    }
      }
      }
      if(all(col_type_list=="bin")){
        data_df <- as.matrix(apply(data_df, 2, as.numeric))
        #data_df <- do.call(qpcR:::cbind.na, lapply(combination, function(i)data_df[complete.cases(data_df[c(i)]),i]))
        #if(all(is.na(data_df[,i])==T))
        contingency.table = data.frame(df[,i],df[,j])
        contingency.table = table(contingency.table[c(1,2)])
        if((all(is.na(df[,i])) == FALSE) & (all(is.na(df[,j])) == FALSE)){
                    if(all(df[,i]=="1",na.rm = TRUE)|all(df[,j]=="1",na.rm = TRUE)){
        if(all(df[,i]=="1",na.rm = TRUE)&all(df[,j]=="0",na.rm = TRUE)){
          test1 <- fisher.test(list.append(df[,i],0),list.append(df[,j],1))
          test <- mcnemar.test(list.append(df[,i],0),list.append(df[,j],1),correct = TRUE)
        }else if(all(df[,i]=="0",na.rm = TRUE)&all(df[,j]=="1",na.rm = TRUE)){
          test1 <- fisher.test(list.append(df[,i],1),list.append(df[,j],0))
          test <- mcnemar.test(list.append(df[,i],1),list.append(df[,j],0),correct = TRUE)
        }else{
        test1 <- fisher.test(list.append(df[,i],0),list.append(df[,j],0))
        test <- mcnemar.test(list.append(df[,i],0),list.append(df[,j],0),correct = TRUE)
        }
        }else if(all(df[,i]=="0",na.rm = TRUE)|all(df[,j]=="0",na.rm = TRUE)){
          if(all(df[,i]=="1",na.rm = TRUE)&all(df[,j]=="0",na.rm = TRUE)){
          test1 <- fisher.test(list.append(df[,i],0),list.append(df[,j],1))
          test <- mcnemar.test(list.append(df[,i],0),list.append(df[,j],1),correct = TRUE)
        }else if(all(df[,i]=="0",na.rm = TRUE)&all(df[,j]=="1",na.rm = TRUE)){
          test1 <- fisher.test(list.append(df[,i],1),list.append(df[,j],0))
          test <- mcnemar.test(list.append(df[,i],1),list.append(df[,j],0),correct = TRUE)
        }else{
          test1 <- fisher.test(list.append(df[,i],1),list.append(df[,j],1))
          test <- mcnemar.test(list.append(df[,i],1),list.append(df[,j],1),correct = TRUE)}
        }else{
          unique.flag = 0
                    if ( length(df[,i]) < 3 | all(is.na(df[,i]))==T  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(df[,j]) < 3 | all(is.na(df[,j]))==T  )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test1 = NULL
                        test = NULL
                    } else{
                     test1 <- fisher.test(df[,i],df[,j])
                     test <- mcnemar.test(contingency.table)
                      
                    }
          # test1 <- fisher.test(data_df[,i],data_df[,j])
          # test <- mcnemar.test(data_df[,i],data_df[,j],correct = TRUE)
        }
        }
        if((all(is.na(df[,i])) == TRUE) & (all(is.na(df[,j])) == FALSE)){
          unique.flag = 0
                    if ( length(df[,i]) < 3 | all(is.na(df[,i]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(df[,j]) < 3 | all(is.na(df[,j]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test1 = NULL
                        test = NULL
                    } else{
                     test1 <- fisher.test(df[,i],df[,j])
                     test <- mcnemar.test(contingency.table)
                      
                    }
      } 
        if((all(is.na(df[,i])) == FALSE) & (all(is.na(df[,j])) == TRUE)){
          unique.flag = 0
                    if ( length(df[,i]) < 3 | all(is.na(df[,i]))==T)
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(df[,j]) < 3 | all(is.na(df[,j]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test1 = NULL
                        test = NULL
                    } else{
                     test1 <- fisher.test(df[,i],df[,j])
                     test <- mcnemar.test(contingency.table)
                      
                    }       
      }
        if((all(is.na(df[,i])) == TRUE) & (all(is.na(df[,j])) == TRUE)){
          unique.flag = 0
                    if ( length(df[,i]) < 3 | all(is.na(df[,i]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(df[,j]) < 3 | all(is.na(df[,j]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test1 = NULL
                        test = NULL
                    } else{
                     test1 <- fisher.test(df[,i],df[,j])
                     test <- mcnemar.test(contingency.table)
                      
                    }        
      }
        
      }
      if(all(col_type_list=="cat")){
        
        if((all(is.na(df[,i])) == FALSE) & (all(is.na(df[,j])) == FALSE)){
          unique.flag = 0
                    if ( length(unique(df[,i])) < 3 | all(is.na(df[,i]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(unique(df[,j])) < 3 | all(is.na(df[,j]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test = NULL
                    } else{
                        contingency.table.1 = table(df[,i],df[,j])
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
                        
                    
         
        }
        if((all(is.na(df[,i])) == TRUE) & (all(is.na(df[,j])) == FALSE)){
          unique.flag = 0
                    if ( length(unique(df[,i])) < 3 | all(is.na(df[,i]))==T)
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(unique(df[,j])) < 3 | all(is.na(df[,j]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test = NULL
                    } else{
                        contingency.table.1 = table(df[,i],df[,j])
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
      } 
        if((all(is.na(df[,i])) == FALSE) & (all(is.na(df[,j])) == TRUE)){
          unique.flag = 0
                    if ( length(unique(df[,i])) < 3 | all(is.na(df[,i]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(unique(df[,j])) < 3 | all(is.na(df[,j]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test = NULL
                    } else{
                        contingency.table.1 = table(df[,i],df[,j])
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
      }
        if((all(is.na(df[,i])) == TRUE) & (all(is.na(df[,j])) == TRUE)){
          unique.flag = 0
                    if ( length(unique(df[,i])) < 3 | all(is.na(df[,i]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    if ( length(unique(df[,j])) < 3 | all(is.na(df[,j]))==T )
                    {
                    
                    cat ("\nunique values in first group too small")
                    unique.flag = 1
                    
                    }
                    
                    if(unique.flag == 1 ) {
                        test = NULL
                    } else{
                        contingency.table.1 = table(df[,i],df[,j])
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
      }
        
      }
#------------------------------------------------------------
      if(all(col_type_list=="num")){
     if(!is.null(test)){
            diff.1 <- round(df[,i]-df[,j],2)
            diff.1 <- NaRV.omit(diff.1)
            diff.p <- MedianCI(outer(diff.1,diff.1,"+")/2,sides="two.sided",method="exact",na.rm=TRUE)
            diff.p.1 <- round(diff.p[[1]], 2)
            res.ci      = paste(c( '[',
                                   as.character(round(diff.p[[2]],2)),
                                   '; ',
                                   as.character(round(diff.p[[3]],2)),
                                   ']'), collapse=''
            )
            smd = epi.smd(mean.trt = mean(df[,i], na.rm =T), 
                          sd.trt = sd(df[,i], na.rm = T), 
                          n.trt = length(df[,i]),
                          mean.ctrl = mean(df[,j], na.rm =T),
                          sd.ctrl = sd(df[,j], na.rm = T),
                          n.ctrl = length(df[,j]),
                          names  = c('df[,i]','df[,j]'),
                          method = "cohens",
                          conf.level = 0.95)
            res.smd     = smd$md[['est']] %>% round(2)
            res.smd.ci  = paste0("[", smd$md[['lower']]%>% round(2), 
                                 "; ", smd$md[['upper']]%>% round(2), "]")
            p.value <- test$p.value
            mark.met <- c(mark.met,colnames(df)[i],"-",colnames(df)[j],": ")
            mark.list <- c(mark.list,p.value)
            mark.list.1 <- c(mark.list.1,paste0(colnames(df)[i],"-",colnames(df)[j],": ",p_value_formatted(p.value)))
            mark.list.met <- p.adjust(mark.list,"BH")
            result[k, length(med)+2] <- paste0(colnames(df)[i],"-",colnames(df)[j],": ",paste(diff.p.1,res.ci,"\n",res.smd," ",res.smd.ci))
            result[k, length(med)+3] <- " "
            if(k==tail(length(comb_list[[1]]), n=1)){
              mark.list.1 <- paste(mark.list.1,", ",p_value_formatted(mark.list.met),collapse = "\n")
              result[k, length(med)+3] <- mark.list.1
            }}
          if(is.null(test)){
            diff.1 <- "NA"
            p.value <- "NA"
            result[k, length(med)+2] <- paste0(colnames(df)[i],"-",colnames(df)[j],": ",diff.1)
            result[k, length(med)+3] <- paste0(colnames(df)[i],"-",colnames(df)[j],": ",p.value)
      }
      }
      if(all(col_type_list=="bin")){
        if(!is.null(test)){
            p.value <- test$p.value
            mark.met <- c(mark.met,colnames(df)[i],"-",colnames(df)[j],": ")
            mark.list <- c(mark.list,p.value)
            mark.list.1 <- c(mark.list.1,paste0(colnames(df)[i],"-",colnames(df)[j],": ",p_value_formatted(p.value)))
            mark.list.met <- p.adjust(mark.list,"BH")
            result[k, length(med)+2] <- paste0(colnames(df)[i],"-",colnames(df)[j],": ",test1$estimate," [",
                                               round(test1$conf.int[1],1),"; ",round(test1$conf.int[2],1),"]")
            result[k, length(med)+3] <- " "
            if(k==tail(length(comb_list[[1]]), n=1)){
              mark.list.1 <- paste(mark.list.1,", ",p_value_formatted(mark.list.met),collapse = "\n")
              result[k, length(med)+3] <- mark.list.1
            }
          }
          if(is.null(test)){
            diff.1 <- "NA"
            p.value <- "NA"
            result[k, length(med)+2] <- paste0(colnames(df)[i],"-",colnames(df)[j],": ",diff.1)
            result[k, length(med)+3] <- paste0(colnames(df)[i],"-",colnames(df)[j],": ",p.value)
      }
        
      }
      if(all(col_type_list=="cat")){
          if(!is.null(test)){
            p.value <- test
            mark.met <- c(mark.met,colnames(df)[i],"-",colnames(df)[j],": ")
            mark.list <- c(mark.list,p.value)
            mark.list.1 <- c(mark.list.1,paste0(colnames(df)[i],"-",colnames(df)[j],": ",p.value))
            mark.list.met <- p.adjust(mark.list,"BH")
            result[k, length(med)+2] <- ""
            result[k, length(med)+3] <- " "
            if(k==tail(length(comb_list[[1]]), n=1)){
              mark.list.1 <- paste(mark.list.1,", ",p_value_formatted(mark.list.met),collapse = "\n")
              result[k, length(med)+3] <- mark.list.1
            }}
          if(is.null(test)){
            diff.1 <- "NA"
            p.value <- "NA"
            result[k, length(med)+2] <- ""
            result[k, length(med)+3] <- paste0(colnames(df)[i],"-",colnames(df)[j],": ",p.value)
      }
        
      }
    #досюда num
    
     
    }
    
    result[1, length(med)+2]=paste(result[,length(med)+2],collapse = "\n")
    result[1, length(med)+3]=paste(result[,length(med)+3],collapse = "\n")
    # if(g==tail(id_groups, n=1)){
    # result[2,length(med)]=paste(result[,length(med)],collapse = "\n")
    # }
    result <- result[complete.cases(result[ ,1]),]
    last.res <- tail(result,n=1)
    #result <- na.omit(result)
    
    result_list[[g]] <- result[complete.cases(result[ ,1]),]
  }
      

  result_new <- do.call(rbind,result_list)
  hh <- as.numeric(unique(unlist(comb_list)))
  #print(paste(comb_test))
  names(result_new) <- c("Группа",names.col,"Величина\nэффекта","p-уровень,\nкоррекция")
  
  res_ls[[current_index]] <- result_new
  item_index <- item_index + 1  # Увеличить индекс на 1 для следующего прохода item
  current_index <- current_index + 1
  }
  result_new1 <-do.call(rbind.data.frame, lapply(res_ls, function(x) unname(rbind(names(x), as.matrix(x)))))#rbindlist(res_ls)
  names(result_new1) <- result_new1[1,]
  result_new1 <- result_new1[-1,]
  #rbind(res_ls[[1]],names(res_ls[[2]]),setNames(res_ls[[2]],names(res_ls[[1]])))#do.call(rbind,res_ls)
  # names(result_new) <- c("GROUP",as.character(unique(unlist(colnames(data[,comb_test])))),"dif","p.value")
  #list.append(data_df[,i],0)
  na.list <- list("na", NA, NA, NA, NA)
  while(length(na.list)!=ncol(result_new1)){
    na.list <- list.append(na.list,NA)
  }
  #------------------------------------------------------------------------------------
  col.names = names(result_new1)
  s <- as.data.frame(na.list)
  names(s) <- col.names
  result_new1 <- rbind(s,result_new1)
  
  for(i in 1:nrow(result_new1)){
    if(result_new1[i,1]=="Группа"){
      result_new1[i, ] <- na.list
    }
  }
  col_name <- sapply(comb_list, function(x) {
    col_index <- x[1]  
    col_name <- colnames(data)[col_index]
    return(col_name)
  })
  col.title <- sapply(list_its, function(x) x$title)
  result_new1[result_new1$'Группа' == "na", "Группа"] <- col.title
  path.data = paste0(PATH, FILENAME,".csv")
  write.table(result_new1,file=path.data,sep=";",dec=",",na="",col.names = T,row.names = F)
  
  ff <- result_new1
  kl <- list()
  kl2 <- list()
  for(g in 1:nrow(ff)){
    if(is.na(ff[g,2])){
      kl <- list.append(kl,ff[g,])

    }
  }
  
  for(g in 1:nrow(ff)){
    if(!is.na(ff[g,2])){
      kl2 <- list.append(kl2,ff[g,])

    }
  }
  std_border = fp_border(color="black")
  kl.vec <- sapply(kl, \(x) as.numeric(row.names(x)))
  kl.vec2 <- sapply(kl2, \(x) as.numeric(row.names(x)))
  #------------------------------------------------------------------------------------
  bigborder <- fp_border(style = "solid", width=1)
  
  (result_new1 <- flextable(result_new1) %>%
      border_outer(part = "body",
                   border = fp_border(width = 1)) %>%
      surround(i = kl.vec2, j = c(1:ncol(ff)),                  
               border = fp_border_default(width = 1,color="black"), part = "body") %>%
      style(i = kl.vec,                                  
            pr_p = fp_par(text.align = "center", padding = 3)) %>%
      style(i = kl.vec2,            
            
            pr_p = fp_par(line_spacing = 1.5, padding = 3)) %>%
      border(border.bottom = bigborder,border.right = bigborder,border.left = bigborder, border.top = bigborder, part = "header")
    %>%
      width(width = .95,unit="in"))
  #----------------------------------
  #------------------------------------------------------------------------------------
   for(h in 1:length(kl.vec)){     
   result_new1 <- merge_at(result_new1,i=kl.vec[h],j = 1:ncol(ff)) %>%
    align(align = "center", part = "all") %>%
    border_inner(border = std_border)
   }
  return(result_new1)
}

f <- function(x) {
  tx <- table(x)
  px <- proportions(tx)
  paste(sprintf('%s - %s (%s%%)', names(tx), tx, round(px*100, 1)), collapse='\n')
}
