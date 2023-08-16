source("C:/Users/works/Documents/type_colomns_data.R")
source("C:/Users/works/Documents/standardize_data.R")
source("C:/Users/works/Documents/conduct_tests.R")
source("C:/Users/works/Documents/format_table_with_na_split.R")
source("C:/Users/works/Documents/check_arguments.R")
source("C:/Users/works/Documents/validate_list_its.R")

# Загружаем необходимые библиотеки
library(DescTools)
library(IDPmisc)
library(rlist)
library(flextable)
library(officer)
library(purrr)
library(magrittr)

# Определяем функцию dynamics
dynamics <- function(data,PATH,FILENAME,comp.flag=T,indicators.description,time.points.title=NULL){

  # Проверяем аргументы функции с помощью check_arguments
  check_arguments(data, PATH, FILENAME, comp.flag, indicators.description,time.points.title)
  
  # Валидируем описание индикаторов
  validate_indicators.description(indicators.description)
  
  #-----------------------------------------------------------------------------
  
  # Создаем вектор с длинами списков временных точек для каждого индикатора
  lengths <- sapply(indicators.description, function(x) length(x$time.point.names))
  # Проверяем, что все длины равны между собой
  all_lengths_equal <- length(unique(lengths)) == 1
  
  # Проверяем, что значения временных точек в первом и втором описании индикаторов идентичны  
  titles_identical <- identical(indicators.description[[1]]$time.point.names, indicators.description[[2]]$time.point.names)
  
  # Если длины временных точек одинаковы и их значения идентичны, то выбираем временные точки из первого описания
  if(all_lengths_equal && titles_identical) {
    
     unique_col_titles <- indicators.description[[1]]$time.point.names
     
  }else if (all_lengths_equal && !titles_identical){# Если длины временных точек одинаковы, но их значения разные, то объединяем все значения временных точек
  
    all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))
    unique_col_titles <- unique(all_titles)
  
  }else if(length(unique(lengths)) < length(lengths)){ # Если количество уникальных длин меньше общего количества индикаторов, значит, в разных индикаторах разное количество временных точек
    
  all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))
  unique_col_titles <- unique(all_titles) 
  
  } else if(length(unique(lengths)) == length(lengths)){ # Если все длины разные, то собираем все временные точки и делаем их уникальными
    
  #собиравем time.point.names со всех списков и делаем их уникальными
  all_titles <- unlist(lapply(indicators.description, function(x) x$time.point.names))  
  unique_col_titles <- unique(all_titles)
  
  }
  #----------------------------------------------------------------------------- 
  # Создаем пустой список для хранения результатов
  result_list <- list()
  
  # Создаем пустой список для хранения промежуточных результатов
  temp_results <- list()
  
  # Устанавливаем начальное значение индекса элемента
  item_index <- 1
  
  if(!is.null(time.points.title)){
  #создаем название точек на основе time.points.title если он не является пустым
  new_colnames <- setNames(time.points.title, seq_along(time.points.title))
  }
  
  # Перебор элементов в списке indicators.description:
  for(item in indicators.description){
    
    # Извлечение информации из item:
    #---------------------------------------------------------------------------
    # Извлекае список меток групп данных
    group.labels <- item$group.labels
    # Все комбинации столбцов данных (пары столбцов)
    comb_list <- combn(item$columns.id,2,simplify = FALSE)
    # Список столбцов данных
    comb_list_all <- item$columns.id
    # Текущий индекс элемента, который будет использоваться в дальнейшем 
    current_index <- item_index
    new_order <- as.character(item$time.point.indices)
    #---------------------------------------------------------------------------
  # Цикл по меткам групп данных
  for(g in group.labels){
    
    comb_test <- unlist(comb_list)
    print(paste("22",comb_test))
    
    # Формирование матрицы данных df на основе выбранной группы g
    df <- as.matrix(data[data[[item$group.col.id]] %in% g,])

    if(!is.null(time.points.title)){
    # Создание датафрейма df_order с индексами столбцов и временными точками
    df_order <- data.frame(columns.id = item$columns.id, time.point.indices = item$time.point.indices)
    # Сортировка df_order по временным точкам для правильного порядка столбцов
    df_order <- df_order[order(df_order$time.point.indices), ]
    # Создание новой матрицы df с отсортированными столбцами
    df <- df[, df_order$columns.id]
    # Замена имен столбцов на индексы временных точек
    colnames(df) <- df_order$time.point.indices
    # Замена имен столбцов на новые имена из time.points.title
    colnames(df) <- new_colnames[colnames(df)]
    # Создание новой матрицы df_full с пробелами
    df_full <- data.frame(matrix(" ", nrow = nrow(df), ncol = length(time.points.title)))
    # Присвоение имен столбцам df_full на основе time.points.title
    colnames(df_full) <- time.points.title
    # Нахождение пересечения имен столбцов для выравнивания данных
    intersect_cols <- intersect(colnames(df_full), colnames(df)) 
    intersect_cols <- as.character(intersect_cols)
    # Заполнение соответствующих ячеек в df_full данными из df
    df_full[,intersect_cols] <- df[,intersect_cols]
    # Присвоение имен столбцам df_full на основе time.points.title
    colnames(df_full) <- time.points.title
    # Преобразование df_full обратно в матрицу
    df <- as.matrix(df_full)
    
    }
    if(is.null(time.points.title)){
    # Создание списка уникальных индексов столбцов из item$columns.id
    df_cols <- unique(unlist(item$columns.id))
    # Выбор только тех столбцов в матрице df, которые указаны в df_cols
    df <- df[,df_cols]
    # Присвоение имен столбцам df имена временных точек из item$time.point.names
    colnames(df) <- item$time.point.names
    # Создание новой матрицы df_full с пустыми ячейками  
    df_full <- data.frame(matrix(" ", nrow = nrow(df), ncol = length(unique_col_titles)))
    # Присвоение имен столбцам df_full имена из unique_col_titles
    colnames(df_full) <- unique_col_titles
    # Нахождение пересечения имен столбцов для выравнивания данных
    intersect_cols <- intersect(colnames(df_full), colnames(df))
    intersect_cols <- as.character(intersect_cols)
    # Заполнение соответствующих ячеек в df_full данными из df
    df_full[,intersect_cols] <- df[,intersect_cols]
    # Присвоение имен столбцам df_full имена из unique_col_titles
    colnames(df_full) <- unique_col_titles
    # Преобразование df_full обратно в матрицу для дальнейшей обработки
    df <- as.matrix(df_full)
    }
    # Создание вектора col_type_list, в котором тип столбца (item$type) дублируется для каждой комбинации
    col_type_list <- rep(item$type, length(comb_list_all))
    # Преобразование вектора col_type_list в обычный вектор
    col_type_list <- unlist(col_type_list)
    print(col_type_list)
    # Создание списка common.list, который содержит пары столбцов и их типы
    common.list <- lapply(1:length(comb_list_all), function(x) list(id = comb_list_all[[x]], type = col_type_list[[x]]))
    print(paste("1111",common.list))


    # Если comp.flag равен TRUE, то выполняется сравнение групп с помощью функции cmp.group.all
    if(comp.flag==T){
    sr <- cmp.group.all(group.col.id     = item$group.col.id,
                  group.labels = group.labels,
                  columns.list = common.list,
                  data         = data,
                  save.file.cmp.2groups    = F,
                  PATH         = path.dir.out,
                  FILENAME     = "FILE",
                  result = NULL)
    # Извлечение столбца результатов из sr и преобразование его в вектор
    rr <- as.matrix(sr[,5])
    rr <- as.vector(rr)

    }
    #
    # #---------------------------------------------------------------------------
    # Стандартизация данных в матрице df с помощью функции standardize_data
    df <- standardize_data(df)
    # #---------------------------------------------------------------------------

    if(comp.flag==T){
      # Нахождение индексов пустых ячеек в матрице df
      empty_cols <- which(df == " ", arr.ind = TRUE)
      # Извлечение уникальных индексов столбцов с пустыми ячейками
      insert_idxs <- unique(empty_cols[,2])
      # Проверка, есть ли пустые ячейки, и если да, то обновление вектора rr
      if(length(insert_idxs)!=0){
      max_idx <- length(rr) + length(insert_idxs)
      new_rr <- character(max_idx)
      rr_idx <- 1
      i <- 1
      # Проход по индексам элементов в векторе new_rr
       for (i in 1:max_idx) {
        if(i %in% insert_idxs) {
          new_rr[i] <- "-"
        } else {
          # Проверка, не превышено ли количество элементов в векторе rr
          if(rr_idx <= length(rr)) {
            new_rr[i] <- rr[rr_idx]
            rr_idx <- rr_idx + 1
          }
        }
       }
      # Обновление вектора rr с учетом временных точек
      if(is.null(time.points.title)){
      rr <- new_rr
      } else{
      rr_order <- new_rr
      idx <- rep(NA, length(rr_order))
      idx[item$time.point.indices] <- 1:length(item$time.point.indices)
      rr_order_new <- rr_order[idx]
      rr_order_new[!is.na(idx)] <- rr_order[item$time.point.indices]
      rr_order_new <- ifelse(is.na(rr_order_new), "-", rr_order_new)
      rr <- rr_order_new 
      }
      }
    }
    
    #---------------------------------------------------------------------------
    # Считаем сатистику изходя из col_type_list
    group_summary_results <- type_columns_data(df, col_type_list)
    # Обновляем df данными из group_summary_results
    df <- group_summary_results$df
    # Сохраняем статистику по столбцам из group_summary_results
    column_stats <- group_summary_results$column_stats
    #---------------------------------------------------------------------------
    # Создаем матрицу данных (data_df) для текущей группы (g) из исходных данных (data)
    data_df = as.matrix(data[data[[item$group.col.id]] %in% g,])
    # Извлекаем данные только для тех столбцов, которые присутствуют в комбинациях comb_list
    data_df_naomit = data_df[,unique(unlist(comb_list))]
    # Создаем матрицу df.matrix, в которой для каждой строки показывается количество отсутствующих значений
    # в соответствующих столбцах из data_df_naomit
    df.matrix <- ifelse(is.matrix(data_df_naomit),na.count.string(data_df_naomit[,1]),na.count.string(data_df_naomit[,1]))
    # Создаем фрейм данных (result) для хранения результатов анализа текущей группы (g)
    # Количество столбцов в result определяется количеством статистических метрик (column_stats) плюс 2
    result <- data.frame(matrix(NA,ncol=length(column_stats)+2,nrow=1))
    # Заполняем первую строку фрейма данных (result) комбинацией текста, описывающего текущую группу (g),
    # количеством строк в data_df (N=...) и статистическими метриками (column_stats)
    result[1,] <- c(paste0(g,"\nN=",df.matrix),column_stats,"")
    
    if(comp.flag==T){
    # Проверяем, является ли текущая группа (g) последней в списке group.labels
    if(g==tail(group.labels, n=1)){
      # Вычисляем номер последней строки в результате и добавляем еще одну строку с NA
      last_row <- nrow(result)+1
      result <- rbind(result, rep(NA, ncol(result))) 
      # Заполняем первую ячейку в последней строке как "Сравнение:"
      result[last_row, 1] <- "Сравнение:"
      # Добавляем к вектору rr NA, чтобы сделать его такой же длины, как и строка результатов
      rr <- c(rr, rep(NA, ncol(result) - length(rr)-1))
      # Заполняем оставшиеся ячейки в последней строке результатами сравнения (rr)
    for(i in 1:length(rr)){
     result[last_row, i+1] <- rr[i] 
     }
    }
    }
    # Генерируем все возможные комбинации столбцов для анализа попарных различий
    pairwise_combinations <- unlist(lapply(ncol(df), combn, 2, simplify=FALSE), recursive=FALSE) 
    
    #---------------------------------------------------------------------------
    # Считаем статистику и тесты в зависимости от типа данных
    result <- conduct_tests(data_df,df, col_type_list, column_stats,result,pairwise_combinations)
    #---------------------------------------------------------------------------
    # Заполняем ячейку в первой строке для объединения результатов (первый столбец)
    result[1, length(column_stats)+2] <- paste(result[,length(column_stats)+2], collapse="\n")
    # Заполняем ячейку в первой строке для объединения результатов (второй столбец)
    result[1, length(column_stats)+3] <- paste(result[,length(column_stats)+3], collapse="\n")
    # Проверяем, если текущая группа (g) - последняя в списке group.labels
    if(g==tail(group.labels, n=1)){
    # Получаем индексы столбцов для последних двух столбцов в результате
    col1_idx <- ncol(result) - 1
    col2_idx <- ncol(result)
    # Индекс строки, куда будем вставлять NA для сравнения
    row_idx <- 2
    # Заполняем ячейку второго столбца для этой строки значением NA
    result[row_idx, col1_idx] <- NA
    # Заполняем ячейку третьего столбца для этой строки значением NA
    result[row_idx, col2_idx] <- NA
    }
    # Оставляем только строки с полными данными в первом столбце (не NA)
    result <- result[complete.cases(result[ ,1]),]
    # Извлекаем последнюю строку в результате как last.res
    last.res <- tail(result,n=1)
    # Сохраняем последний результат для текущей группы в список результатов
    result_list[[g]] <- result[complete.cases(result[ ,1]),]
  }
      
  # Объединяем результаты для каждой группы в одну большую таблицу
  result_new <- do.call(rbind,result_list)
  # Получаем уникальные значения столбцов из comb_list и преобразуем их в числовой формат
  hh <- as.numeric(unique(unlist(comb_list)))
  # Создаем имена для столбцов таблицы result_new, включая названия групп, временных точек, величину эффекта и p-уровень коррекции
  names(result_new) <- c("Группа",unique_col_titles,"Величина эффекта","p-уровень,\nкоррекция")
  # Сохраняем полученные результаты для текущего набора параметров во временный список temp_results
  temp_results[[current_index]] <- result_new
  # Увеличиваем индекс элемента и текущий индекс на 1
  item_index <- item_index + 1  
  current_index <- current_index + 1
  }
  # Удаляем первую строку из final_results_table, так как она использовалась только для временных названий столбцов
  final_results_table <-do.call(rbind.data.frame, lapply(temp_results, function(x) unname(rbind(names(x), as.matrix(x)))))
  names(final_results_table) <- final_results_table[1,]
  final_results_table <- final_results_table[-1,]
  # Создаем список na.list для добавления строк с NA
  na.list <- list("na", NA, NA, NA, NA)
  # Пока длина списка na.list не станет равной количеству столбцов в final_results_table, продолжаем добавлять NA
  while(length(na.list)!=ncol(final_results_table)){
    na.list <- list.append(na.list,NA)
  }
  #------------------------------------------------------------------------------------
  # Получаем имена столбцов из final_results_table
  col.names = names(final_results_table)
  # Создаем DataFrame с наличием NA значений на основе списка na.list
  s <- as.data.frame(na.list)
  # Задаем имена столбцам DataFrame s на основе имен столбцов final_results_table
  names(s) <- col.names
  # Объединяем DataFrame s с final_results_table, чтобы добавить строки с NA в начале
  final_results_table <- rbind(s,final_results_table)
  # Проходим по каждой строке таблицы final_results_table
  for(i in 1:nrow(final_results_table)){
    # Если значение в первой колонке строки равно "Группа"
    if(final_results_table[i,1]=="Группа"){
      # Заменяем всю строку на значения из списка na.list
      final_results_table[i, ] <- na.list
    }
  }
  # Получаем имена колонок на основе comb_list
  col_name <- sapply(comb_list, function(x) {
    col_index <- x[1]  
    col_name <- colnames(data)[col_index]
    return(col_name)
  })
  # Получаем заголовки групп из indicators.description
  col.title <- sapply(indicators.description, get_new_group)
  # Заменяем "na" значения в колонке "Группа" на заголовки групп
  final_results_table[final_results_table$'Группа' == "na", "Группа"] <- col.title
  # Создаем путь к файлу, в который будем записывать final_results_table в формате CSV
  path.data = paste0(PATH, FILENAME,".csv")
  # Записываем final_results_table в CSV файл
  write.table(final_results_table,file=path.data,sep=";",dec=",",na="",col.names = T,row.names = F)
  
  #-----------------------------------------------------------------------------
  # Формирование финальной таблицы в word
  final_results_table <- format_table_with_na_split(final_results_table)
  #-----------------------------------------------------------------------------
  
  return(final_results_table)
}