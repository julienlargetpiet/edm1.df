#' @import stringr openxlsx stringi stats utils dplyr
#' @title edm1

#' insert_datf
#'
#' @description Allow to insert dataframe into another dataframe according to coordinates (row, column) from the dataframe that will be inserted
#' @param datf_in is the dataframe that will be inserted 
#' @param datf_ins is the dataset to be inserted
#' @param ins_loc is a vector containg two parameters (row, column) of the begining for the insertion
#' @examples 
#'
#'datf1 <- data.frame(c(1, 4), c(5, 3))
#'
#'datf2 <- data.frame(c(1, 3, 5, 6), c(1:4), c(5, 4, 5, "ereer"))
#'
#'print(insert_datf(datf_in=datf2, datf_ins=datf1, ins_loc=c(4, 2)))
#'
#'#   c.1..3..5..6. c.1.4. c.5..4..5...ereer..
#'# 1             1      1                   5
#'# 2             3      2                   4
#'# 3             5      3                   5
#'# 4             6      1                   5
#'
#'print(insert_datf(datf_in=datf2, datf_ins=datf1, ins_loc=c(3, 2)))
#'
#'#   c.1..3..5..6. c.1.4. c.5..4..5...ereer..
#'# 1             1      1                   5
#'# 2             3      2                   4
#'# 3             5      1                   5
#'# 4             6      4                   3
#'
#'print(insert_datf(datf_in=datf2, datf_ins=datf1, ins_loc=c(2, 2)))
#'
#'#   c.1..3..5..6. c.1.4. c.5..4..5...ereer..
#'# 1             1      1                   5
#'# 2             3      1                   5
#'# 3             5      4                   3
#'# 4             6      4               ereer
#'
#' @export

insert_datf <- function(datf_in, datf_ins, ins_loc){

  ins_loc <- ins_loc - 1
  
  datf_pre1 <- datf_in[0:ins_loc[1], 1:ncol(datf_in)] 
 
  if ((ins_loc[1] + nrow(datf_ins)) > nrow(datf_in)){
    
    datf_pre2 <- datf_in[(ins_loc[1]+1):nrow(datf_in), 1:ncol(datf_in)]
    
    datf_pre3 <- datf_in[0:0, 1:ncol(datf_in)]
    
    row_end <- nrow(datf_pre2)
    
  }else{
   
    datf_pre2 <- datf_in[(ins_loc[1]+1):(ins_loc[1]+nrow(datf_ins)), 1:ncol(datf_in)]
   
    if ((ins_loc[1]+nrow(datf_ins)) < nrow(datf_in)){

        datf_pre3 <- datf_in[(ins_loc[1] + nrow(datf_ins) + 1):nrow(datf_in), 1:ncol(datf_in)]
    
    }else {

        datf_pre3 <- datf_in[0:0, 1:ncol(datf_in)]

    }

    row_end <- nrow(datf_ins)
    
  }
  
  t = 1
  
  for (i in 1:ncol(datf_ins)){
    
    datf_pre2[, (ins_loc[2]+i)] <- datf_ins[1:row_end, t] 
    
    t = t + 1
    
  }
  
  rtnl <- rbind(datf_pre1, datf_pre2, datf_pre3)
  
  return(rtnl)
  
}

#' vlookup_datf
#'
#' Alow to perform a vlookup on a dataframe
#' @param datf is the input dataframe
#' @param v_id is a vector containing the ids
#' @param col_id is the column that contains the ids (default is equal to 1)
#' @param included_col_id is if the result should return the col_id (default set to yes)
#' @examples
#'
#' datf1 <- data.frame(c("az1", "az3", "az4", "az2"), c(1:4), c(4:1))
#' 
#' print(vlookup_datf(datf=datf1, v_id=c("az1", "az2", "az3", "az4")))
#'
#' #   c..az1....az3....az4....az2.. c.1.4. c.4.1.
#' #2                            az1      1      4
#' #4                            az2      4      1
#' #21                           az3      2      3
#' #3                            az4      3      2
#'
#' @export

vlookup_datf <- function(datf, v_id, col_id=1, included_col_id="yes"){
  
  rtnl <- datf[1, ]
  
  for (i in 1:length(v_id)){

    idx = match(v_id[i], datf[, col_id])
    
    rtnl <- rbind(rtnl, datf[idx,])
    
    datf <- datf[-idx, ]
    
  }
  
  if (included_col_id == "yes"){
  
    return(rtnl[-1, ])
  
  }else{
    
    return(rtnl[-1, -col_id])
    
  }
    
}

#' see_datf
#' 
#' Allow to return a dataframe with special value cells (ex: TRUE) where the condition entered are respected and another special value cell (ex: FALSE) where these are not
#' @param datf is the input dataframe
#' @param condition_l is the vector of the possible conditions ("==", ">", "<", "!=", "%%", "reg", "not_reg", "sup_nchar", "inf_nchar", "nchar") (equal to some elements in a vector, greater than, lower than, not equal to, is divisible by, the regex condition returns TRUE, the regex condition returns FALSE, the length of the elements is strictly superior to X, the length of the element is strictly inferior to X, the length of the element is equal to one element in a vector), you can put the same condition n times. 
#' @param val_l is the list of vectors containing the values or vector of values related to condition_l (so the vector of values has to be placed in the same order)
#' @param conjunction_l contains the and or conjunctions, so if the length of condition_l is equal to 3, there will be 2 conjunctions. If the length of conjunction_l is inferior to the length of condition_l minus 1, conjunction_l will match its goal length value with its last argument as the last arguments. For example, c("&", "|", "&") with a goal length value of 5 --> c("&", "|", "&", "&", "&")
#' @param rt_val is a special value cell returned when the conditions are respected
#' @param f_val is a special value cell returned when the conditions are not respected
#' @details This function will return an error if number only comparative conditions are given in addition to having character values in the input dataframe.
#' @examples
#' 
#' datf1 <- data.frame(c(1, 2, 4), c("a", "a", "zu"))
#' 
#' print(see_datf(datf=datf1, condition_l=c("nchar"), val_l=list(c(1))))
#' 
#' #    X1    X2
#' #1 TRUE  TRUE
#' #2 TRUE  TRUE
#' #3 TRUE FALSE
#' 
#' print(see_datf(datf=datf1, condition_l=c("=="), val_l=list(c("a", 1))))
#' 
#' #    X1    X2
#' #1  TRUE  TRUE
#' #2 FALSE  TRUE
#' #3 FALSE FALSE
#'
#' 
#' print(see_datf(datf=datf1, condition_l=c("nchar"), val_l=list(c(1, 2))))
#' 
#' #    X1   X2
#' #1 TRUE TRUE
#' #2 TRUE TRUE
#' #3 TRUE TRUE
#'
#' print(see_datf(datf=datf1, condition_l=c("not_reg"), val_l=list("[a-z]")))
#' 
#' #    X1    X2
#' #1 TRUE FALSE
#' #2 TRUE FALSE
#' #3 TRUE FALSE
#'
#' @export

see_datf <- function(datf, condition_l, val_l, conjunction_l=c(), rt_val=TRUE, f_val=FALSE){

        if (length(condition_l) > 1 & length(conjunction_l) < (length(condition_l) - 1)){

                for (i in (length(conjunction_l)+1):length(condition_l)){

                        conjunction_l <- append(conjunction_l, conjunction_l[length(conjunction_l)])

                }

        }

        datf_rtnl <- data.frame(matrix(f_val, ncol=ncol(datf), nrow=nrow(datf)))

        all_op <- c("==", ">", "<", "!=", "%%", "reg", "not_reg", "sup_nchar", "inf_nchar", "nchar")

        for (I in 1:ncol(datf)){

                for (i in 1:nrow(datf)){

                        checked_l <- c()

                        previous = 1

                        for (t in 1:length(condition_l)){

                                already <- 0

                                if (condition_l[t] == "==" & already == 0){

                                        if (datf[i, I] %in% unlist(val_l[t])){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == ">" & already == 0){

                                        if (all(datf[i, I] > unlist(val_l[t])) == TRUE){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "<" & already == 0){

                                        if (all(datf[i, I] < unlist(val_l[t]))){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "!=" & already == 0){

                                        if (!(datf[i, I] %in% unlist(val_l[t])) == TRUE){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "%%" & already == 0){

                                        if (sum(datf[i, I] %% unlist(val_l[t])) == 0){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                } else if (condition_l[t] == "reg" & already == 0){

                                        if (str_detect(datf[i, I], unlist(val_l[t]))){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }  else if (condition_l[t] == "not_reg" & already == 0){

                                        if ((str_detect(datf[i, I], unlist(val_l[t]))) == FALSE ){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }  else if (condition_l[t] == "sup_nchar" & already == 0){

                                        if (nchar(as.character(datf[i, I])) > unlist(val_l[t])){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }  else if (condition_l[t] == "inf_nchar" & already == 0){

                                        if (nchar(as.character(datf[i, I])) < unlist(val_l[t])){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }

                                if (condition_l[t] == "nchar" & already == 0){

                                        if (nchar(as.character(datf[i, I])) %in% unlist(val_l[t])){

                                                checked_l <- append(checked_l, TRUE)

                                                if (length(condition_l) > 1 & t > 1){

                                                        bfr <- conjunction_l[previous:t]

                                                        if (t == length(condition_l)){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }else if (conjunction_l[t] == "|"){

                                                                if (length(checked_l) == length(bfr)){

                                                                        already <- 1

                                                                        datf_rtnl[i, I] <- rt_val

                                                                }

                                                        }

                                                }else if (length(condition_l) == 1){

                                                        datf_rtnl[i, I] <- rt_val

                                                }else {

                                                        if (conjunction_l[1] == "|"){

                                                                datf_rtnl[i, I] <- rt_val

                                                                checked_l <- c()

                                                        }

                                                }

                                        }

                                        if (t <= length(conjunction_l)){ 

                                                if (conjunction_l[t] == "|"){

                                                        checked_l <- c()

                                                        previous = t + 1 

                                                }

                                        }

                                }

                        }
                        
                }

        }

  return(datf_rtnl)

}

#' val_replacer
#' 
#' Allow to replace value from dataframe to another one.
#'
#' @param datf is the input dataframe
#' @param val_replaced is a vector of the value(s) to be replaced
#' @param val_replacor is the value that will replace val_replaced
#' @examples
#'
#' print(val_replacer(datf=data.frame(c(1, "oo4", TRUE, FALSE), c(TRUE, FALSE, TRUE, TRUE)), 
#'      val_replaced=c(TRUE), val_replacor="NA"))
#'
#' #  c.1...oo4...T..F. c.T..F..T..T.
#' #1                 1            NA
#' #2               oo4         FALSE
#' #3                NA            NA
#' #4             FALSE            NA
#' 
#' @export

val_replacer <- function(datf, val_replaced, val_replacor=TRUE){
  
  for (i in 1:(ncol(datf))){
    
      for (i2 in 1:length(val_replaced)){
        
        vec_pos <- grep(val_replaced[i2], datf[, i])
          
        datf[vec_pos, i] <- val_replacor
    
      }
    
  }
  
  return(datf)
  
}

#' nestr_datf2
#'
#' Allow to write a special value (1a) in the cells of a dataframe (1b) that correspond (row and column) to whose of another dataframe (2b) that return another special value (2a). The cells whose coordinates do not match the coordinates of the dataframe (2b), another special value can be written (3a) if not set to NA. 
#' @param inptf_datf is the input dataframe (1b)
#' @param rtn_pos is the special value (1a)
#' @param rtn_neg is the special value (3a) 
#' @param nestr_datf is the dataframe (2b)
#' @param yes_val is the special value (2a) 
#' @examples
#'
#' print(nestr_datf2(inptf_datf=data.frame(c(1, 2, 1), c(1, 5, 7)), rtn_pos="yes", 
#' rtn_neg="no", nestr_datf=data.frame(c(TRUE, FALSE, TRUE), c(FALSE, FALSE, TRUE)), yes_val=TRUE)) 
#'
#' #  c.1..2..1. c.1..5..7.
#' #1        yes         no
#' #2         no         no
#' #3        yes        yes
#' 
#' @export

nestr_datf2 <- function(inptf_datf, rtn_pos, rtn_neg=NA, nestr_datf, yes_val=T){

        if (is.na(rtn_neg)){

                for (I in 1:ncol(nestr_datf)){

                        for (i in 1:nrow(nestr_datf)){

                                if (nestr_datf[i, I] == yes_val){

                                        inptf_datf[i, I] <- rtn_pos

                                }

                        }

                }

        }else{

                for (I in 1:ncol(nestr_datf)){

                        for (i in 1:nrow(nestr_datf)){

                                if (nestr_datf[i, I] == yes_val){

                                        inptf_datf[i, I] <- rtn_pos

                                }else{

                                        inptf_datf[i, I] <- rtn_neg

                                }

                        }

                }

        }

    return(inptf_datf)

}

#' nestr_datf1
#'
#' Allow to write a value (1a) to a dataframe (1b) to its cells that have the same coordinates (row and column) than the cells whose value is equal to a another special value (2a), from another another dataframe (2b). The value (1a) depends of the cell  value coordinates of the third dataframe (3b). If a cell coordinates (1c) of the first dataframe (1b) does not correspond to the coordinates of a good returning cell value (2a) from the dataframe (2b), so this cell (1c) can have its value changed to the same cell coordinates value (3a) of a third dataframe (4b), if (4b) is not set to NA.
#' @param inptf_datf is the input dataframe (1b)
#' @param inptt_pos_datf is the dataframe (2b) that corresponds to the (1a) values
#' @param inptt_neg_datf is the dataframe (4b) that has the (3a) values, defaults to NA
#' @param nestr_datf is the dataframe (2b) that has the special value (2a)
#' @param yes_val is the special value (2a)
#' @examples
#'
#' print(nestr_datf1(inptf_datf=data.frame(c(1, 2, 1), c(1, 5, 7)), 
#' inptt_pos_datf=data.frame(c(4, 4, 3), c(2, 1, 2)), 
#' inptt_neg_datf=data.frame(c(44, 44, 33), c(12, 12, 12)), 
#' nestr_datf=data.frame(c(TRUE, FALSE, TRUE), c(FALSE, FALSE, TRUE)), yes_val=TRUE)) 
#'
#' #  c.1..2..1. c.1..5..7.
#' #1          4         12
#' #2         44         12
#' #3          3          2
#'
#' print(nestr_datf1(inptf_datf=data.frame(c(1, 2, 1), c(1, 5, 7)), 
#' inptt_pos_datf=data.frame(c(4, 4, 3), c(2, 1, 2)), 
#' inptt_neg_datf=NA, 
#' nestr_datf=data.frame(c(TRUE, FALSE, TRUE), c(FALSE, FALSE, TRUE)), yes_val=TRUE))
#'
#' #   c.1..2..1. c.1..5..7.
#' #1          4          1
#' #2          2          5
#' #3          3          2
#' 
#' @export

nestr_datf1 <- function(inptf_datf, inptt_pos_datf, nestr_datf, yes_val=TRUE, inptt_neg_datf=NA){

        if (all(is.na(inptt_neg_datf)) == TRUE){

                for (I in 1:ncol(nestr_datf)){

                        for (i in 1:nrow(nestr_datf)){

                                if (nestr_datf[i, I] == yes_val){

                                        inptf_datf[i, I] <- inptt_pos_datf[i, I]

                                }

                        }

                }

        }else{

                for (I in 1:ncol(nestr_datf)){

                        for (i in 1:nrow(nestr_datf)){

                                if (nestr_datf[i, I] == yes_val){

                                        inptf_datf[i, I] <- inptt_pos_datf[i, I]

                                }else{

                                        inptf_datf[i, I] <- inptt_neg_datf[i, I]

                                }

                        }

                }

        }

    return(inptf_datf)

}

#' groupr_datf
#' 
#' Allow to create groups from a dataframe. Indeed, you can create conditions that lead to a flag value for each cell of the input dataframeaccording to the cell value. This function is based on see_datf and nestr_datf2 functions.
#' @param inpt_datf is the input dataframe
#' @param condition_lst is a list containing all the condition as a vector for each group
#' @param val_lst is a list containing all the values associated with condition_lst as a vector for each group
#' @param conjunction_lst is a list containing all the conjunctions associated with condition_lst and val_lst as a vector for each group
#' @param rtn_val_pos is a vector containing all the group flag value like this ex: c("flag1", "flag2", "flag3") 
#' @export
#' @examples interactive()
#' 
#' datf1 <- data.frame(c(1, 2, 1), c(45, 22, 88), c(44, 88, 33))
#'                                                                       
#' val_lst <- list(list(c(1), c(1)), list(c(2)), list(c(44, 88)))
#' 
#' condition_lst <- list(c(">", "<"), c("%%"), c("==", "=="))
#' 
#' conjunction_lst <- list(c("|"), c(), c("|"))
#' 
#' rtn_val_pos <- c("+", "++", "+++")
#' 
#' print(groupr_datf(inpt_datf=datf1, val_lst=val_lst, condition_lst=condition_lst, 
#' conjunction_lst=conjunction_lst, rtn_val_pos=rtn_val_pos))
#' 
#' #    X1  X2  X3
#' #1 <NA>   + +++
#' #2   ++  ++ +++
#' #3 <NA> +++   +
#' 
#' @export

groupr_datf <- function(inpt_datf, condition_lst, val_lst, conjunction_lst, rtn_val_pos=c()){
 
        nestr_datf2 <- function(inptf_datf, rtn_pos, rtn_neg=NA, nestr_datf, yes_val=TRUE){

                if (is.na(rtn_neg)){

                        for (I in 1:ncol(nestr_datf)){

                                for (i in 1:nrow(nestr_datf)){

                                        if (nestr_datf[i, I] == yes_val){

                                                inptf_datf[i, I] <- rtn_pos

                                        }

                                }

                        }

                }else{

                        for (I in 1:ncol(nestr_datf)){

                                for (i in 1:nrow(nestr_datf)){

                                        if (nestr_datf[i, I] == yes_val){

                                                inptf_datf[i, I] <- rtn_pos

                                        }else{

                                                inptf_datf[i, I] <- rtn_neg

                                        }

                                }

                        }

                }

            return(inptf_datf)

        }
 
        see_datf <- function(datf, condition_l, val_l, conjunction_l=c(), rt_val=TRUE, f_val=FALSE){

                if (length(condition_l) > 1 & length(conjunction_l) < (length(condition_l) - 1)){

                        for (i in (length(conjunction_l)+1):length(condition_l)){

                                conjunction_l <- append(conjunction_l, conjunction_l[length(conjunction_l)])

                        }

                }

                datf_rtnl <- data.frame(matrix(f_val, ncol=ncol(datf), nrow=nrow(datf)))

                all_op <- c("==", ">", "<", "!=", "%%")

                for (I in 1:ncol(datf)){

                        for (i in 1:nrow(datf)){

                                checked_l <- c()

                                previous = 1

                                for (t in 1:length(condition_l)){

                                        already <- 0

                                        if (condition_l[t] == "==" & already == 0){

                                                if (datf[i, I] %in% unlist(val_l[t])){

                                                        checked_l <- append(checked_l, TRUE)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                datf_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        datf_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == ">" & already == 0){

                                                if (all(datf[i, I] > unlist(val_l[t])) == TRUE){

                                                        checked_l <- append(checked_l, TRUE)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                datf_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        datf_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == "<" & already == 0){

                                                if (all(datf[i, I] < unlist(val_l[t]))){

                                                        checked_l <- append(checked_l, TRUE)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                datf_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        datf_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == "!=" & already == 0){

                                                if (!(datf[i, I] %in% unlist(val_l[t])) == TRUE){

                                                        checked_l <- append(checked_l, TRUE)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                datf_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        datf_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                        if (condition_l[t] == "%%" & already == 0){

                                                if (sum(datf[i, I] %% unlist(val_l[t])) == 0){

                                                        checked_l <- append(checked_l, TRUE)

                                                        if (length(condition_l) > 1 & t > 1){

                                                                bfr <- conjunction_l[previous:t]

                                                                if (t == length(condition_l)){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }else if (conjunction_l[t] == "|"){

                                                                        if (length(checked_l) == length(bfr)){

                                                                                already <- 1

                                                                                datf_rtnl[i, I] <- rt_val

                                                                        }

                                                                }

                                                        }else if (length(condition_l) == 1){

                                                                datf_rtnl[i, I] <- rt_val

                                                        }else {

                                                                if (conjunction_l[1] == "|"){

                                                                        datf_rtnl[i, I] <- rt_val

                                                                        checked_l <- c()

                                                                }

                                                        }

                                                }

                                                if (t <= length(conjunction_l)){ 

                                                        if (conjunction_l[t] == "|"){

                                                                checked_l <- c()

                                                                previous = t + 1 

                                                        }

                                                }

                                        }

                                }
                                
                        }

                }

          return(datf_rtnl)

        }
              
        rtn_datf <- data.frame(matrix(nrow=nrow(inpt_datf), ncol=ncol(inpt_datf)))

        for (I in 1:length(condition_lst)){

                pre_datf <- see_datf(datf=inpt_datf, condition_l=unlist(condition_lst[I]), val_l=unlist(val_lst[I]), conjunction_l=unlist(conjunction_lst[I])) 

                rtn_datf <- nestr_datf2(inptf_datf=rtn_datf, nestr_datf=pre_datf, rtn_pos=rtn_val_pos[I], rtn_neg=NA)  

        }

        return(rtn_datf)

}

#' paste_datf
#' 
#' Return a vector composed of pasted elements from the input dataframe at the same index.
#' @param inpt_datf is the input dataframe
#' @param sep is the separator between pasted elements, defaults to ""
#' @examples
#' 
#' print(paste_datf(inpt_datf=data.frame(c(1, 2, 1), c(33, 22, 55))))
#'
#' #[1] "133" "222" "155"
#'
#' @export

paste_datf <- function(inpt_datf, sep=""){

    if (ncol(as.data.frame(inpt_datf)) == 1){ 

        return(inpt_datf) 

    }else {

        rtn_datf <- inpt_datf[,1]

        for (i in 2:ncol(inpt_datf)){

            rtn_datf <- paste(rtn_datf, inpt_datf[,i], sep=sep)

        }

        return(rtn_datf)

    }

}

#' cut_v
#'
#' Allow to convert a vector to a dataframe according to a separator.
#' 
#' @param inpt_v is the input vector
#' @param sep_ is the separator of the elements in inpt_v, defaults to ""
#' 
#' @examples
#'
#' print(cut_v(inpt_v=c("oui", "non", "oui", "non")))
#' 
#' #    X.o. X.u. X.i.
#' #oui "o"  "u"  "i" 
#' #non "n"  "o"  "n" 
#' #oui "o"  "u"  "i" 
#' #non "n"  "o"  "n" 
#' 
#' print(cut_v(inpt_v=c("ou-i", "n-on", "ou-i", "n-on"), sep_="-"))
#' 
#' #     X.ou. X.i.
#' #ou-i "ou"  "i" 
#' #n-on "n"   "on"
#' #ou-i "ou"  "i" 
#' #n-on "n"   "on"
#' 
#' @export

cut_v <- function(inpt_v, sep_=""){

        rtn_datf <- data.frame(matrix(data=NA, nrow=0, ncol=length(unlist(strsplit(inpt_v[1], split=sep_)))))

        for (el in inpt_v){ rtn_datf <- rbind(rtn_datf, unlist(strsplit(el, split=sep_))) }

        return(rtn_datf)

}

#' wider_datf
#'
#' Takes a dataframe as an input and the column to split according to a seprator.
#'
#' @param inpt_datf is the input dataframe
#' @param col_to_splt is a vector containing the number or the colnames of the columns to split according to a separator
#' @param sep_ is the separator of the elements to split to new columns in the input dataframe 
#' @examples
#'
#' datf1 <- data.frame(c(1:5), c("o-y", "hj-yy", "er-y", "k-ll", "ooo-mm"), c(5:1))
#' 
#' datf2 <- data.frame("col1"=c(1:5), "col2"=c("o-y", "hj-yy", "er-y", "k-ll", "ooo-mm"))
#'  
#' print(wider_datf(inpt_datf=datf1, col_to_splt=c(2), sep_="-"))
#'
#' #       pre_datf X.o.  X.y.  
#' #o-y    1      "o"   "y"  5
#' #hj-yy  2      "hj"  "yy" 4
#' #er-y   3      "er"  "y"  3
#' #k-ll   4      "k"   "ll" 2
#' #ooo-mm 5      "ooo" "mm" 1
#'
#' print(wider_datf(inpt_datf=datf2, col_to_splt=c("col2"), sep_="-"))
#' 
#' #       pre_datf X.o.  X.y.
#' #o-y    1      "o"   "y" 
#' #hj-yy  2      "hj"  "yy"
#' #er-y   3      "er"  "y" 
#' #k-ll   4      "k"   "ll"
#' #ooo-mm 5      "ooo" "mm"
#'
#' @export

wider_datf <- function(inpt_datf, col_to_splt=c(), sep_="-"){

        cut_v <- function(inpt_v, sep_=""){

                rtn_datf <- data.frame(matrix(data=NA, nrow=0, ncol=length(unlist(strsplit(inpt_v[1], split=sep_)))))

                rtn_datf <- t(mapply(function(x) return(rbind(rtn_datf, unlist(strsplit(x, split=sep_)))), inpt_v))

                return(rtn_datf)

        }

        if (typeof(col_to_splt) == "character"){

            for (i in 1:length(col_to_splt)){

               col_to_splt[i] <- match(col_to_splt[i], colnames(inpt_datf))

            }

            col_to_splt <- as.numeric(col_to_splt)

        }

        for (cl in col_to_splt){

            pre_datf <- inpt_datf[,1:(cl-1)]

            cur_datf <- cut_v(inpt_v=inpt_datf[, cl], sep_=sep_) 

            if (cl < ncol(inpt_datf)){

                    w_datf <- cbind(pre_datf, cur_datf, inpt_datf[, ((cl+1):ncol(inpt_datf))])

            }else{

                    w_datf <- cbind(pre_datf, cur_datf)

            }

        }

    return(w_datf)

}

#' colins_datf
#'
#' Allow to insert vectors into a dataframe.
#' 
#' @param inpt_datf is the dataframe where vectors will be inserted
#' @param target_col is a list containing all the vectors to be inserted
#' @param target_pos is a list containing the vectors made of the columns names or numbers where the associated vectors from target_col will be inserted after
#'
#' @examples
#'
#' datf1 <- data.frame("frst_col"=c(1:5), "scd_col"=c(5:1))
#' 
#' print(colins_datf(inpt_datf=datf1, target_col=list(c("oui", "oui", "oui", "non", "non"), 
#'              c("u", "z", "z", "z", "u")), 
#'                 target_pos=list(c("frst_col", "scd_col"), c("scd_col"))))
#' 
#' #  frst_col cur_col scd_col cur_col.1 cur_col
#' #1        1     oui       5       oui       u
#' #2        2     oui       4       oui       z
#' #3        3     oui       3       oui       z
#' #4        4     non       2       non       z
#' #5        5     non       1       non       u
#'
#' print(colins_datf(inpt_datf=datf1, target_col=list(c("oui", "oui", "oui", "non", "non"), 
#'              c("u", "z", "z", "z", "u")), 
#'                 target_pos=list(c(1, 2), c("frst_col"))))
#' 
#' #  frst_col cur_col scd_col cur_col cur_col
#' #1        1     oui       5       u     oui
#' #2        2     oui       4       z     oui
#' #3        3     oui       3       z     oui
#' #4        4     non       2       z     non
#' #5        5     non       1       u     non
#'
#' @export

colins_datf <- function(inpt_datf, target_col=list(), target_pos=list()){

    cl_nms <- colnames(inpt_datf)

    for (id_vec in 1:length(target_pos)){

            vec <- unlist(target_pos[id_vec])

            if (typeof(vec) == "character"){

                    pre_v <- c()

                    for (el in vec){

                        pre_v <- c(pre_v, match(el, cl_nms))

                    }

                    target_pos <- append(x=target_pos, values=list(pre_v), after=id_vec)

                    target_pos <- target_pos[-id_vec]

            }

    }

    for (cl in 1:length(target_col)){

        cur_col <- unlist(target_col[cl])

        cur_pos_v <- unlist(target_pos[cl])

        for (pos in 1:length(cur_pos_v)){

            idx <- cur_pos_v[pos]

            if (idx == 0){

                inpt_datf <- cbind(cur_col, inpt_datf[(idx+1):ncol(inpt_datf)])

            }else if (idx < ncol(inpt_datf)){

                inpt_datf <- cbind(inpt_datf[1:idx], cur_col, inpt_datf[(idx+1):ncol(inpt_datf)])

            }else{

                inpt_datf <- cbind(inpt_datf[1:idx], cur_col)

            }

            if (pos < length(cur_pos_v)){

                cur_pos_v[(pos+1):length(cur_pos_v)] = cur_pos_v[(pos+1):length(cur_pos_v)] + 1 
         
            }

            if (cl < length(target_pos)){

                    for (i in (cl+1):length(target_pos)){

                        target_pos <- append(x=target_pos, values=(unlist(target_pos[i])+1), after=i)

                        target_pos <- target_pos[-i]
                    
                    } 

            }

        }

    }

  return(inpt_datf)

}

#' id_keepr_datf
#'
#' Allow to get the original indexes after multiple equality comparaison according to the original number of row
#'
#' @param inpt_datf is the input dataframe
#' @param col_v is the vector containing the column numbers or names to be compared to their respective elements in "el_v"
#' @param el_v is a vector containing the elements that may be contained in their respective column described in "col_v" 
#' @param rstr_l is a list containing the vector composed of the indexes of the elements chosen for each comparison. If the length of the list is inferior to the lenght of comparisons, so the last vector of rstr_l will be the same as the last one to fill make rstr_l equal in term of length to col_v and el_v
#' @examples
#' 
#' datf1 <- data.frame(c("oui", "oui", "oui", "non", "oui"), 
#'      c("opui", "op", "op", "zez", "zez"), c(5:1), c(1:5))
#' 
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op")))
#'
#' #[1] 2 3
#' 
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op"), 
#'      rstr_l=list(c(1:5), c(3, 2, 2, 2, 3))))
#'
#' #[1] 2 3
#'
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op"), 
#'      rstr_l=list(c(1:5), c(3))))
#'
#' #[1] 3
#'
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op"), rstr_l=list(c(1:5))))
#' 
#' #[1] 2 3
#' 
#' @export

id_keepr <- function(inpt_datf, col_v=c(), el_v=c(), rstr_l=NA){

    rtn_v <- c(1:nrow(inpt_datf))

    if (typeof(col_v) == "character"){

        cl_nms <- colnames(inpt_datf)

        for (i in 1:length(col_v)){

                col_v[i] <- match(col_v[i], cl_nms)

        }

        col_v <- as.numeric(col_v)

    }

    if (all(is.na(rstr_l))){

        for (i in 1:length(col_v)){

            rtn_v <- rtn_v[inpt_datf[rtn_v, col_v[i]] == el_v[i]]  

        }

        return(rtn_v)

    }else if (length(rstr_l) < length(col_v)){

            lst_v <- unlist(rstr_l[length(rstr_l)])

            for (i in (length(rstr_l)+1):length(col_v)){

                rstr_l <- append(x=rstr_l, values=list(lst_v))

            }

    }

    pre_vec <- c()

    fun <- function() { return(c(pre_vec, FALSE)) }

    for (i in 1:length(col_v)){

        pre_vec2 <- mapply(function(x) return(fun()), c(1:length(rtn_v)))

        interst <- intersect(unlist(rstr_l[i]), rtn_v)

        pre_vec2[interst] <- inpt_datf[interst, col_v[i]] == el_v[i]

        rtn_v <- rtn_v[pre_vec2]  

    }

    return(rtn_v)

}

#' unique_datf
#' 
#' Returns the input dataframe with the unique columns or rows.
#'
#' @param inpt_datf is the input dataframe
#' @param col is a parameter that specifies if the dataframe returned should have unique columns or rows, defaults to F, so the dataframe returned by default has unique rows
#' @examples
#'
#' datf1 <- data.frame(c(1, 2, 1, 3), c("a", "z", "a", "p"))
#' 
#' print(unique_datf(inpt_datf=datf1))
#' 
#' #   c.1..2..1..3. c..a....z....a....p..
#' #1             1                     a
#' #2             2                     z
#' #4             3                     p
#' 
#' datf1 <- data.frame(c(1, 2, 1, 3), c("a", "z", "a", "p"), c(1, 2, 1, 3))
#' 
#' print(unique_datf(inpt_datf=datf1, col=TRUE))
#' 
#' #  cur_v cur_v
#' #1     1     a
#' #2     2     z
#' #3     1     a
#' #4     3     p
#' 
#' @export

unique_datf <- function(inpt_datf, col=FALSE){

        comp_l <- list()

        if (col){

                rtn_datf <- data.frame(matrix(data=NA, nrow=nrow(inpt_datf), ncol=0))

                for (col in 1:ncol(inpt_datf)){

                        cur_v <- inpt_datf[, col]

                        if ((list(cur_v) %in% comp_l) == FALSE){ rtn_datf <- cbind(rtn_datf, cur_v) }

                        comp_l <- append(x=comp_l, values=list(cur_v))

                }

        }else{

                rtn_datf <- data.frame(matrix(data=NA, nrow=0, ncol=ncol(inpt_datf)))

                for (row in 1:nrow(inpt_datf)){

                        cur_v <- inpt_datf[row, ]

                        if ((list(cur_v) %in% comp_l) == FALSE){ rtn_datf <- rbind(rtn_datf, cur_v) }

                        comp_l <- append(x=comp_l, values=list(cur_v))

                }

        }

    return(rtn_datf)

}

#' vec_in_datf
#'
#' Allow to get if a vector is in a dataframe. Returns the row and column of the vector in the dataframe if the vector is contained in the dataframe.
#'
#' @param inpt_datf is the input dataframe
#' @param inpt_vec is the vector that may be in the input dataframe
#' @param coeff is the "slope coefficient" of inpt_vec
#' @param conventional is if a positive slope coefficient means that the vector goes upward or downward 
#' @param stop_untl is the maximum number of the input vector the function returns, if in the dataframe 
#' @examples
#'
#' datf1 <- data.frame(c(1:5), c(5:1), c("a", "z", "z", "z", "a"))
#' 
#' print(datf1)
#' 
#' #  c.1.5. c.5.1. c..a....z....z....z....a..
#' #1      1      5                          a
#' #2      2      4                          z
#' #3      3      3                          z
#' #4      4      2                          z
#' #5      5      1                          a
#'
#' print(vec_in_datf(inpt_datf=datf1, inpt_vec=c(5, 4, "z"), coeff=1))
#'
#' #NULL
#' 
#' print(vec_in_datf(inpt_datf=datf1, inpt_vec=c(5, 2, "z"), coeff=1))
#' 
#' #[1] 5 1
#'
#' print(vec_in_datf(inpt_datf=datf1, inpt_vec=c(3, "z"), coeff=1))
#'
#' #[1] 3 2
#'
#' print(vec_in_datf(inpt_datf=datf1, inpt_vec=c(4, "z"), coeff=-1))
#' 
#' #[1] 2 2
#'
#' print(vec_in_datf(inpt_datf=datf1, inpt_vec=c(2, 3, "z"), coeff=-1))
#' 
#' #[1] 2 1
#' 
#' print(vec_in_datf(inpt_datf=datf1, inpt_vec=c(5, 2, "z"), coeff=-1, conventional=TRUE))
#'  
#' #[1] 5 1
#'
#' datf1[4, 2] <- 1
#' 
#' print(vec_in_datf(inpt_datf=datf1, inpt_vec=c(1, "z"), coeff=-1, conventional=TRUE, stop_untl=4))
#' 
#' #[1] 4 2 5 2
#' 
#' @export

vec_in_datf <- function(inpt_datf, inpt_vec=c(), coeff=0, stop_untl=1, conventional=FALSE){

    if (conventional){ coeff <- coeff * -1 }

    rtn_v <- c()

    encounter_cnt = 0

    if (coeff > -1){

            for (I in 1:(ncol(inpt_datf) - length(inpt_vec) + 1)){

                    strt_id = 1 + (length(inpt_vec) * coeff)

                    for (i in strt_id:nrow(inpt_datf)){

                        if (inpt_datf[i, I] == inpt_vec[1]){

                                cur_row = i

                                cur_col = I 

                                col_cnt = 1

                                while (col_cnt < (length(inpt_vec) + 1) & inpt_datf[cur_row, cur_col] == inpt_vec[col_cnt]){

                                    cur_row = cur_row - coeff

                                    if (!(col_cnt) == length(inpt_vec)){

                                        cur_col = cur_col + 1

                                    }

                                    col_cnt = col_cnt + 1

                                }

                                if (cur_col == ncol(inpt_datf)){

                                        rtn_v <- c(rtn_v, i, I)

                                        encounter_cnt = encounter_cnt + 1

                                        if (encounter_cnt == stop_untl){

                                                return(rtn_v)

                                        }

                                }

                        }

                    }

            }

    }else{

            for (I in 1:(ncol(inpt_datf) - length(inpt_vec) + 1)){

                    strt_id = nrow(inpt_datf) - (length(inpt_vec) * abs(coeff))

                    for (i in 1:strt_id){

                        if (inpt_datf[i, I] == inpt_vec[1]){

                                cur_row = i 

                                cur_col = I

                                col_cnt = 1

                                while (col_cnt < (length(inpt_vec) + 1) & inpt_datf[cur_row, cur_col] == inpt_vec[col_cnt]){

                                    cur_row = cur_row + abs(coeff)

                                    if (!(col_cnt) == length(inpt_vec)){

                                        cur_col = cur_col + 1

                                    }

                                    col_cnt = col_cnt + 1


                                }

                                if (cur_col == ncol(inpt_datf)){

                                        rtn_v <- c(rtn_v, i, I)

                                        encounter_cnt = encounter_cnt + 1

                                        if (encounter_cnt == stop_untl){

                                                return(rtn_v)

                                        }

                                }

                        }

                    }

            }

    }

    return(rtn_v)

}

#' diff_datf
#'
#' Returns a vector with the coordinates of the cell that are not equal between 2 dataframes (row, column).
#'
#' @param datf1 is an an input dataframe
#' @param datf2 is an an input dataframe
#' @examples
#'
#' datf1 <- data.frame(c(1:6), c("oui", "oui", "oui", "oui", "oui", "oui"), c(6:1))
#' 
#' datf2 <- data.frame(c(1:7), c("oui", "oui", "oui", "oui", "non", "oui", "zz"))
#' 
#' print(diff_datf(datf1=datf1, datf2=datf2)) 
#'
#' #[1] 5 1 5 2
#'
#' @export

diff_datf <- function(datf1, datf2){

    rtn_v <- c()

    min_r <- min(c(nrow(datf1), nrow(datf2)))

    for (col_i in 1:min(c(ncol(datf1), ncol(datf2)))){

            for (row_i in 1:min_r){

                if (datf1[row_i, col_i] != datf2[row_i, col_i]){ rtn_v <- c(rtn_v, row_i, col_i) } 

            }

    }

    return(rtn_v)

}

#' swipr
#'
#' Returns an ordered dataframes according to the elements order given. The input datafram has two columns, one with the ids which can be bonded to multiple elements in the other column.
#'
#' @param inpt_datf is the input dataframe
#' @param how_to is a vector containing the elements in the order wanted
#' @param id_w is the column number or the column name of the elements
#' @param id_ids is the column number or the column name of the ids
#' @examples
#'
#' datf <- data.frame("col1"=c("Af", "Al", "Al", "Al", "Arg", "Arg", "Arg", "Arm", "Arm", "Al"),
#' 
#'         "col2"=c("B", "B", "G", "S", "B", "S", "G", "B", "G", "B"))
#' 
#' print(swipr(inpt_datf=datf, how_to=c("G", "S", "B")))
#' 
#'    col1 col2
#' 1    Af    B
#' 2    Al    G
#' 3    Al    S
#' 4    Al    B
#' 5   Arg    G
#' 6   Arg    S
#' 7   Arg    B
#' 8   Arm    G
#' 9   Arm    B
#' 10   Al    B
#'
#' @export

swipr <- function(inpt_datf, how_to=c(), id_w=2, id_ids=1){

       if (typeof(id_w) == "character"){

               id_w <- match(id_w, colnames(inpt_datf))

       }

       if (typeof(id_ids) == "character"){

               id_ids <- match(id_ids, colnames(inpt_datf))

       }

       for (el in unique(inpt_datf[, id_ids])){

            cur_rows <- inpt_datf[, id_ids] == el

            cur_v <- inpt_datf[cur_rows, id_w]

            inpt_datf[cur_rows, id_w] <- how_to[sort(match(x=cur_v, table=how_to), 
                                                     decreasing=FALSE)]

       }

  return(inpt_datf)

}

#' intersect_mod
#'
#' Returns the mods that have elements in common
#'
#' @param datf is the input dataframe
#' @param inter_col is the column name or the column number of the values that may be commun betwee the different mods
#' @param mod_col is the column name or the column number of the mods in the dataframe
#' @param ordered_descendly, in case that the elements in commun are numeric, this option can be enabled by giving a value of TRUE or FALSE see examples
#' @param n_min is the minimum elements in common a mod should have to be taken in count
#'
#' @examples
#'
#' datf <- data.frame("col1"=c("oui", "oui", "oui", "oui", "oui", "oui", 
#'                      "non", "non", "non", "non", "ee", "ee", "ee"), "col2"=c(1:6, 2:5, 1:3))
#' 
#' print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=2))
#' 
#'    col1 col2
#' 2   oui    2
#' 3   oui    3
#' 7   non    2
#' 8   non    3
#' 12   ee    2
#' 13   ee    3
#'
#' print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=3))
#'
#'    col1 col2
#' 2   oui    2
#' 3   oui    3
#' 4   oui    4
#' 5   oui    5
#' 7   non    2
#' 8   non    3
#' 9   non    4
#' 10  non    5
#'
#' print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=5))
#' 
#'   col1 col2
#' 1  oui    1
#' 2  oui    2
#' 3  oui    3
#' 4  oui    4
#' 5  oui    5
#' 6  oui    6
#' 
#' datf <- data.frame("col1"=c("non", "non", "oui", "oui", "oui", "oui", 
#'                       "non", "non", "non", "non", "ee", "ee", "ee"), "col2"=c(1:6, 2:5, 1:3))
#' 
#' print(intersect_mod(datf=datf, inter_col=2, mod_col=1, n_min=3))
#' 
#'    col1 col2
#' 8   non    3
#' 9   non    4
#' 10  non    5
#' 3   oui    3
#' 4   oui    4
#' 5   oui    5
#' 
#' @export

intersect_mod <- function(datf, inter_col, mod_col, n_min, descendly_ordered=NA){

    if (typeof(inter_col) == "character"){

            inter_col <- match(inter_col, colnames(datf))

    }

    if (typeof(mod_col) == "character"){

            mod_col <- match(mod_col, colnames(datf))

    }

    mods <- unique(datf[, mod_col])  

    final_intersect <- as.numeric(datf[datf[, mod_col] == mods[1], inter_col])

    mods2 <- c(mods[1])

    if (length(mods) > 1){

            for (i in 2:length(mods)){

                    cur_val <- as.numeric(datf[datf[, mod_col] == mods[i], inter_col])

                    if (length(intersect(final_intersect, cur_val)) >= n_min){

                            final_intersect <- intersect(final_intersect, cur_val)

                            mods2 <- c(mods2, mods[i])

                    }

            }

    }

    cur_datf <- datf[datf[, mod_col] == mods2[1], ]

    if (!is.na(descendly_ordered)){

            final_intersect <- sort(x=final_intersect, decreasing=FALSE)

            rtn_datf <- cur_datf[sort(match(final_intersect, cur_datf[, inter_col]), decreasing=descendly_ordered), ]

            if (length(mods2) > 1){

                    for (i in 2:length(mods2)){

                        cur_datf <- datf[datf[, mod_col] == mods2[i], ]

                        rtn_datf <- rbind(rtn_datf, cur_datf[sort(match(final_intersect, cur_datf[, inter_col]), decreasing=descendly_ordered), ])
    

                    }

            }

    }else{

            rtn_datf <- cur_datf[match(final_intersect, cur_datf[, inter_col]), ]

            if (length(mods2) > 1){

                    for (i in 2:length(mods2)){

                        cur_datf <- datf[datf[, mod_col] == mods2[i], ]

                        rtn_datf <- rbind(rtn_datf, cur_datf[match(final_intersect, cur_datf[, inter_col]), ])
    

                    }

            }

    }

    return(rtn_datf)

}

