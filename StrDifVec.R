StrDifVec <-
function(grp1_string, grp2_string, num_perm = 1000, o.x = 0.01, o.y = 0, p.x = 0.051, p.y = 0){

   # number of strings in each group
  
   num_subj1 <- length(grp1_string)
   num_subj2 <- length(grp2_string)
   num_subj_all <- num_subj1 + num_subj2

   # combination of strings from 2 groups

   grps_string <- c(grp1_string, grp2_string)

   ### function to find longer string

   longerStr <- function(s1, s2){
      l1<- nchar(s1)
      l2 <- nchar(s2)
      if (l1 > l2){
         return (l1)
      } else {
         return (l2)
      }
   }

   #### within
   ### function of within: ld

   ld.within <- function(s){
      # number of strings
      len <- length(s)

      # list to hold ld 

      ld.within.list <- vector(mode="list", length=len)
      longLen.list <- vector(mode="list", length=len)

      ld.within.list <- sapply(1:(len-1), function(i){
                           utils::adist(s[i],s[(i+1):len])
                        })

      ld.within.vec <- unlist(ld.within.list)

      return(ld.within.vec)
   }

   ### function of within: length of longer string between 2

   longLen.within <- function(s){
      # number of strings
      len <- length(s)

      # list to hold the length of longer string between 2

      longLen.within.list <- vector(mode = "list", length = len)

      longLen.within.list <- sapply(1:(len-1), function(i){
                         mapply(longerStr, s1 = s[i], s2 = s[(i+1):len])
                  })

      longLen.within.vec <- unlist(longLen.within.list)
      return(longLen.within.vec)
   }

   ### function of mean normalized ld - within

   ave_ld.within_norm <- function(s1, s2){

      ## apply within functions to two groups of strings

      ld.within.vec1 <- ld.within(s1)
      longLen.within.vec1 <- longLen.within(s1)

      ld.within.vec2 <- ld.within(s2)
      longLen.within.vec2 <- longLen.within(s2)

      ## combine two groups

      ld.within.vec.com <- c(ld.within.vec1, ld.within.vec2)
      longLen.within.vec.com <- c(longLen.within.vec1, longLen.within.vec2)

      ## get mean of normalized ld

      ld.within.vec_norm.vec <- ld.within.vec.com/longLen.within.vec.com

      ld.within_norm <- mean(ld.within.vec_norm.vec)

       return(ld.within_norm)
   }
      
   ## original normalized ld: within

   ld.within_norm.ori <- ave_ld.within_norm(grp1_string, grp2_string)
   

   #### between
   ### function of mean normalized ld - between

   ave_ld.between_norm <- function(s1, s2){

      # number of strings
      len1 <- length(s1)
      len2 <- length(s2)

      # matrix to hold ld and the length of longer string between 2

      ld.between.m <- matrix(ncol = len2, nrow = len1)
      longLen.m <- matrix(ncol = len2, nrow = len1)

      ld.between.m <- utils::adist(s1, s2)

      longLen.m <-sapply(1:len2, function(i) {
            mapply(longerStr, s1 = s2[i], s2 = s1)
      })

      ld.between_norm.m <- ld.between.m / longLen.m

      ld.between_norm <- mean(ld.between_norm.m)

      return(ld.between_norm)
   }

   ## original normalized ld: between

   ld.between_norm.ori <- ave_ld.between_norm(grp1_string, grp2_string)


   #### difference of ld
   ### function of difference of ld between mean normalized ld (between) and mean normalized ld (within)

   dif_ld_norm <- function(s1, s2){

      ld_b_norm <- ave_ld.between_norm(s1, s2)
      ld_w_norm <- ave_ld.within_norm(s1, s2)

      dif_ld_norm <- ld_b_norm - ld_w_norm

      return(dif_ld_norm)
   }

   ## original difference of normalized ld

   dif_ld_norm.ori <- dif_ld_norm(grp1_string, grp2_string)


   #### Permutation

   ### number of permutation
   # It should be the same if using num_subj2
   actual_perm <- choose(num_subj_all, num_subj1)

   if(actual_perm < num_perm){
       num_perm <- actual_perm
   }

   ### use a matrix to hold all permutation strings, each column is a set of all strings

   # strings_perm.m <- matrix(NULL, nrow = num_subj_all, ncol = num_perm) 
   strings_perm.m <- matrix(nrow = num_subj_all, ncol = num_perm) 

   # permute all strings num_perm times

   strings_perm.m <- replicate(num_perm, sample(grps_string, num_subj_all, replace = FALSE))
 
   # Force the first permutation (1st column in the matrix) to be the original:

   strings_perm.m[,1] <- grps_string  

   # divide the matrix to groups 1 and 2

   grp1_strings_perm.m <- strings_perm.m[1:num_subj1,]
   grp2_strings_perm.m <- strings_perm.m[(num_subj1+1):num_subj_all,]

   # output 1000 mean dif ld, apply function dif_ld_norm:

     dif_ld <- vector(length = num_perm)

     dif_ld <- sapply(1:num_perm, function(k){
                            dif_ld_norm(grp1_strings_perm.m[,k], grp2_strings_perm.m[,k])
     })


   pvalue <- round(mean(dif_ld >= dif_ld_norm.ori), 5)

   p_out <- format(pvalue, nsmall = 5)

   ld.between_norm.ori <- round(ld.between_norm.ori, 5)

   ld.within_norm.ori <- round(ld.within_norm.ori, 5)

   dif_ld_norm.ori <- round(dif_ld_norm.ori, 5)

   
   ### return the list containing vector of ld differences and other values
   strDif_result.list <- list(dif_ld, ld.between_norm.ori, ld.within_norm.ori, dif_ld_norm.ori, p_out)

   return(strDif_result.list)

}
