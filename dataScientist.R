dataScientist = function(names=c("D. Scientist"),skills=matrix(rep(1/3,3),nrow=1), addSS=TRUE, just=NULL){
#########################################################################################
#   A function to calculate whether you are a data scientist.
#   R function Copyright (C) 2011 Simply Statistics and Jeffrey T. Leek
#   (http://simplystatistics.tumblr.com, http://twitter.com/leekgroup)
#   The Venn Diagram is due to David Champagne of Revolution Analytics
#   and was first published in a post here:
#   http://tdwi.org/articles/2011/01/05/Rise-of-Data-Science.aspx
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details, see <http://www.gnu.org/licenses/>.
#    
#   Inputs: 
#   names = a character vector of names to be plotted
#   skills = A matrix where each row corresponds to an individual whose name appears in
#            names. Column 1 is the number (percent) of papers that are hacking. Column 2
#            is the number (percent) of papers that are substantive/scientific. Column 3
#            is the number (percent) of papers that are mathematical/statistical. See
#            http://simplystatistics.tumblr.com/ for more details. 
#   addSS = Add the Simply Statistics bloggers to your plot
#   just = A character vector telling which side of the dot the name should be printed
#          just = "left" for the left and just="right" for the right. If NULL, all names
#          will print on the right. 
#  
########################################################################################


  require(png)
  require(klaR)
  if(!is.null(just)){
    if(length(just)!=dim(skills)[1]){
      stop("There must be exactly one value of just for every row in the skills matrix ")
    }
    if(!all(just %in% c("left","right"))){
      stop("just can only take the values of left or right")
    }
    just2 = rep(NA,length(just))
    just2[just=="left"] = -1
    just2[just=="right"] = 1
    just = just2
  }
  
  if(length(names)!=dim(skills)[1]){
    stop("There must be exactly one name for every row in the skills matrix")
  }

  if("datascience.png" %in% list.files()){
    tmp = readPNG("datascience.png")
  }else{
    tmp = try(download.file("http://www.biostat.jhsph.edu/~jleek/datascience.png","datascience.png"),silent=T)
    if(tmp != 0){
      stop("You must either download the file http://www.biostat.jhsph.edu/~jleek/datascience.png and include it in the directory where you are running dataScientist, or be connected to the internet so the file can be downloaded for you.")
    }else{
      cat("You will need to run the function again to see the Venn Diagram \n since the file needed to be downloaded. \n")
    } 
  }
  plot(0:1, xaxt="n",yaxt="n", type='n', main="Are you a data scientist?", xlab="",ylab="",xlim=c(-2,2),ylim=c(-2,2))
  lim <- par()
  rasterImage(tmp,lim$usr[1]+0.01,lim$usr[3],lim$usr[2],lim$usr[4])

   tripoints2 = function (x, y = NULL, z = NULL,names=NULL,just=NULL,...){
    if(is.null(just)){just=rep(1,length(x))}
    result <- 3*tritrafo(x, y, z)
    points(result[, 1], -result[, 2], ...)
    rect((result[,1]+ just*0.05),(-result[,2]-1.2/2*strheight(names)),(result[,1]+just*0.05+just*1.2*strwidth(names)),(-result[,2]+1.2/2*strheight(names)),border=NA,col="white")
    xval = result[,1]
    xval[(just < 0)] = xval[(just < 0)] - 0.05 - 1.2*strwidth(names)[(just < 0)]
    text(xval,-result[,2],labels=names,adj=c(0,0.5),font=2,...)
    invisible(result)
  }
  
  
  if(addSS){
    SSnames = c(" R. Irizarry"," R. Peng"," J. Leek")
    SSskills = matrix(c(31,43,29),nrow=1)
    SSskills = rbind(SSskills,c(9,28,11))
    SSskills = rbind(SSskills,c(4,7,6))
    SSskills = SSskills/rowSums(SSskills)
    tripoints2(SSskills,names=SSnames,just=c(-1,1,1),pch=23,col="black",cex=1.2,bg="blue",lwd=2)
  }
 
  skills = skills/rowSums(skills)
  names = paste(" ",names,sep="")
  
 
  tripoints2(skills,names=names,just=just,pch=23,cex=1.2,lwd=2,bg="red",col="black")

}
