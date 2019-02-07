# Multi-Scenario W-CLUS-MCDA with Parallel Decision Making (PDM)
##### Corresponding Author: Abteen Ijadi Maghsoodi
##### Algorithm Designer/Developer: Abteen Ijadi Maghsoodi
#aimaghsoodi@srbiau.ac.ir - aimaghsoodi@outlook.com
##### Software Developer: Dara Riahi
#darariahi@ut.ac.ir - riahi.dara@gmail.com

#Copyright (c) 2019 Abteen Ijadi Maghsoodi, Dara Riahi.

#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in all
#copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.


#' W-CLUS-MCDA: Weighted Cluster analysis for improving Multiple Criteria Decision Analysis with Parallel Decision Making (PDM) and Multi-Scenario structure
#'
#' @param strt This is the only command that need to be written to start the CLUS-MCDA algorithm.
#'
#' @return returns a data frame which contains the clustering outputs of classified/categorized data as well as the scores and the four rankings calculated (Ratio System, Reference Point, Multiplicative Form and MUTIMOORA ranking [Dominance Theory]).
#' @export
#' @references [1] Ijadi Maghsoodi, A., Kavian, A., Khalilzadeh, M., & Brauers, W. K. M. (2018). CLUS-MCDA: A Novel Framework based on Cluster Analysis and Multiple Criteria Decision Theory in a Supplier Selection Problem. Computers & Industrial Engineering, 118 (August 2017), 409–422. Elsevier. Retrieved from http://linkinghub.elsevier.com/retrieve/pii/S0360835218300962. [2] Ijadi Maghsoodi, A., Riahi, D., E. Herrera-Viedma, E. K. Zavadskas, An Integrated Parallel Big Data Decision Support Tool Using the W-CLUS-MCDA: A Multi-Scenario Personnel Assessment. [Under-Review]. [3] Ijadi Maghsoodi, A., Riahi, D. The CLUS-MCDA Package: An Original Software Article. [Under-Review]
#' @examples
#' #just type CLUSMCDA() to run the CLUS-MCDA runnable program/package.
CLUSMCDA=function(strt)
{
  options(scipen = 1000)
  options(max.print = 100000)
  #CONSOLE INPUT MUST BE CSV FORMAT
  flcnt=readline(prompt =  "How many CSV files (as input data) do you want to load/calculate? ")
  lt=letters[3:(3+as.numeric(flcnt)-1)]
  LT=LETTERS[3:(3+as.numeric(flcnt)-1)]
  totRCclus=c()
  totrowclus=c()
  trowclus=c()
  totCT=matrix(0,ncol = 10)
  for (q in 1:flcnt)
  {
    cat("Information of CSV file",q,"\n")
    print("Use capital letters to load columns/criteria/attributes")
    columnumstart=readline(prompt = "Column/Criteria start point: ")
    columnumend=readline(prompt = "Column/Criteria end point: ")
    clustercriterion=readline(prompt = "In Which column is your clustering attribute located? ")
    codecriterion=readline(prompt = "In Which column is your main alternatives are located? ")
    labelcriterion=readline(prompt = "In Which column is your categorizing/classification attribute is located? ")
    #LETTER TO NoI CONVERTION
    letter2num <- function(x)
    {
      utf8ToInt(x) - utf8ToInt("A") + 1L
    }
    #ORIGINAL DATA INPUT
    columnumstart=LETTERS[letter2num(columnumstart)+1]
    columnumend=LETTERS[letter2num(columnumend)+1]
    cat("Input CSV file Information",q,"\n")
    strt=read.csv(file.choose(),header = TRUE,skipNul = TRUE)
    a=strt
    a=a[order(a[,letter2num(codecriterion)]),]
    NoI=1:dim(a)[1]
    a=cbind(NoI,a)
    a=as.data.frame(a)
    attach(a,warn.conflicts = FALSE)
    rownum=c()
    for (i in 1:dim(a)[1])
    {
      if(is.na(a[i,2])==FALSE)
      {
        rownum[i]=i
      }
    }
    b=a[min(rownum):max(rownum),letter2num(columnumstart):letter2num(columnumend)]
    #print(b)
    #NORMALIZED ORIGINAL DATA MATRIX
    n0=b
    n0p2=n0^2
    n1=matrix(1,length(rownum),letter2num(columnumend) - letter2num(columnumstart) + 1)
    n2=matrix(1,length(rownum),letter2num(columnumend) - letter2num(columnumstart) + 1)
    for (i in 1:length(rownum))
    {
      for (j in 1:(letter2num(columnumend)-letter2num(columnumstart)+1))
      {
        n1[i,j]=matrix(n0p2[i,j]/sqrt(sum(n0p2[,j])),byrow = TRUE)
        n2[i,j]=matrix(n1[i,j]/sqrt(n0p2[i,j]),byrow = TRUE)
      }
    }
    #print(n2)
    cat("There are",letter2num(columnumend)-letter2num(columnumstart)+1, "criteria identified\n")
    #BENEFIT AND COST COLUMNS INPUT
    orgcost=c()
    newcost=c()
    cost=c()
    orgbenefit=c()
    newbenefit=c()
    benefit=c()
    costnum=readline(prompt = "how many cost/non-beneficial columns/crietria do you have? ")
    for(i in 1:costnum)
    {
      orgcost[i]=readline(prompt = "Use capital letters to load cost column(s)/crietria: ")
      newcost=letter2num(orgcost[i]) + 1
      cost=c(cost,newcost)
    }
    benefitnum=readline(prompt = "how many benefit/beneficial columns/crietria do you have? ")
    for(i in 1:benefitnum)
    {
      orgbenefit[i]=readline(prompt = "Use capital letters to load benefit column(s)/crietria: ")
      newbenefit=letter2num(orgbenefit[i]) + 1
      benefit=c(benefit,newbenefit)
    }
    orgcostbenefit=c(orgcost,orgbenefit)
    costbenefit=c(cost,benefit)
    if(min(costbenefit)!=1)
    {
      costbenefit=rank(costbenefit)
    }
    cost=costbenefit[1:length(cost)]
    benefit=costbenefit[(length(cost)+1):((length(cost))+length(benefit))]
    o=which(sort(orgcostbenefit)==clustercriterion)
    clustercriterion=LETTERS[letter2num(clustercriterion)+1]
    codecriterion=LETTERS[letter2num(codecriterion)+1]
    labelcriterion=LETTERS[letter2num(labelcriterion)+1]
    #WEIGHT MATRIX INPUT
    w=c()
    print("Enter weight for each criterion from left to right according to your desired or input original data csv file(s)")
    for (i in 1:(letter2num(columnumend)-letter2num(columnumstart)+1))
    {
      cat("Criterion NUmber",i)
      neww=readline()
      w[i]=as.numeric(neww)
    }
    #}
    print(w)
    print(sum(w))
    if(sum(w)==1)
    {
      print("The summation of weights are 1; therefore the expert bias is not available and you may proceed")
    }
    else
    {
      cat("The summation of weights are not 1; which means that an expert bias error is available but you may proceed \nIgnore this message if you wish to proceed or press ESC button in order to quit the program/runnable package\n")
      warning("EXPERT BIAS ERROR")
    }
    MCDb=b[,-as.integer(o)]
    n2M=n2[,-as.integer(o)]
    wMCD=w[-as.integer(o)]
    #CLUSTERING (MCDM WITHIN)
    for (k in 1:length(levels(a[,letter2num(labelcriterion)])))
    {
      print("#############################################################################")
      cat("Calculations for classification/category/section",k,"\n")
      if(length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]])<=8)
      {
        RCclus=a[,letter2num(codecriterion)][a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]]
        plot(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]],a[,letter2num(clustercriterion)][a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]],col="red",pch=16,xlim =c(0,max(NoI)+1),ylim = c(0,max(a[,letter2num(clustercriterion)])+1),xlab = "Number of Potential Candidates",ylab = "Total Work Experience (Years)",cex.lab=1.5,cex.axis=1.5,cex=2.5)
        legend("topright",levels(a[,letter2num(labelcriterion)])[k],col="red")
        #RATIO SYSTEM
        inclus=k
        xw=matrix(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]),dim(MCDb)[2])
        xwcst=rep(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        xwbnft=rep(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        newxw=c()
        XW=rep(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        for(i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        {
          for (j in 1:dim(MCDb)[2])
          {
            xw[i,j]=n2M[NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][i]+1,j]*wMCD[j]
            newxw=xw[i,j]

            if(sum(j==cost)>0)
            {
              xwcst[i]=xwcst[i]+newxw
            }
            if(sum(j==benefit)>0)
            {
              xwbnft[i]=xwbnft[i]+newxw
            }
          }
          XW[i]=xwbnft[i]-xwcst[i]
        }
        t1=cbind(XW,rank(-XW,ties.method = "first"))
        #REFERENCE POINT
        r=c()
        d=matrix(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]),dim(MCDb)[2])
        dw=matrix(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]),dim(MCDb)[2])
        for (i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        {
          for (j in 1:dim(MCDb)[2])
          {
            if(sum(j==cost)>0)
            {
              r[j]=min(n2M[,j],na.rm = TRUE)
            }
            if(sum(j==benefit)>0)
            {
              r[j]=max(n2M[,j],na.rm = TRUE)
            }
            d[i,j]=abs(r[j]-n2M[NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][i]+1,j])
            dw[i,j]=d[i,j]*w[j]
          }
        }
        DW=c()
        for (i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        {
          DW[i]=max(dw[i,])
        }
        t2=cbind(DW,rank(DW,ties.method = "first"))
        #MULTIPICATIVE FORM
        xpw=matrix(1,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]),letter2num(columnumend) - letter2num(columnumstart) + 1)
        newxpw=c()
        xpwcst=rep(1,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        xpwbnft=rep(1,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        XPW=rep(1,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        for(i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        {
          for (j in 1:dim(MCDb)[2])
          {
            xpw[i,j]=n2M[NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][i]+1,j]^w[j]
            newxpw=xpw[i,j]
            if(sum(j==cost)>0)
            {
              xpwcst[i]=xpwcst[i]*newxpw
            }
            if(sum(j==benefit)>0)
            {
              xpwbnft[i]=xpwbnft[i]*newxpw
            }
          }
          XPW[i]=xpwbnft[i]/xpwcst[i]
        }
        t3=cbind(XPW,rank(-XPW,ties.method = "first"))
        #DOMINANCE THEORY
        Tt=cbind(Yi=XW,Zi=DW,Ui=XPW,rnkYi=rank(-XW,ties.method = "first"),rnkZi=rank(DW,ties.method = "first"),rnkUi=rank(-XPW,ties.method = "first"))
        DR=c()
        for (i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]))
        {
          DR[i] = (Tt[i,4]+Tt[i,5]+Tt[i,6])/3
        }
        t4=cbind(DR,rank(DR,ties.method = "first"))
        RCclus=a[,letter2num(codecriterion)][a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]]
        CT=cbind(Rcrtmntlvl=k,RcrmntCd=as.character(a[,letter2num(codecriterion)][RCclus]),RcrtmntDprtmmt=as.character(levels(a[,letter2num(labelcriterion)])[k]),Yi=XW,Zi=DW,Ui=XPW,rnkYi=rank(-XW,ties.method = "first"),rnkZi=rank(DW,ties.method = "first"),rnkUi=rank(-XPW,ties.method = "first"),DR,rnkDR=rank(DR,ties.method = "first"))
        CT=CT[,-10]
        print(CT)
        rankc=c()
        rankc1=which(CT[,10]==1)
        rankc2=which(CT[,10]==2)
        rankc3=which(CT[,10]==3)
        rankc4=which(CT[,10]==4)
        rankc=c(rankc1,rankc2,rankc3,rankc4)
        if(k==1)
        {
          totCT=CT[rankc,]
        }
        if(k!=1)
        {
          totCT=rbind(totCT,CT[rankc,])
        }
        finRCclus=RCclus[rankc]
        cat("Your final results for clasification/category/department/section",k,"are as follows:\n")
        cat(as.character(finRCclus),"\n")
        print(NoI[finRCclus])
        newfinRCclus=as.character(as.factor(finRCclus))[1:4]
        totRCclus=c(totRCclus,newfinRCclus)
      }
      if(length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]])>8)
      {
        #CLUSTERING
        itrnum=0
        ogrowclus=c()
        ogrowclus=NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]]
        rowclus=ogrowclus
        inrowclus=rank(rowclus)
        while(length(inrowclus)>8)
        {
          set.seed(4)
          oldwarn=getOption("warn")
          options(warn = -1)
          k1=kmeans(a[c(rowclus),letter2num(clustercriterion)],centers = 4, iter.max = 100,nstart = 100)
          options(warn = oldwarn)
          itrnum=itrnum+1
          c1=c2=c3=c4=c()
          c1=which(k1$cluster==1)
          c2=which(k1$cluster==2)
          c3=which(k1$cluster==3)
          c4=which(k1$cluster==4)
          RCclus=a[,letter2num(codecriterion)][a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]][inrowclus]
          RCclus1=RCclus[c1]
          RCclus2=RCclus[c2]
          RCclus3=RCclus[c3]
          RCclus4=RCclus[c4]
          #MATRIX
          m=matrix(0,length(k1$centers),dim(MCDb)[2])
          newm=c()
          for (i in 1:length(k1$centers))
          {
            for (j in 1:dim(MCDb)[2])
            {
              newm=sum(MCDb[k1$cluster,j][k1$cluster==i])
              m[i,j]=m[i,j]+newm
            }
          }
          n0MCD=m
          n0p2MCD=n0MCD^2
          n1MCD=matrix(0,length(k1$centers),dim(n0MCD)[2])
          n2MCD=matrix(0,length(k1$centers),dim(n0MCD)[2])
          for (i in 1:dim(n0MCD)[1])
          {
            for (j in 1:dim(n0MCD)[2])
            {
              n1MCD[i,j]=matrix(n0p2MCD[i,j]/sqrt(sum(n0p2MCD[,j])),byrow = TRUE)
              n2MCD[i,j]=matrix(n1MCD[i,j]/sqrt(n0p2MCD[i,j]),byrow = TRUE)
            }
          }
          #RATIO SYSTEM
          xwMCD=matrix(0,length(k1$centers),dim(n2MCD)[2])
          newxwMCD=c()
          xwcstMCD=rep(0,length(k1$centers))
          xwbnftMCD=rep(0,length(k1$centers))
          XWMCD=rep(0,length(k1$centers))
          for(i in 1:length(k1$centers))
          {
            for (j in 1:dim(xwMCD)[2])
            {
              xwMCD[i,j]=n2MCD[i,j]*wMCD[j]
              newxwMCD=xwMCD[i,j]

              if(sum(j==cost)>0)
              {
                xwcstMCD[i]=xwcstMCD[i]+newxwMCD
              }
              if(sum(j==benefit)>0)
              {
                xwbnftMCD[i]=xwbnftMCD[i]+newxwMCD
              }
            }
            XWMCD[i]=xwbnftMCD[i]-xwcstMCD[i]
          }
          t1MCD=cbind(XWMCD,rank(-XWMCD,ties.method = "first"))
          #REFERENCE POINT
          rMCD=c()
          dMCD=matrix(0,length(k1$centers),dim(n2MCD)[2])
          dwMCD=matrix(0,length(k1$centers),dim(n2MCD)[2])
          for (i in 1:length(k1$centers))
          {
            for (j in 1:dim(dwMCD)[2])
            {
              if(sum(j==cost)>0)
              {
                rMCD[j]=min(n2MCD[,j],na.rm = TRUE)
              }
              if(sum(j==benefit)>0)
              {
                rMCD[j]=max(n2MCD[,j],na.rm = TRUE)
              }
              dMCD[i,j]=abs(rMCD[j]-n2MCD[i,j])
              dwMCD[i,j]=dMCD[i,j]*wMCD[j]
            }
          }
          DWMCD=c()
          for (i in 1:length(k1$centers))
          {
            DWMCD[i]=max(dwMCD[i,])
          }
          t2MCD=cbind(DWMCD,rank(DWMCD,ties.method = "first"))
          #MULTIPICATIVE FORM
          xpwMCD=matrix(1,length(k1$centers),dim(n2MCD)[2])
          newxpwMCD=c()
          xpwcstMCD=rep(1,length(k1$centers))
          xpwbnftMCD=rep(1,length(k1$centers))
          XPWMCD=rep(1,length(k1$centers))
          for(i in 1:length(k1$centers))
          {
            for (j in 1:dim(xpwMCD)[2])
            {
              xpwMCD[i,j]=n2MCD[i,j]^wMCD[j]
              newxpwMCD=xpwMCD[i,j]

              if(sum(j==cost)>0)
              {
                xpwcstMCD[i]=xpwcstMCD[i]*newxpwMCD
              }
              if(sum(j==benefit)>0)
              {
                xpwbnftMCD[i]=xpwbnftMCD[i]*newxpwMCD
              }
            }
            XPWMCD[i]=xpwbnftMCD[i]/xpwcstMCD[i]
          }
          t3MCD=cbind(XPWMCD,rank(-XPWMCD,ties.method = "first"))
          #DOMINANCE RANKING
          TMCD=cbind(Yi=XWMCD,Zi=DWMCD,Ui=XPWMCD,rnkYi=rank(-XWMCD,ties.method = "first"),rnkZi=rank(DWMCD,ties.method = "first"),rnkUi=rank(-XPWMCD,ties.method = "first"))
          DRMCD=c()
          for (i in 1:length(k1$centers))
          {
            DRMCD[i] = (TMCD[i,4]+TMCD[i,5]+TMCD[i,6])/3
          }
          t4=cbind(DRMCD,rank(DRMCD,ties.method = "first"))
          CTMCD=cbind(Yi=XWMCD,Zi=DWMCD,Ui=XPWMCD,rnkYi=rank(-XWMCD,ties.method = "first"),rnkZi=rank(DWMCD,ties.method = "first"),rnkUi=rank(-XPWMCD,ties.method = "first"),DR=DRMCD,rnkDR=rank(DRMCD,ties.method = "first"))
          #print(CTMCD)
          #USING CLUSTER 1,2,3
          rankclus1=which(CTMCD[,8]==1)
          rankclus2=which(CTMCD[,8]==2)
          rankclus3=which(CTMCD[,8]==3)
          #cat("The selected clusters are: ",rankclus1,rankclus2,rankclus3,"\n")
          inrowclus1=which(k1$cluster==rankclus1)
          inrowclus2=which(k1$cluster==rankclus2)
          inrowclus3=which(k1$cluster==rankclus3)
          inrowclus=c(inrowclus1,inrowclus2,inrowclus3)
          inRCclus1=as.character(RCclus[k1$cluster==rankclus1])
          inRCclus2=as.character(RCclus[k1$cluster==rankclus2])
          inRCclus3=as.character(RCclus[k1$cluster==rankclus3])
          inRCclus=c(inRCclus1,inRCclus2,inRCclus3)
          newrowclus=c()
          for (i in 1:length(inRCclus))
          {
            newrowclus[i]=NoI[a[,letter2num(codecriterion)]==inRCclus[i]]
          }
          rowclus=newrowclus
          rowclus=sort(rowclus)
          newrowclus=sort(newrowclus)
          inrowclus=sort(inrowclus)
          inRCclus=sort(inRCclus)
          innewrowclus=c()
          for (i in 1:length(rowclus))
          {
            innewrowclus[i]=which(ogrowclus==newrowclus[i])
          }
          RCclus1=RCclus[inrowclus1]
          RCclus2=RCclus[inrowclus2]
          RCclus3=RCclus[inrowclus3]
          selRCclus=c(RCclus1,RCclus2,RCclus3)
          selRCclus=sort(selRCclus)
          #cat("Cluster Rank 1: ",as.character(RCclus1),"\n")
          #cat("Cluster Rank 2: ",as.character(RCclus2),"\n")
          #cat("Cluster Rank 3: ",as.character(RCclus3),"\n")
          resrowclus=inrowclus
          inrowclus=innewrowclus
          if(length(innewrowclus)<=8)
          {
            totrowclus=c(totrowclus,rowclus)
            plot(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]][inrowclus],a[,letter2num(clustercriterion)][a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[k]][inrowclus],xlim =c(0,max(NoI)+1),ylim = c(0,max(a[,letter2num(clustercriterion)])+1),xlab = "Number of Potential Candidates",ylab = "Total Work Experience (Years)",col="red",pch=16,cex.lab=1.5,cex.axis=1.5,cex=2.5)
            legend("topright",levels(a[,letter2num(labelcriterion)])[k])
            inclus=k
            xw=matrix(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]),dim(MCDb)[2])
            newxw=c()
            XW=rep(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]))
            for(i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]))
            {
              for (j in 1:dim(MCDb)[2])
              {
                xw[i,j]=n2M[NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][i]+1,j]*w[j]
                newxw=xw[i,j]

                if(sum(j==cost)>0)
                {
                  XW[i]=XW[i]-newxw
                }
                if(sum(j==benefit)>0)
                {
                  XW[i]=XW[i]+newxw
                }
              }
            }
            t1=cbind(XW,rank(-XW,ties.method = "first"))
            #REFERENCE POINT
            r=c()
            d=matrix(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]),dim(MCDb)[2])
            dw=matrix(0,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]),dim(MCDb)[2])
            for (i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]))
            {
              for (j in 1:dim(MCDb)[2])
              {
                if(sum(j==cost)>0)
                {
                  r[j]=min(n2M[,j],na.rm = TRUE)
                }
                if(sum(j==benefit)>0)
                {
                  r[j]=max(n2M[,j],na.rm = TRUE)
                }
                d[i,j]=abs(r[j]-n2M[NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][i]+1,j])
                dw[i,j]=d[i,j]*w[j]
              }
            }
            DW=c()
            for (i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]))
            {
              DW[i]=max(dw[i,])
            }
            t2=cbind(DW,rank(DW,ties.method = "first"))
            #MULTIPICATIVE FORM
            xpw=matrix(1,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]),dim(MCDb)[2])
            newxpw=c()
            XPW=rep(1,length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]))
            for(i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]))
            {
              for (j in 1:dim(MCDb)[2])
              {
                xpw[i,j]=n2M[NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][i]+1,j]^w[j]
                newxpw=xpw[i,j]

                if(sum(j==cost)>0)
                {
                  XPW[i]=XPW[i]/newxpw
                }
                if(sum(j==benefit)>0)
                {
                  XPW[i]=XPW[i]*newxpw
                }
              }
            }
            t3=cbind(XPW,rank(-XPW,ties.method = "first"))
            #DOMINANCE Theory
            Tt=cbind(Yi=XW,Zi=DW,Ui=XPW,rnkYi=rank(-XW,ties.method = "first"),rnkZi=rank(DW,ties.method = "first"),rnkUi=rank(-XPW,ties.method = "first"))
            DR=c()
            for (i in 1:length(NoI[a[,letter2num(labelcriterion)]==levels(a[,letter2num(labelcriterion)])[inclus]][innewrowclus]))
            {
              DR[i] = (Tt[i,4]+Tt[i,5]+Tt[i,6])/3
            }
            t4=cbind(DR,rank(DR,ties.method = "first"))
            CT=cbind(Rcrtmntlvl=k,RcrtmntCd=as.character(a[,letter2num(codecriterion)][selRCclus]),RcrtmntDprtmmt=as.character(levels(a[,letter2num(labelcriterion)])[k]),Yi=XW,Zi=DW,Ui=XPW,rnkYi=rank(-XW,ties.method = "first"),rnkZi=rank(DW,ties.method = "first"),rnkUi=rank(-XPW,ties.method = "first"),DR,rnkDR=rank(DR,ties.method = "first"))
            CT=CT[,-10]
            print(CT)
            rankc1=which(CT[,10]==1)
            rankc2=which(CT[,10]==2)
            rankc3=which(CT[,10]==3)
            rankc4=which(CT[,10]==4)
            rankc=c(rankc1,rankc2,rankc3,rankc4)
            if(k==1)
            {
              totCT=CT[rankc,]
            }
            if(k!=1)
            {
              totCT=rbind(totCT,CT[rankc,])
            }
            finRCclus=RCclus[resrowclus][rankc]
            cat("Your final results for classification/category/section",k,"are as follows:\n")
            cat(as.character(finRCclus),"\n")
            print(NoI[finRCclus])
            newfinRCclus=as.character(as.factor(finRCclus))[1:4]
            totRCclus=c(totRCclus,newfinRCclus)
          }
        }
      }
      csvout=readline(prompt = "If you wish to have a CSV output file write Y, otherwise write N :  ")
      if(csvout=="Y")
      {
        print("Select a CSV file for to save the final outcomes/results")
        if(k==1)
        {
          write.table(CT[rankc,],file=file.choose(),sep = ",",row.names = FALSE,col.names = TRUE,append = FALSE)
        }
        if(k!=1)
        {
          oldwarn=getOption("warn")
          options(warn = -1)
          write.table(CT[rankc,],file=file.choose(),sep = ",",row.names = FALSE,col.names = TRUE,append = TRUE)
          options(warn = oldwarn)
        }
      }
    }
    assign(lt[q],totRCclus)
    assign(LT[q],totCT)
  }
  cntr=1:length(lt)
  print("The results/outcomes of the algorithm are as follows: ")
  for(v in 1:length(flcnt))
  {
    for (u in 1:length(cntr))
    {
      zr=0
      cat("Results for the input CSV file number",cntr,"\n")
      for (cn in 1:length(levels(a[,letter2num(labelcriterion)])))
      {
        cat("Results of section/category/classification",cn,"\n")
        print(c[(4*zr+1):(4*zr+4)],na.print = "")
        print(C[(4*zr+1):(4*zr+4),],na.print = "")
        zr=zr+1
      }
    }
  }
}
