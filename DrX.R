
  resdf=c()
  al="abcdefghijklmnopqrstuvwxyz"
  al=strsplit(al,split="")[[1]]
  for(j in al){
    x=(readLines(paste(c("https://www.drugs.com/mcd-",j,"1.html"),collapse = "",sep="")))
  start=which(regexpr("<ul class='column-list-2 sitemap-list'>",x)==1)
  if(length(start)==0){print(j)}else{
  end=which(regexpr("</ul>",x)==1)
  end=(end[which(end>start)])[1]
  x=x[(start+1):(end-1)]
  #if(length(x)==215){print(x)}
  y=x[which(regexpr("<li><a href=",x)!=-1)]
  z=y
  url=y
  for(i in 1:length(y) )
  {
    z[i]=strsplit(strsplit(y[i],split='\">')[[1]][2],split="</a>")[[1]][1]
    url[i]=paste("https://www.drugs.com",strsplit(strsplit(y[i],split="<li><a href=\"")[[1]][2],"\">")[[1]][1],collapse="",sep = "")
    if(is.na(z[i])){print(1)}
    
  }
  
  resd=cbind(z,url)
  resdf=rbind(resdf,resd)
  }
  }
  
  View(resdf)
  sde=list()
  features=c()
  for(i in 1:nrow(resdf)){
    if(i%%10==0){print(i)}
    sd=readLines(resdf[i,2])
    sd0=which(regexpr('<h2 id="symptoms">Symptoms</h2>',sd)!=-1)
    if(length(sd0)>1){sd0=sd0[1]}
    sd01=which(regexpr('<h',sd)!=-1)
    sd01=sd01[which(sd01>sd0)[1]]
    if((length(sd01)!=0)&(!is.na(sd01))){
    features=c(features,paste0(sd[(sd0+1):(sd01-1)],collapse=""))
    }else{
      if(length(sd0)==0){features=c(features,0)}else{
      features=c(features,paste0(sd[(sd0+1):length(sd)],collapse=""))
      }
    }
    if(length(sd0)==0){sdg==0}else{
      sd1=which(regexpr('<ul>',sd)!=-1)
      sd1=sd1[which(sd1>sd0)[1]]
      if(length(sd1)!=0){
      sd2=which(regexpr('</ul>',sd)!=-1)
      sd2=sd2[which(sd2>sd0)[1]]
    sdg=paste0(sd[sd1:sd2],collapse = "")
    lst=unlist(strsplit(strsplit(x = sdg,split = ("<li>"))[[1]],split="</li>"))
    sdg=gsub(gsub(lst,replacement = "",pattern = "<ul>"),pattern = "</ul>",replacement = "")
    v=which(gsub(sdg,replacement = "",pattern = " ")=="")
    
    if(length(v)!=0){
    sdg=sdg[-v]}
    
    
      }
    }
    sde[[i]]=sdg
    sdg=0
    
  }
  #vvc="<ul><li>A pulsating feeling near the navel</li><li>Deep, constant pain in your abdomen or on the side of your abdomen</li><li>Back pain</li></ul>"
  listfeatures=sde
  x1 <- gsub("<([A-Za-z][A-Za-z0-9]*)\\b[^>]*>", " ", features)
  x1 <- gsub("</([A-Za-z][A-Za-z0-9]*)\\b[^>]*>", " ", x1)
  
  
  
  
  
  
  
  
  data("crude")
  termFreq(crude[[14]])
  strsplit_space_tokenizer <- function(x)
    unlist(strsplit(as.character(x), "[[:space:]]+"))
  ctrl <- list(tokenize = strsplit_space_tokenizer,
               removePunctuation = list(preserve_intra_word_dashes = TRUE),
               stopwords = c("reuter", "that"),
               stemming = TRUE,
               wordLengths = c(4, Inf))
  termFreq(crude[[14]], control = ctrl
  
