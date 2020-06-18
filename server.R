library(shiny)
library(ggplot2)
library(shinythemes)
library(shinyalert)
library(dplyr)
library(httr)
library(shinyalert)
library(shinycssloaders)
library(V8)
library(rtweet)
library(gtools)
library(uuid)
library(jsonlite)
library(scales)
library(shinyjs)
library(gender)
library(genderdata)
library(wru)
library(shinybusy)
library(ggrepel)
library(extrafont)

function(input, output, session) {
  
  splitlast=function(x){
    thislast=tolower(x) %>% gsub("[[:punct:]]","",.) %>%
      gsub(" phd","",.) %>% gsub(" md","",.)
    out=tail(strsplit(thislast," ")[[1]],1)
    if(length(out)>0){
      return(out)
    }else{
      return(NA)
    }
  }
  splitfirst=function(x){
    thisfirst=tolower(x) %>% gsub("[[:punct:]]","",.) %>%
      gsub("dr ","",.) %>% gsub("mr ","",.) %>% gsub("ms ","",.)
    out=head(strsplit(thisfirst," ")[[1]],1)
    if(length(out)>0){
      return(out)
    }else{
      return(NA)
    }
  }
  matchgend=function(name,gends){
    if(name%in%gends$name){
      pm=unique(gends$proportion_male[gends$name==name])
      pw=1-pm
      return(c(pm,pw))
    }else{
      return(c(0,0))
    }
  }
  getPriors=function(df){
    if(nrow(df)>1000){
      df=df[sample(1:nrow(df),1000,replace=F),]
    }
    surname=unlist(lapply(as.character(df$name),splitlast))
    given=unlist(lapply(as.character(df$name),splitfirst))
    
    womdesc="pronoun: she|she,her|she/her|woman| latina |pronoun.is/she|female|
    mother|mom|wife| girl | gal "
    mandesc="pronoun: he|he,his|he/his|he,him|he/him|pronoun.is/he| guy | latino |
    father|dad| boy | man |husband| man."
    nbdesc="nonbinary|non-binary|non binary|they/them|them/they|they,them|
    them,they|pronoun.is/they|pronoun.is/foo|pronoun.is/zie|
    pronoun.is/hir| nb |she/they|she,they|he/they|he,they| any pronoun|genderqueer"
    man=grepl(mandesc,tolower(as.character(df$description)))
    woman=grepl(womdesc,tolower(as.character(df$description)))
    nonbin=grepl(nbdesc,tolower(as.character(df$description)))
    
    blackdesc=" afro|african|black"
    hispdesc=" latin| chican"
    middesc=" arab|#mena|[\u0600-\u06ff]|[\u0750-\u077f]|[\ufb50-\ufc3f]|[\ufe70-\ufefc]"
    nativedesc="native american|indigenous|nativetwitter"
    noblm_desc=gsub("blacklivesmatter|black lives matter","",
                    tolower(as.character(df$description)))
    black=grepl(blackdesc,noblm_desc)
    hisp=grepl(hispdesc,tolower(as.character(df$description)))
    mideast=grepl(middesc,tolower(as.character(df$description)))
    native=grepl(nativedesc,tolower(as.character(df$description)))
    
    newdf=data.frame(given=as.character(given),
                     surname=as.character(surname),stringsAsFactors=F)
    race=suppressWarnings(merge_surnames(newdf,surname.year=2010,
                                         clean.surname=T,impute.missing=F))
    
    gends1=gender(newdf$given)
    gends=do.call(rbind,lapply(newdf$given,matchgend,gends1))
    nbcol=rep(0,nrow(gends));nbcol[apply(gends,1,sum)>0]=0.005
    gends=cbind(gends,as.numeric(nbcol))
    gends[man,1]=1; gends[man,2:3]=0
    gends[woman,2]=1; gends[woman,c(1,3)]=0
    gends[nonbin,3]=1; gends[nonbin,1:2]=0
    
    races=cbind(race[,c(4,5,7,6)],.5*race[,8],.5*race[,8])
    races[hisp,4]=1; races[hisp,c(1:3,5,6)]=0
    races[black,2]=1; races[black,c(1,3:6)]=0
    races[mideast,5]=1; races[mideast,c(1:4,6)]=0
    races[native,6]=1; races[native,c(1:5)]=0
    races[is.na(races)]=0
    
    comb=as.matrix(cbind(races*gends[,1],races*gends[,2],races*gends[,3]))
    size=sum(apply(comb,1,sum)>0)
    alpha=apply(comb,2,sum)
    return(list(alpha=alpha,size=size))
  }
  oauth_sig=function(url, method, token = NULL,token_secret = NULL, private_key = NULL, ...){
    httr::oauth_header(
      httr::oauth_signature(url, method, app, token, token_secret, private_key,
                            other_params = list(...)))
  }
  get_authorization_url=function(app, callback_url, permission=NULL){
    private_key <- NULL
    response <- httr::POST("https://api.twitter.com/oauth/request_token",
                           oauth_sig("https://api.twitter.com/oauth/request_token",
                                     "POST", private_key = NULL, oauth_callback = callback_url))
    httr::stop_for_status(response)
    params <- httr::content(response, type = "application/x-www-form-urlencoded")
    authorize_url <- httr::modify_url("https://api.twitter.com/oauth/authenticate",
                                      query = list(oauth_token = params$oauth_token, 
                                                   permission = permission))
    authorize_url 
  }
  get_access_token=function(app,oauth_token,oauth_verifier){
    url <- paste0("https://api.twitter.com/oauth/access_token?oauth_token=",
                  oauth_token, "&oauth_verifier=", oauth_verifier)
    response <- httr::POST(url, oauth_sig(url, "POST", private_key = NULL))
    if(response$status_code == 200L){
      results <- content(response,type = "application/x-www-form-urlencoded",
                         encoding = "UTF-8")
      # since storing the user id might be creepy
      results[["user_id"]] <- NULL
      results
    } else {
      NULL
    }
  }
  
  app <- oauth_app(
    app = "",
    key = keys$consumer_key,
    secret = keys$consumer_secret
  )
  
  x=c("White","Black","Asian","Hispanic/Latinx","Middle Eastern","Native American")
  y=c("Man","Woman","Non-binary")
  expand=expand.grid(x,y)
  expand$Var1=as.character(expand$Var1)
  expand$Var2=as.character(expand$Var2)
  rows=rep(1:length(unique(expand$Var2)),
           each=length(unique(expand$Var1)))
  cols=rep(1:length(unique(expand$Var1)),
           length(unique(expand$Var2)))
  numcats=length(x)*length(y)
  
  if(!exists("events")){
    events <- reactiveValues(alpha_auto=rep(.1,numcats),num_auto=0,
                             alpha_man=rep(.1,numcats),index=1,true_ind=1,
                             nums=rdirichlet(1000,rep(.1,numcats)),
                             prop_man=0,this_ind=0,
                             url=paste0("location.href='",
                                        get_authorization_url(app,
                                                              callback_url="https://jdwor.shinyapps.io/WhoYouFollow/"),
                                        "';"),
                             friend.df=NULL,num_friends=100,showres="Yes",user=NULL,
                             gendgoals=c(49,50,1),racegoals=c(60,14,6,18,1,1))
  }
  
  options(httr_oob_default=TRUE)
  
  access_token <- reactive({
        
    # is the user is coming in from having just authenticated? 
    # if yes save the tokens, if not then no keys to user
    access_token <- NULL
    query <- getQueryString(session)
    if(!is.null(query) 
       && !is.null(query$oauth_token) 
       && !is.null(query$oauth_verifier)){ 
      access_token <- get_access_token(app, query$oauth_token, query$oauth_verifier)
      events$user=access_token$screen_name
    } 
    
    # turn the information from the file into a valid token object
    tryCatch({
      #setup_twitter_oauth(keys$consumer_key, keys$consumer_secret, 
      #                    access_token = access_token$oauth_token, 
      #                    access_secret = access_token$oauth_token_secret)
      create_token(app="WhoYouFollow",
                   keys$consumer_key,keys$consumer_secret,
                   access_token=access_token$oauth_token,
                   access_secret=access_token$oauth_token_secret)
    }, error = function(e) {
      NULL
    })
  })
  
  observe({
    access_token()
  })
  
  observeEvent(is.null(input$descrip), {
    thisurl=paste0(session$clientData$url_protocol,
                   session$clientData$url_hostname,
                   session$clientData$url_pathname,
                   session$clientData$url_port,
                   session$clientData$url_search)
    if(!grepl("oauth",thisurl) & is.null(events$user)){
      showModal(loginModal())
    }else{
      showModal(userModal())
    }
  })
  
  loginModal <- function(failed = FALSE) {
    modalDialog(
      h2(strong("Who do you follow?"), class="title"),
      h3("Estimating race and gender on your Twitter"),
      br(),
      p("Many people in science, tech, and journalism follow mostly white men.
      While other tools have been created to provide users with gender breakdowns of who they follow on Twitter (e.g., proporti.onl),
      we wanted to incorporate information on race, as well as the intersection of race and gender. Algorithms are far from ideal in these contexts, 
      so this tool supplements name-based gender/race estimates with information drawn directly from Twitter bios. It also gives you the option to 
      improve the initial automatic estimates by manually entering demographic information for a sample of the people you follow. 
      Algorithmic, bio-based, and second-hand assessments of individuals' gender and race can all be highly flawed
      and are likely to be wrong in some cases. But in our opinion it is better than not making an effort at all."),
      br(),
      p("To use this tool, you have to log in using your twitter account using the link below. No information is stored about your account,
      the accounts of those you follow, or anything you type into this application. Email jordandworkin@gmail.com with any questions."),
      if (failed)
        div(tags$b("Sorry, this app can't be used without authentication",
                   style = "margin-left:2%; color: red;")),
      footer = tagList(
        actionButton("no", "Cancel", style="font-size: 1em; font-family: Garamond"),
        actionButton("ok","Log in",onclick=events$url,
                     style="font-size: 1em; font-family: Garamond")  
      )
    )
  }
  descripModal <- function() {
    modalDialog(
      h3("Description"),
      br(),
      p("Many people in science, tech, and journalism follow mostly white men.
      While other tools have been created to provide users with gender breakdowns of who they follow on Twitter (e.g., proporti.onl),
      we wanted to incorporate information on race, as well as the intersection of race and gender. Algorithms are far from ideal in these contexts, 
      so this tool supplements name-based gender/race estimates with information drawn directly from Twitter bios. It also gives you the option to 
      improve the initial automatic estimates by manually entering demographic information for a sample of the people you follow. 
      Algorithmic, bio-based, and second-hand assessments of individuals' gender and race can all be highly flawed
      and are likely to be wrong in some cases. But in our opinion it is better than not making an effort at all.")
    )
  }
  methodsModal <- function() {
    modalDialog(
      h3("Methods"),
      br(),
      p("Automated assessment of race is conducted using the 'wru' R package, and automated assessment of gender is conducted using the 'gender' R package.
        Both procedures rely on United States Census data; names are given a probability of belonging to someone in each gender/racial group 
        based on comparison to historical datasets. In this tool, both gender and race probabilities are replaced by self-identified gender/race when  
        they can be inferred from Twitter bios. You are also given the option to manually enter demographic information for a sample of people you follow.
        If you choose to do so, you will be able to view results that combine or replace the algorithmic assessments with the data you've provided.
        The inclusion of manual information can also greatly increase the accuracy and precision of the results. 
        
        Feel free to email jordandworkin@gmail.com with any questions or concerns.")
    )
  }
  goalsModal <- function(failed=FALSE){
    modalDialog(
      h3("Edit the goal/benchmark Twitter make-up"),
      br(),
      p("Gender (%)"),
      fluidRow(
        column(3),
        column(2,numericInput("men","Men",value=events$gendgoals[1],min=0,max=100)),
        column(2,numericInput("women","Women",value=events$gendgoals[2],min=0,max=100)),
        column(2,numericInput("nonbin","Non-binary",value=events$gendgoals[3],min=0,max=100)),
        column(3)
      ),
      p("Race (%)"),
      fluidRow(
        column(3),
        column(2,numericInput("white","White",value=events$racegoals[1],min=0,max=100)),
        column(2,numericInput("black","Black",value=events$racegoals[2],min=0,max=100)),
        column(2,numericInput("asian","Asian",value=events$racegoals[3],min=0,max=100)),
        column(3)
      ),
      fluidRow(
        column(3),
        column(2,numericInput("hispanic","Hispanic/\nLatinx",value=events$racegoals[4],min=0,max=100)),
        column(2,numericInput("mideast","Middle Eastern",value=events$racegoals[5],min=0,max=100)),
        column(2,numericInput("native","Native American",value=events$racegoals[6],min=0,max=100)),
        column(3)
      ),
      if (failed)
        div(tags$b("Entries in each category must sum to 100",
                   style = "margin-left:2%; color: red;")),
      footer = tagList(
        actionButton("canc","Cancel",style="font-size: 1em; font-family: Garamond"),
        actionButton("sub","Submit",style="font-size: 1em; font-family: Garamond")
      )
    )
  }
  userModal <- function(failed=FALSE){
    modalDialog(
      h3("Enter Twitter username"),
      br(),
      fluidRow(
        align="center",
        textInput("user","",value=events$user)
      ),
      if (failed)
        div(tags$b("Sorry, an error occurred. It is likely that either (A) the username you entered cannot be found,
                   or (B) you have run out of Twitter API queries (15 searches every 15 minutes).",
                   style = "margin-left:2%; color: red;")),
      footer = tagList(
        actionButton("canc","Cancel",style="font-size: 1em; font-family: Garamond"),
        actionButton("newuser","Submit",style="font-size: 1em; font-family: Garamond")
      )
    )
  }
  
  observeEvent(input$no, {
    showModal(loginModal(failed = TRUE))
  })
  observeEvent(input$canc, {
    removeModal()
  })
  observeEvent(input$sub, {
    allgends=c(input$men,input$women,input$nonbin)
    allrace=c(input$white,input$black,input$asian,
              input$hispanic,input$mideast,input$native)
    if(sum(allgends)!=100 | sum(allrace)!=100){
      showModal(goalsModal(failed = TRUE))
    }else{
      events$gendgoals=allgends
      events$racegoals=allrace
      removeModal()
    }
  })
  
  observeEvent(input$newuser, {
    if(is.null(input$user)){
      access_token()
    }else if(substr(input$user,1,1)=="@"){
      user=substr(input$user,2,nchar(input$user))
      tryCatch({
        show_modal_spinner(
          spin = "double-bounce",
          color = "#2f4f4f",
          text = "Loading (could take up to a minute)"
        )
        if(!dir.exists('~/.fonts')){
          dir.create('~/.fonts')
          file.copy("www/EBGaramond.ttf", "~/.fonts")
          system('fc-cache -f ~/.fonts')
        }
        num_friends=lookup_users(user)$friends_count
        if(num_friends>5000){
          friends=NULL
          uplim=min(3,ceiling(num_friends/5000))
          for(i in 1:uplim){
            friends=c(friends,get_friends(user,n=num_friends,
                                          page=paste0("-",i))$user_id)
          }
          friends=unique(friends)
        }else{
          friends=get_friends(user)$user_id
        }
        friends=lookup_users(friends)
        events$user=user
        info=friends %>% select(profile_image_url,name,screen_name,description)
        rownames(info)=NULL; rm(friends)
        friend.df=data.frame(image=info[,1],name=info[,2],handle=info[,3],description=info[,4])
        rand.ord=sample(1:nrow(friend.df),nrow(friend.df),replace=F)
        friend.df=friend.df[rand.ord,]
        events$friend.df=friend.df
        events$num_friends=nrow(friend.df)
        priors=getPriors(friend.df)
        events$alpha_auto=priors$alpha
        events$alpha_man=rep(.1,numcats)
        events$index=1; events$true_ind=1
        events$num_auto=priors$size
        events$this_ind=priors$size
        events$prop_man=0
        events$nums=rdirichlet(1000,alpha=events$alpha_auto)
        remove_modal_spinner()
        removeModal()
      }, error = function(e) {
        showModal(userModal(failed=TRUE))
      })
    }else{
      user=input$user
      tryCatch({
        show_modal_spinner(
          spin = "double-bounce",
          color = "#2f4f4f",
          text = "Loading (could take up to a minute)"
        )
        if(!dir.exists('~/.fonts')){
          dir.create('~/.fonts')
          file.copy("www/EBGaramond.ttf", "~/.fonts")
          system('fc-cache -f ~/.fonts')
        }
        num_friends=lookup_users(user)$friends_count
        if(num_friends>5000){
          friends=NULL
          uplim=min(3,ceiling(num_friends/5000))
          for(i in 1:uplim){
            friends=c(friends,get_friends(user,n=num_friends,
                                          page=paste0("-",i))$user_id)
          }
          friends=unique(friends)
        }else{
          friends=get_friends(user)$user_id
        }
        friends=lookup_users(friends)
        events$user=user
        info=friends %>% select(profile_image_url,name,screen_name,description)
        rownames(info)=NULL; rm(friends)
        friend.df=data.frame(image=info[,1],name=info[,2],handle=info[,3],description=info[,4])
        rand.ord=sample(1:nrow(friend.df),nrow(friend.df),replace=F)
        friend.df=friend.df[rand.ord,]
        events$friend.df=friend.df
        events$num_friends=nrow(friend.df)
        priors=getPriors(friend.df)
        events$alpha_auto=priors$alpha
        events$alpha_man=rep(.1,numcats)
        events$index=1; events$true_ind=1
        events$num_auto=priors$size
        events$this_ind=priors$size
        events$prop_man=0
        events$nums=rdirichlet(1000,alpha=events$alpha_auto)
        remove_modal_spinner()
        removeModal()
      }, error = function(e) {
        showModal(userModal(failed=TRUE))
      })
    }
  }, ignoreNULL=TRUE)
  
  observeEvent(input$changeuser, {
    showModal(userModal())
  })
  observeEvent(input$editgoals, {
    showModal(goalsModal())
  })
  observeEvent(input$descrip, {
    showModal(descripModal())
  })
  observeEvent(input$methods, {
    showModal(methodsModal())
  })
  
  output$picture <- renderText({
    if(!is.null(events$friend.df)){
      src=as.character(events$friend.df$profile_image_url[events$index])
      c('<img src="',src,'" style="margin-top:15px;">')
    }
  })
  output$thisfriend <- renderTable({
    if(!is.null(events$friend.df)){
      thisfriend=events$friend.df[events$index,2:4]
      colnames(thisfriend)=c("Name","Username","Description")
      thisfriend
    }
  }, width="95%")
  
  output$counter <- renderText({
    if(!is.null(events$num_friends)){
      paste0(events$index, " of ", events$num_friends)
    }
  })
  
  observeEvent(input$showres,{
    if(events$showres=="No"){
      showElement(id="row1")
      showElement(id="row2")
      showElement(id="row3")
      showElement(id="row4")
      updateActionButton(session,"showres",
                         label="Hide results (hide for faster updating)")
      events$showres = "Yes"
    }else{
      hideElement(id="row1")
      hideElement(id="row2")
      hideElement(id="row3")
      hideElement(id="row4")
      updateActionButton(session,"showres",
                         label="Show results (hide for faster updating)")
      events$showres = "No"
    }
  })
  
  output$resultshead=renderUI({
    if(is.null(events$user)){
      h3(strong(paste0("Estimated gender and racial breakdown of your Twitter")))
    }else{
      h3(strong(paste0("Estimated gender and racial breakdown of ",events$user,"'s Twitter")))
    }
  })
  
  output$sumhead=renderUI({
    usrates=c(49,50,1,60,14,6,18,1,1)
    current=c(events$gendgoals,events$racegoals)
    if(identical(usrates,current)){
      h3("Compared to the US population*, the following groups appear to be:")
    }else{
      h3("Compared to the given benchmarks*, the following groups appear to be:")
    }
  })
  
  output$weights=renderUI({
    if(!is.null(events$num_friends))
    if(events$true_ind<25 & (events$index/events$num_friends)<.25){
      radioButtons('relweight', label="Relative weight of automated/manual assessments (add manual data for more options)", 
                   choices=c("Proportional","100% automated"),
                   selected="Proportional",inline=T)
    }else if(events$true_ind<75 & (events$index/events$num_friends)<0.5){
      radioButtons('relweight', label="Relative weight of automated/manual assessments (add manual data for more options)", 
                   choices=c("Proportional","50/50","100% automated"),
                   selected="Proportional",inline=T)
    }else{
      radioButtons('relweight', label="Relative weight of automated/manual assessments", 
                   choices=c("Proportional","50/50","100% automated","100% manual"),
                   selected="Proportional",inline=T)
    }
  })
  
  observeEvent(input$submit, {
    if(input$relweight=="Proportional"){
      prop_man=events$true_ind/(events$true_ind+events$num_auto)
    }else if(input$relweight=="100% automated"){
      prop_man=0
    }else if(input$relweight=="50/50"){
      prop_man=0.50
    }else{
      prop_man=1
    }
    events$prop_man=prop_man
    old_alph=as.numeric(events$alpha_man)
    new_alph=old_alph
    if(input$gend!="Unknown/NA" & input$race!="Unknown/NA" & 
       events$index<=events$num_friends){
      ind=which(expand$Var1==as.character(input$race) & 
                  expand$Var2==as.character(input$gend))
      new_alph[ind]=old_alph[ind]+1
      events$true_ind=as.numeric(events$true_ind)+1
    }
    new_alph=as.numeric(new_alph)
    events$alpha_man=new_alph
    full_alph=prop_man*new_alph+(1-prop_man)*events$alpha_auto
    events$this_ind=prop_man*events$true_ind+(1-prop_man)*events$num_auto
    events$nums=rdirichlet(1000,alpha=full_alph)
    events$index=as.numeric(events$index)+1
  })
  
  observeEvent(input$relweight, {
    if(input$relweight=="Proportional"){
      prop_man=events$true_ind/(events$true_ind+events$num_auto)
    }else if(input$relweight=="100% automated"){
      prop_man=0
    }else if(input$relweight=="50/50"){
      prop_man=0.50
    }else{
      prop_man=1
    }
    full_alph=prop_man*events$alpha_man+(1-prop_man)*events$alpha_auto
    events$prop_man=prop_man
    events$this_ind=prop_man*events$true_ind+(1-prop_man)*events$num_auto
    events$nums=rdirichlet(1000,alpha=full_alph)
  })
  
  output$allplot <- renderPlot({
    if(events$showres=="Yes" & mean(events$alpha_auto)>0.1){
      ests=apply(events$nums,2,mean)
      lbs=apply(events$nums,2,quantile,0.025)
      ubs=apply(events$nums,2,quantile,0.975)
      scale=sqrt((events$num_friends-events$this_ind+1)/(events$num_friends-1))
      lbs=ests-scale*(ests-lbs)
      ubs=ests+scale*(ubs-ests)
      
      labels=paste0(round(ests*100,0),"%")
      labels[ests<0.01]=""
      cats=data.frame(Race=expand$Var1,Gender=expand$Var2,
                      Ests=ests,LBs=lbs,UBs=ubs,
                      Labels=labels,stringsAsFactors=F)
      cats$Gender[cats$Gender=="Non-binary"]="Non-\nbinary"
      cats$Race[cats$Race=="Hispanic/Latinx"]="Hispanic/\nLatinx"
      cats$Race[cats$Race=="Middle Eastern"]="Middle\nEastern"
      cats$Race[cats$Race=="Native American"]="Native\nAmerican"
      cats$Race=factor(cats$Race,levels=rev(c("White","Black","Asian",
                                              "Hispanic/\nLatinx","Middle\nEastern",
                                              "Native\nAmerican")))
      cats$Gender=factor(cats$Gender,levels=rev(c("Woman","Man","Non-\nbinary")))
      
      ggplot(cats, aes(y=Ests, x=Race, col=Gender)) +
        geom_errorbar(aes(ymin=LBs,ymax=UBs),width=.1,alpha=.6)+
        geom_point(alpha=.8,size=4) + theme_bw() +
        theme(legend.position="bottom",
              axis.title=element_text(size=16,family="EBGaramond"),
              axis.text=element_text(size=14,family="EBGaramond"),
              legend.text=element_text(size=14,family="EBGaramond"),
              legend.title=element_text(size=14,family="EBGaramond")) + 
        geom_text_repel(aes(x=Race,y=Ests,label=Labels,col=Gender),
                        size=5,vjust=-1,hjust=0,fontface="bold",
                        family="EBGaramond",force=.2,min.segment.length=5,
                        show.legend = FALSE) +
        coord_flip() + scale_y_continuous(labels = percent) +
        xlab(NULL) + ylab("Estimated proportion") + 
        scale_color_manual(values=c("Man"="darkslateblue","Woman"="lightsalmon3",
                                    "Non-\nbinary"="darkslategray4"),
                           labels=c("Man"="Man","Woman"="Woman",
                                    "Non-\nbinary"="Non-binary"))
    }else{
      ggplot()
    }
  })
  
  sum_cat=function(vals,cats,ucats){
    newvals=NULL
    for(i in ucats){
      newvals=c(newvals,sum(vals[cats==i]))
    }
    return(newvals)
  }
  
  output$raceplot <- renderPlot({
    if(events$showres=="Yes" & mean(events$alpha_auto)>0.1){
      this_race=unique(expand$Var1)
      this_nums=t(apply(events$nums,1,sum_cat,expand$Var1,this_race))
      ests=apply(this_nums,2,mean)
      lbs=apply(this_nums,2,quantile,0.025)
      ubs=apply(this_nums,2,quantile,0.975)
      scale=sqrt((events$num_friends-events$this_ind+1)/(events$num_friends-1))
      lbs=ests-scale*(ests-lbs)
      ubs=ests+scale*(ubs-ests)
      
      cats=data.frame(Race=this_race,Ests=ests,LBs=lbs,UBs=ubs,
                      Labels=paste0(round(ests*100,0),"%"),stringsAsFactors=F)
      cats$Race[cats$Race=="Hispanic/Latinx"]="Hispanic/\nLatinx"
      cats$Race[cats$Race=="Middle Eastern"]="Middle\nEastern"
      cats$Race[cats$Race=="Native American"]="Native\nAmerican"
      cats$Race=factor(cats$Race,levels=rev(c("White","Black","Asian",
                                              "Hispanic/\nLatinx","Middle\nEastern",
                                              "Native\nAmerican")))
      
      ggplot(cats, aes(y=Ests, x=Race),col="slategray4") +
        geom_errorbar(aes(ymin=LBs,ymax=UBs),width=.1,alpha=.6)+
        geom_point(alpha=.8,size=4) + theme_bw() +
        theme(legend.position="bottom",
              axis.title=element_text(size=16,family="EBGaramond"),
              axis.text=element_text(size=14,family="EBGaramond")) +
        geom_text_repel(aes(x=Race,y=Ests,label=Labels),
                        col="gray20",size=5,vjust=-1,hjust=.2,fontface="bold",
                        family="EBGaramond",force=.2,min.segment.length=5) +
        coord_flip() + scale_y_continuous(labels = percent) +
        xlab(NULL) + ylab("Estimated proportion")
    }else{
      ggplot()
    }
  })
  
  output$gendplot <- renderPlot({
    if(events$showres=="Yes" & mean(events$alpha_auto)>0.1){
      this_gend=unique(expand$Var2)
      this_nums=t(apply(events$nums,1,sum_cat,expand$Var2,this_gend))
      ests=apply(this_nums,2,mean)
      lbs=apply(this_nums,2,quantile,0.025)
      ubs=apply(this_nums,2,quantile,0.975)
      scale=sqrt((events$num_friends-events$this_ind+1)/(events$num_friends-1))
      lbs=ests-scale*(ests-lbs)
      ubs=ests+scale*(ubs-ests)
      
      cats=data.frame(Gender=this_gend,Ests=ests,LBs=lbs,UBs=ubs,
                      Labels=paste0(round(ests*100,0),"%"),stringsAsFactors=F)
      cats$Gender[cats$Gender=="Non-binary"]="Non-\nbinary"
      cats$Gender=factor(cats$Gender,levels=rev(c("Woman","Man","Non-\nbinary")))
      
      ggplot(cats, aes(y=Ests, x=Gender),col="slategray4") +
        geom_errorbar(aes(ymin=LBs,ymax=UBs),width=.05,alpha=.6)+
        geom_point(alpha=.8,size=4) + theme_bw() +
        theme(legend.position="bottom",
              axis.title=element_text(size=16,family="EBGaramond"),
              axis.text=element_text(size=14,family="EBGaramond")) + 
        geom_text_repel(aes(x=Gender,y=Ests,label=Labels),
                        col="gray20",size=5,vjust=-1,hjust=.2,fontface="bold",
                        family="EBGaramond",force=.2,min.segment.length=5) +
        coord_flip() + scale_y_continuous(labels = percent) +
        xlab(NULL) + ylab("Estimated proportion")
    }else{
      ggplot()
    }
  })
  
  output$wordtab <- renderTable({
    if(events$showres=="Yes" & mean(events$alpha_auto)>0.1){
      me=apply(events$nums,2,mean)
      lb=apply(events$nums,2,quantile,0.1)
      ub=apply(events$nums,2,quantile,0.9)
      scale=sqrt((events$num_friends-events$this_ind+1)/(events$num_friends-1))
      lb=me-scale*(me-lb); lb[as.numeric(events$alpha_auto)<=0.1 &
                                as.numeric(events$alpha_man)<=0.1]=0
      ub=me+scale*(ub-me)
      
      #un_race=unique(expand$Var1)
      #un_gend=unique(expand$Var2)
      #this_race=t(apply(events$nums,1,sum_cat,expand$Var1,un_race))
      #this_gend=t(apply(events$nums,1,sum_cat,expand$Var2,un_gend))
      
      #me.r=apply(this_race,2,mean); me.g=apply(this_gend,2,mean)
      #lb.r=apply(this_race,2,quantile,0.05); lb.g=apply(this_gend,2,quantile,0.05)
      #ub.r=apply(this_race,2,quantile,0.95); ub.g=apply(this_gend,2,quantile,0.95)
      #lb.r=me.r-scale*(me.r-lb.r); lb.g=me.g-scale*(me.g-lb.g)
      #ub.r=me.r+scale*(ub.r-me.r); ub.g=me.g+scale*(ub.g-me.g)
      
      #lb=c(lb.r,lb.g,lb)
      #ub=c(ub.r,ub.g,ub)
      
      x=c("White","Black","Asian","Hispanic/Latinx","Middle Eastern","Native American",
          "White","Black","Asian","Hispanic/Latinx","Middle Eastern","Native American",
          "White","Black","Asian","Hispanic/Latinx","Middle Eastern","Native American")
      y=rep(c("Men","Women","Non-Binary People"),each=6)
      groups=paste(x,y)
      #groups=c(paste(un_race,"People"),un_gend,groups)
      
      allgoals=unlist(lapply(.01*as.numeric(events$gendgoals),`*`,
                             .01*as.numeric(events$racegoals)))
      #allgoals=c(.01*as.numeric(events$racegoals),.01*as.numeric(events$gendgoals),allgoals)
      
      under=groups[allgoals>ub]; len.un=length(under)
      over=groups[allgoals<lb]; len.ov=length(over)
      prop=groups[allgoals>=lb & allgoals<=ub]; len.prop=length(prop)
      #maxlen=max(len.un,len.ov,len.prop)
      maxlen=max(len.un,len.ov)
      under=c(under,rep("",maxlen-len.un))
      over=c(over,rep("",maxlen-len.ov))
      #prop=c(prop,rep("",maxlen-len.prop))
      
      summary.tab=cbind(over,under)
      colnames(summary.tab)=c("Overrepresented",
                              "Underrepresented")
      summary.tab
    }else{
      summary.tab=cbind(rep("",8),rep("",8))
      colnames(summary.tab)=c("Overrepresented",
                              "Underrepresented")
      summary.tab
    }
  })
  
  output$sumtab <- renderTable({
    if(events$showres=="Yes" & mean(events$alpha_auto)>0.1){
      mat=matrix(0,nrow=length(unique(rows)),
                 ncol=length(unique(cols)))
      me=apply(events$nums,2,mean)
      lb=apply(events$nums,2,quantile,0.025)
      ub=apply(events$nums,2,quantile,0.975)
      scale=sqrt((events$num_friends-events$this_ind+1)/(events$num_friends-1))
      lb=round(me-scale*(me-lb),3)*100; lb[as.numeric(events$alpha_auto)<=0.1 &
                                             as.numeric(events$alpha_man)<=0.1]=0
      ub=round(me+scale*(ub-me),3)*100
      me=round(me,3)*100
      tabvals=paste0(me,"% [",lb," - ",ub,"]")
      
      mat[cbind(rows,cols)]=tabvals
      
      gend_tots=t(apply(events$nums,1,sum_cat,expand$Var2,unique(expand$Var2)))
      me=apply(gend_tots,2,mean)
      lb=apply(gend_tots,2,quantile,0.025)
      ub=apply(gend_tots,2,quantile,0.975)
      lb=round(me-scale*(me-lb),3)*100
      ub=round(me+scale*(ub-me),3)*100
      me=round(me,3)*100
      tabvals=paste0(me,"% [",lb," - ",ub,"]")
      mat=cbind(mat,tabvals)
      
      race_tots=t(apply(events$nums,1,sum_cat,expand$Var1,unique(expand$Var1)))
      me=apply(race_tots,2,mean)
      lb=apply(race_tots,2,quantile,0.025)
      ub=apply(race_tots,2,quantile,0.975)
      lb=round(me-scale*(me-lb),3)*100
      ub=round(me+scale*(ub-me),3)*100
      me=round(me,3)*100
      tabvals=c(paste0(me,"% [",lb," - ",ub,"]"),"")
      mat=rbind(mat,tabvals)
      
      rownames(mat)=c(unique(expand$Var2),"Col. totals")
      colnames(mat)=c(unique(expand$Var1),"Row totals")
      mat
    }else{
      mat=matrix("",nrow=4,ncol=7)
      mat
    }
  },rownames=T)
  
}
