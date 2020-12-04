function(input, output, session) {
  
  # INCREASE SIZE OF FILES UPLOADS ----
  options(shiny.maxRequestSize=50*1024^2)
  
  # DROPDOWN FUNCTION -----
  create_dropdown <- function(input_name, choices, label) {
    if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
      selected <- input[[input_name]]
    } else {
      selected <- choices[1]
    }
    
    selectInput(
      inputId = input_name,
      label = label,
      choices = choices,
      selected = selected
    )
  }
  
## WORKFLOW IMAGE ----
#   output$image.workflow <- renderImage({
#     return(list(
#       src = "images/globalarchive-workflow.png", width="50%",align = "center",
#       contentType = "image/png",
#       alt = "Workflow"
#     ))
#   }, deleteFile = FALSE)
  
# READ IN DATA ----
  # COUNT - RAW DATAFRAME-----
  count.raw <- reactive({
    # If no file in upload read in montebello example data 
    if(is.null(input$upload.count)){
      count.raw <- fst::read_fst("data/montebello.example.complete.maxn.fst")%>%
        as.data.frame()
      }
    else{
      # Read in FST data when uploaded
      count.raw <- fst::read_fst(input$upload.count$datapath)%>%
        as.data.frame()}
    # Save count data
    count.raw <- count.raw
    
  })
  
  # LENGTH - RAW DATAFRAME-----
  length.raw <- reactive({
    # If no file in upload read in montebello example data 
    if(is.null(input$upload.length)){
      length.raw <- fst::read_fst("data/montebello.example.complete.length.fst")%>%
        as.data.frame()
    }
    else{
      # Read in FST data when uploaded
      length.raw <- fst::read_fst(input$upload.length$datapath)%>%
        as.data.frame()}
    # Save length data
    length.raw <- length.raw
    
  })
  
  
  # COUNT SUMMARY - CAMPAIGN DROPDOWN ----
  output$countsummary.campaign <- renderUI({
    
    # Use count data
    df <- count.raw()
    
    # Create a list of campaignIDs
    options <- df %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    # Add "All" as an option 
    options <- c("All", options)
    
    # Update dropdown menus
    create_dropdown("countsummary.campaign", options, "Choose a campaign:")
    
  })
  
  # LENGTH SUMMARY - CAMPAIGN DROPDOWN ----
  output$lengthsummary.campaign <- renderUI({
    
    # Use length data
    df <- length.raw()
    
    # Create a list of campaignIDs
    options <- df %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    # Add "All" as an option 
    options <- c("All", options)
    
    # Update dropdown menus
    create_dropdown("lengthsummary.campaign", options, "Choose a campaign:")
    
  })
  
  # SUMMARIES ----
  # COUNT CAMPAIGN SUMMARY----
  countsummary.campaigndata <- reactive({
    count <- count.raw()
    
    count <- count%>%
      filter(maxn>0)%>%
      dplyr::group_by(campaignid)%>%
      dplyr::summarise(total.abundance=sum(maxn),species.richness=length(unique(scientific)),family.richness=length(unique(family)),genus.richness=length(unique(genus)),number.of.samples=length(unique(id)))%>%
      ungroup()%>%
      arrange(campaignid)%>%
      dplyr::rename('Campaign'=campaignid,
                    'Total abundance'=total.abundance,
                    'Species richness'=species.richness,
                    'Family richness'=family.richness,
                    'Genus richness'=genus.richness,
                    'Number of samples'=number.of.samples)
    
    count
  })
  
  output$countsummary.campaigntable <- DT::renderDataTable(
    DT::datatable(countsummary.campaigndata(), options = list(dom = 't')
                  # lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
                  # pageLength = 15, rownames= FALSE
                  , rownames = FALSE
    )
  )
  
  # Groub by species, trophic or target summary ----
  countsummary.groupbydata <- reactive({
    
    count <- count.raw()
    
    campaignid.selected <- as.character(input$countsummary.campaign)
    
    if (input$countsummary.campaign %in% "All") {
      count <- count
      
    } else {
      count <- count%>%
        dplyr::filter(campaignid %in% campaignid.selected)
    }
    
    count <- count
    
    if (input$countsummary.groupby == "Species") {
      
      count <- count%>%
        dplyr::filter(maxn>0)%>%
        dplyr::group_by(family,genus,species)%>%
        dplyr::summarise(total.abundance=sum(maxn),number.of.samples=length(unique(id)))%>%
        ungroup()%>%
        arrange(-total.abundance)%>%
        dplyr::left_join(.,life.history)%>%
        tidyr::replace_na(list(target.group="Non-target",trophic.group="Missing trophic group"))%>%
        dplyr::rename(Family=family,
                      Genus=genus,
                      Species=species,
                      'Total abundance'=total.abundance,
                      'Number of samples'=number.of.samples,
                      'Trophic group'=trophic.group,
                      'Target group'=target.group,
                      'Common name'=australian.common.name,
                      CAAB=caab)%>%
        dplyr::select(-fish.names)
      
      count$'Trophic group' <- sub("^$", "Missing trophic group", count$'Trophic group')
    }
    
    if (input$countsummary.groupby=="Target group") {
      count <- count%>%
        dplyr::filter(maxn>0)%>%
        dplyr::left_join(.,life.history)%>%
        tidyr::replace_na(list(target.group="Non-target"))%>%
        dplyr::group_by(target.group)%>%
        dplyr::summarise(total.abundance=sum(maxn),number.of.samples=length(unique(id)))%>%
        ungroup()%>%
        arrange(-total.abundance)%>%
        dplyr::rename('Total abundance'=total.abundance,'Number of samples'=number.of.samples,'Target group'=target.group)
    }
    
    if (input$countsummary.groupby=="Trophic group") {
      count <- count %>%
        dplyr::filter(maxn>0)%>%
        dplyr::left_join(.,life.history)%>%
        dplyr::mutate_each(funs(empty_as_na),matches="trophic.group")%>%
        naniar::replace_with_na(replace = list(trophic.group = ""))%>%
        tidyr::replace_na(list(trophic.group="Missing trophic group"))%>%
        dplyr::group_by(trophic.group)%>%
        dplyr::summarise(total.abundance=sum(maxn),number.of.samples=length(unique(id)))%>%
        ungroup()%>%
        arrange(-total.abundance)%>%
        dplyr::rename('Total abundance'=total.abundance,'Number of samples'=number.of.samples,'Trophic group'=trophic.group)
      
      print(unique(count$'Trophic group'))
    }
    
    count
  })
  
  output$countsummary.groupbytable <- DT::renderDataTable(
    DT::datatable(countsummary.groupbydata(), options = list( # [, input$show_vars, drop = FALSE]
      lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
      pageLength = 15, rownames= FALSE
    ), rownames = FALSE
    )
  )
  
  # LENGTH SUMMARY ----
  lengthsummary.data <- reactive({
    
    sum.length.data <- length.raw()
    
    if (input$lengthsummary.campaign == "All") {
      sum.length.data
      
    } else {
      sum.length.data<-sum.length.data%>%
        filter(campaignid == as.character(input$lengthsummary.campaign))
    }
    
    if (input$lengthsummary.groupby=="Species") {
      
      length.data <- sum.length.data%>%
        filter(length>0)%>%
        dplyr::group_by(family,genus,species)%>%
        dplyr::summarise(Total.measured=sum(number),Number.of.samples=length(unique(id)),Mean.length=mean(length),Min.length=min(length),Max.length=max(length))%>%
        mutate(Mean.length=round(Mean.length, digits=2))%>%
        mutate(Min.length=round(Min.length, digits=2))%>%
        mutate(Max.length=round(Max.length, digits=2))%>%
        ungroup()%>%
        arrange(-Total.measured)%>%
        left_join(.,life.history)%>%
        dplyr::rename(Family=family,Genus=genus,Species=species,'Trophic group'=trophic.group,"Target group"=target.group,'Total measured'=Total.measured,'Number of samples'=Number.of.samples,'Mean length'=Mean.length,'Min length'=Min.length,'Max length'=Max.length,'Common name'=australian.common.name)%>%
        dplyr::select(-fish.names)
      
    }
    
    if (input$lengthsummary.groupby=="Target group") {
      length.data <- sum.length.data%>%
        filter(length>0)%>%
        left_join(life.history)%>%
        replace_na(list(target.group="Non-target"))%>%
        dplyr::group_by(target.group)%>%
        dplyr::summarise(Total.measured=sum(number),Number.of.samples=length(unique(id)),Mean.length=mean(length),Min.length=min(length),Max.length=max(length))%>%
        mutate(Mean.length=round(Mean.length, digits=2))%>%
        mutate(Min.length=round(Min.length, digits=2))%>%
        mutate(Max.length=round(Max.length, digits=2))%>%
        ungroup()%>%
        arrange(-Total.measured)%>%
        dplyr::rename('Total measured'=Total.measured,'Number of samples'=Number.of.samples,'Mean length'=Mean.length,'Min length'=Min.length,'Max length'=Max.length,'Target group'=target.group)
    }
    
    if (input$lengthsummary.groupby=="Trophic group") {
      length.data <- sum.length.data%>%
        dplyr::filter(length>0)%>%
        dplyr::left_join(life.history)%>%
        replace_na(list(trophic.group="Missing trophic group"))%>%
        dplyr::group_by(trophic.group)%>%
        dplyr::summarise(Total.measured=sum(number),Number.of.samples=length(unique(id)),Mean.length=mean(length),Min.length=min(length),Max.length=max(length))%>%
        dplyr::mutate(Mean.length=round(Mean.length, digits=2))%>%
        dplyr::mutate(Min.length=round(Min.length, digits=2))%>%
        dplyr::mutate(Max.length=round(Max.length, digits=2))%>%
        ungroup()%>%
        arrange(-Total.measured)%>%
        dplyr::rename('Total measured'=Total.measured,'Number of samples'=Number.of.samples,'Mean length'=Mean.length,'Min length'=Min.length,'Max length'=Max.length, 'Trophic group'=trophic.group)
    }
    length.data 
  })
  
  output$lengthsummary.table <- DT::renderDataTable(
    DT::datatable(lengthsummary.data(), options = list( #
      lengthMenu = list(c(10, 25, 50, -1), c('10', '25','50', 'All')),
      pageLength = 15, rownames= FALSE,digits=2)
    )
  )
  
  # COUNT SPECIES - CAMPAIGN DROPDOWN ----
  output$countspecies.campaign <- renderUI({
    
    # Use count data
    df <- count.raw()
    
    # Create a list of campaignIDs
    options <- df %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    # Add "All" as an option 
    options <- c("All", options)
    
    # Update dropdown menus
    create_dropdown("countspecies.campaign", options, " ")
    
  })
  
  # LENGTH SPECIES - CAMPAIGN DROPDOWN ----
  output$lengthspecies.campaign <- renderUI({
    
    # Use length data
    df <- count.raw()
    
    # Create a list of campaignIDs
    options <- df %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    # Add "All" as an option 
    options <- c("All", options)
    
    # Update dropdown menus
    create_dropdown("lengthspecies.campaign", options, " ")
    
  })
  
  # COUNT - FISH NAMES DROPDOWN ----
  output$countspecies.names <- renderUI({
    if (input$countspecies.campaign == "All") {
      
      df<-count.raw()%>%
        left_join(life.history)
      
      options <- df %>%
        dplyr::filter(maxn>0)%>%
        dplyr::distinct(fish.names) %>%
        pull("fish.names")%>%
        sort()
      
      create_dropdown("countspecies.names", options, " ")
      
    } else {
      
      df<-count.raw()%>%
        left_join(life.history)
      
      common.to.keep<-df%>%
        dplyr::filter(maxn>0)%>%
        dplyr::filter(campaignid == input$countspecies.campaign)%>%
        distinct(fish.names)
      
      options <- df %>%
        dplyr::filter(campaignid == input$countspecies.campaign) %>%
        dplyr::semi_join(common.to.keep)%>%
        dplyr::distinct(fish.names) %>%
        pull("fish.names")%>%
        sort()
      
      create_dropdown("countspecies.names", options, " ")
    }
  })
  
  # LENGTH - FISH NAMES DROPDOWN ----
  output$lengthspecies.names <- renderUI({
    if (input$lengthspecies.campaign == "All") {
      
      df<-length.raw()%>%
        left_join(life.history)
      
      options <- df %>%
        dplyr::filter(number>0)%>%
        dplyr::distinct(fish.names) %>%
        pull("fish.names")%>%
        sort()
      
      create_dropdown("lengthspecies.names", options, " ")
      
    } else {
      
      df<-length.raw()%>%
        left_join(life.history)
      
      common.to.keep<-df%>%
        dplyr::filter(number>0)%>%
        dplyr::filter(campaignid == input$lengthspecies.campaign)%>%
        distinct(fish.names)
      
      options <- df %>%
        dplyr::filter(campaignid == input$lengthspecies.campaign) %>%
        dplyr::semi_join(common.to.keep)%>%
        dplyr::distinct(fish.names) %>%
        pull("fish.names")%>%
        sort()
      
      create_dropdown("lengthspecies.names", options, " ")
    }
  })
  

  # COUNT SPECIES - DATAFRAME ----
  countspecies <- reactive({
    countspecies <-count.raw() %>%
      dplyr::left_join(life.history)%>%
      dplyr::filter(fish.names == input$countspecies.names)%>%
      dplyr::select(-c(depth,observer,successful.count,successful.length))
    
  if (input$countspecies.campaign == "All") {
    countspecies
    
  } else {
    
    countspecies %>%
      filter(campaignid == input$countspecies.campaign)
  }
  })
  
  # LENGTH SPECIES - DATAFRAME ----
  lengthspecies <- reactive({
    lengthspecies <-length.raw() %>%
      dplyr::left_join(life.history)%>%
      dplyr::filter(fish.names == input$lengthspecies.names)%>%
      dplyr::select(-c(depth,observer,successful.count,successful.length))
    
    if (input$lengthspecies.campaign == "All") {
      lengthspecies <- lengthspecies
      
    } else {
      
      lengthspecies %>%
        filter(campaignid == input$lengthspecies.campaign)
    }
  })
  
  # COUNT SPECIES - SPATIAL PLOT ----
  output$countspecies.spatial.plot <- renderLeaflet({
    map <- leaflet(countspecies()) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    overzero <- filter(countspecies(), maxn > 0)
    equalzero <- filter(countspecies(), maxn == 0)
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((maxn/max(maxn))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(maxn)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(maxn)
        )
    }
    map
  })
  
  # COUNT SPECIES PLOT - STATUS ----
  output$countspecies.status.plot <- renderPlot({
    
    count.per.sample<-countspecies()%>%
      dplyr::group_by(campaignid,sample,status)%>%
      dplyr::summarise(maxn=sum(maxn))
    
    family.name<-unique(countspecies()$family)
    species.name<-unique(countspecies()$species)
    genus.name<-unique(countspecies()$genus)
    
    scientific.name<-paste(genus.name,species.name,sep=" ")
    common.name<-unique(countspecies()$australian.common.name)
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                  gp=gpar(col="black", fontsize=13, fontface="italic")))
    grob.com <- grobTree(textGrob(as.character(common.name), x=0.01,  y=0.90, hjust=0,
                                  gp=gpar(col="black", fontsize=13)))
    
    if(input$countspecies.theme=="Black and white"){
      scale.theme<-scale_fill_manual(values = c("Fished" = "grey90", "No-take" = "grey40"))
    }else{
      scale.theme<-scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))
    }
    
    ggplot(count.per.sample, aes(x = status,y=maxn, fill = status)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      scale.theme+
      #annotation_custom(fish.img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      xlab("")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
      Theme1+
      ggtitle("Plot of abundance by Status")+
      annotation_custom(grob.sci)+ 
      annotation_custom(grob.com)
  })
  
  # COUNT SPECIES PLOT - LOCATION ----
  output$countspecies.location.plot <- renderPlot({
    
    count.per.sample<-countspecies()%>%
      dplyr::group_by(campaignid,sample,location)%>%
      dplyr::summarise(maxn=sum(maxn))
    
    family.name<-unique(countspecies()$family)
    species.name<-unique(countspecies()$species)
    genus.name<-unique(countspecies()$genus)
    
    scientific.name<-paste(genus.name,species.name,sep=" ")
    common.name<-unique(countspecies()$australian.common.name)
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                  gp=gpar(col="black", fontsize=13, fontface="italic")))
    grob.com <- grobTree(textGrob(as.character(common.name), x=0.01,  y=0.90, hjust=0,
                                  gp=gpar(col="black", fontsize=13)))
    
    ggplot(count.per.sample, aes(x = location, y=maxn)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
      Theme1+
      ggtitle("Plot of abundance by Location")+
      annotation_custom(grob.sci)+ 
      annotation_custom(grob.com)
  })
  
  # COUNT SPECIES PLOT - SITE ----
  output$countspecies.site.plot <- renderPlot({
    
    count.per.sample<-countspecies()%>%
      dplyr::group_by(campaignid,sample,site)%>%
      dplyr::summarise(maxn=sum(maxn))
    
    family.name<-unique(countspecies()$family)
    species.name<-unique(countspecies()$species)
    genus.name<-unique(countspecies()$genus)
    
    scientific.name<-paste(genus.name,species.name,sep=" ")
    common.name<-unique(countspecies()$australian.common.name)
    
    grob.sci <- grobTree(textGrob(as.character(scientific.name), x=0.01,  y=0.97, hjust=0,
                                  gp=gpar(col="black", fontsize=13, fontface="italic")))
    grob.com <- grobTree(textGrob(as.character(common.name), x=0.01,  y=0.90, hjust=0,
                                  gp=gpar(col="black", fontsize=13)))
    
    ggplot(count.per.sample, aes(x = site, y=maxn)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Average abundance per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      scale_y_continuous(expand = expand_scale(mult = c(0, .2)))+
      Theme1+
      ggtitle("Plot of abundance by Site")+
      annotation_custom(grob.sci)+ 
      annotation_custom(grob.com)
  })
  
  
  
  # LENGTH SPECIES - HISTOGRAM ----
  output$lengthspecies.histogram.plot <- renderPlot({
    if(input$lengthspecies.theme=="Black and white"){
      scale.theme<-scale_fill_manual(values = c("Fished" = "grey90", "No-take" = "grey40"))
    }else{
      scale.theme<-scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))
    }
    
    ggplot(lengthspecies(),aes(x = length,  fill=status), col = "black",alpha=0.5)+
      scale.theme+
      geom_histogram(alpha=0.5, position="identity",binwidth=input$length.binwidth,col="black")+
      xlab("Length (mm)") + ylab("Count") +
      theme_bw() +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      Theme1
  })
  
  # LENGTH SPECIES PLOT - STATUS ----
  output$lengthspecies.status.plot <- renderPlot({
    
    if(input$lengthspecies.theme=="Black and white"){
      scale.theme<-scale_fill_manual(values = c("Fished" = "grey90", "No-take" = "grey40"))
    }else{
      scale.theme<-scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))
    }
    
    ggplot(lengthspecies(),aes(x = factor(status), y = length,  fill = status, notch=FALSE, outlier.shape = NA),alpha=0.5) + 
      theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
      stat_boxplot(geom='errorbar')+
      scale.theme+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      xlab("Status") + ylab("Length (mm)") +
      theme_bw() +
      Theme1
  })
  # COUNT METRICS - CAMPAIGN DROPDOWN ----
  output$countmetrics.campaign <- renderUI({
    
    # Use count data
    df <- count.raw()
    
    # Create a list of campaignIDs
    options <- df %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    # Add "All" as an option 
    options <- c("All", options)
    
    create_dropdown("countmetrics.campaign", options, " ")
    
  })
  
  # COUNT METRIC -DATA ----
  countmetrics.data <- reactive({
    req(input$countmetrics.campaign)
    
    count <- count.raw()
    
    if (input$countmetrics.campaign == "All") {
      count <- count
      
    } else {
      campaign.name <- input$countmetrics.campaign
      filter(count, campaignid == campaign.name)
    }
    
    count <- count
    
    total.abundance<-count%>%
      dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
      dplyr::summarise(total.abundance=sum(maxn))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(metric="Total abundance")
    
    species.richness<-count%>%
      dplyr::filter(maxn>0)%>%
      dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
      dplyr::summarise(total.abundance=length(unique(scientific)))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(metric="Species richness")
    
    family.richness<-count%>%
      dplyr::filter(maxn>0)%>%
      dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
      dplyr::summarise(total.abundance=length(unique(family)))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(metric="Family richness")
    
    genus.richness<-count%>%
      dplyr::filter(maxn>0)%>%
      dplyr::group_by(campaignid,sample,status,location,site,latitude,longitude)%>%
      dplyr::summarise(total.abundance=length(unique(genus)))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(metric="Genus richness")
    
    assemblage <- bind_rows(total.abundance, species.richness,family.richness,genus.richness)

    if (input$countmetrics.campaign == "All") {
    assemblage

      } else {
  campaign.name <- input$countmetrics.campaign
  assemblage<-assemblage%>%
    dplyr::filter(campaignid == as.character(campaign.name))%>%
    glimpse()
  }
    unique(assemblage$campaignid)
    assemblage
    })
  
  # COUNT METRIC - SPATIAL PLOT ----
  output$countmetrics.spatial.plot <- renderLeaflet({
    
    data<-countmetrics.data()%>%
      as.data.frame()%>%
      dplyr::filter(metric == input$countmetrics.metric)
    
    map <- leaflet(data) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
    
    overzero <- filter(data, total.abundance > 0)
    equalzero <- filter(data, total.abundance == 0)
    
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((total.abundance/max(total.abundance))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(total.abundance)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(total.abundance)
        )
    }
    map
  })
  
  
  # COUNT METRIC - STATUS ----
  output$countmetrics.status.plot <- renderPlot({
    
    count.per.sample<-countmetrics.data()%>%
      as.data.frame()%>%
      dplyr::filter(metric == input$countmetrics.metric)
    
    posn.d <- position_dodge(0.9)
    
    if(input$countmetrics.theme=="Black and white"){
      scale.theme.metric<-scale_fill_manual(values = c("Fished" = "grey90", "No-take" = "grey40"))
    }else{
      scale.theme.metric<-scale_fill_manual(values = c("Fished" = "grey", "No-take" = "#1470ad"))
    }
    
    metric<-as.character(input$countmetrics.metric)
    
    grob.metric <- grobTree(textGrob(metric, x=0.01,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=13)))
    
    ggplot(count.per.sample, aes(x = status, y=total.abundance, fill=status, group=status)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black",position="dodge") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1,position=posn.d) +
      scale.theme.metric+
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      Theme1+
      ggtitle("Plot of abundance by Status")+
      annotation_custom(grob.metric)
  })
  # COUNT METRIC - LOCATION ----
  output$countmetrics.location.plot <- renderPlot({
    
    count.per.sample <- countmetrics.data()%>%
      as.data.frame()%>%
      dplyr::filter(metric == input$countmetrics.metric)
    
    posn.d <- position_dodge(0.9)
    metric<-as.character(input$countmetrics.metric)
    grob.metric <- grobTree(textGrob(metric, x=0.01,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=13)))
    
    ggplot(count.per.sample, aes(x = location, y=total.abundance, group=location)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black",position="dodge") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1,position=posn.d) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      Theme1+
      ggtitle("Plot of abundance by Location")+
      annotation_custom(grob.metric)
  })
  
  # COUNT METRIC - SITE ----
  output$countmetrics.site.plot <- renderPlot({
    
    count.per.sample <- countmetrics.data()%>%
      as.data.frame()%>%
      dplyr::filter(metric == input$countmetrics.metric)
    
    posn.d <- position_dodge(0.9)
    metric<-as.character(input$countmetrics.metric)
    grob.metric <- grobTree(textGrob(metric, x=0.01,  y=0.97, hjust=0, gp=gpar(col="black", fontsize=13)))
    
    ggplot(count.per.sample, aes(x = site, y=total.abundance, group=site)) + 
      stat_summary(fun.y=mean, geom="bar",colour="black",position="dodge") +
      stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1,position=posn.d) +
      geom_hline(aes(yintercept=0))+
      xlab("")+
      ylab("Per stereo-BRUV \n(+/- SE)")+
      theme_bw()+
      scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
      Theme1+
      ggtitle("Plot of abundance by Site")+
      annotation_custom(grob.metric)
  })
  
}


