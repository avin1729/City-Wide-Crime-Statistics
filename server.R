 #   # ##### #   #     #     # #  ####  ####     #   ##### #  #     #  #####
# #  #   #    # #      #     # #  #  #  #       # #  #   # #  # # # #  #
#    #   #     #       #  #  # #  #  #  ####    #    ##### #  #  #  #  #####
# #  #   #     #       # # # # #  #  #  #       # #  # #   #  #     #  #
 #   #   #     #       #     # #  ####  ####     #   #   # #  #     #  #####

#City Wide Crime Statistics#####################################################


#Loading Required data sets#####################################################

Crime_in_NY_by_Year <- read_excel("Source Data/Crime in NY by Year.xlsx") #historic crime of Ny and LA

Crime_details_by_location_and_demographics <- read_csv("Source Data/Crime_details_by_location_and_demographics.csv", 
                                            col_types = cols(CMPLNT_FR_DT = col_date(format = "%m/%d/%Y"))) %>% head(1300)  # Crime Data for map and demographics

Crime_in_LA_by_Year <- read_excel("Source Data/Crime in LA by Year.xlsx")  #historic crime of  LA 
Los_angeles_Crime_data <- read_csv("Source Data/Los angeles Crime data.csv") #lA Crime data
LA_Polupation_By_Year <- read_excel("Source Data/LA Polupation By Year.xlsx") #LA Polupation by Year
LA_Sex_Ratio <- read_excel("Source Data/LA Sex Ratio.xlsx") # La Sex ratio data
LA_Education <- read_csv("Source Data/LA_Education.csv") # LA Education for the current Year
NY_Population_by_year <- read_excel("Source Data/NY Population by year.xlsx")  #NY Population By Year
NY_Sex_ratio <- read_excel("Source Data/NY Sex ratio.xlsx") # NY Sex ratio
NY_education <- read_csv("Source Data/NY_education.csv")  # NA Education data
NY_POP_BY_RACE <- read_excel("Source Data/NY_POP_BY_RACE.xlsx") # Na populaion by race
LA_population_by_race <- read_excel("Source Data/LA_population_by_race.xlsx") # La Population by Race






# Define server logic required -------------------------------------------------------------------------------------------------------------------------------------------------
shinyServer(function(input,output,session){
   
  #Defining Action to close the popup and to get the User name #########################################################################################################################################
  output$value <- renderText({ input$uname })
  
  observeEvent(input$letsgo, {
    
    toggleModal(session, "window", toggle = "close")
  })
  
  # Filtering data to select city by user input ###################################################################################################################################################
  
  filtered_data<- reactive({
      dplyr::filter(Crime_in_NY_by_Year, Crime_in_NY_by_Year$City ==input$selectcity)
    
  })
  
  
  # Creating trend graph and faceting by type of crime ######################################################################################################################################################
  output$historycrime<-renderPlot({
    
    filtered_data() %>%
      ggplot() +
      
      geom_line(aes(x=Year, y=Crimes, color=Type))+                            #adding line chart
      geom_smooth(aes(x=Year, y=Crimes, color=Type),se=FALSE)+                 #Adding Smooth
      scale_y_continuous(name="Crimes", labels = comma)+                       #Number Formating the Y Axis
      facet_wrap(~Type,scales = "free")+
      theme(panel.background = element_rect(fill = "black", colour = "black"), #Setting Theme and background color to pane and grids and text size
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "black"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "black"),
            strip.text.x = element_text(size=9,face="bold",colour="white" ),
            strip.background = element_rect(colour="#035c66", fill="#008e9e"))+
      labs(y="Total Crimes",x=" Year of Crimes",
           title= paste0("Total Crime in ",input$selectcity, " over Year"))+
      theme(text = element_text(size = 16), element_line(size = 0.9))+
      theme(legend.position="none")                                           # Hiding legend
  })
  

# Filtering data by LA for Map chart ####################################################################################################################################################################
    Crime_details_by_location_and_demographicsla <-Crime_details_by_location_and_demographics %>% 
      filter(City=="LA")
# Creating Map Chart for LA #############################################################################################################################################################################
    output$map1 <- renderLeaflet({
      
      leaflet(Crime_details_by_location_and_demographicsla) %>% addTiles() %>% addMarkers( 
        popup = ~paste0("Offecnce:",as.character(OFNS_DESC),"<br/> Date of Crime: ", as.character(CMPLNT_FR_DT), "<br/>Location: ", as.character(BORO_NM), "<br/> Gender: ", 
                        as.character(Gender),"<br/> Education: ", as.character(Education) ), 
        label = ~as.character(BORO_NM) , 
        clusterOptions = markerClusterOptions()) %>% 
        
        addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%         # Adding dark,light,OSm layers for map
        addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
        addLayersControl(baseGroups=c('OSM','Dark','Light')) %>% 
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Locate Me",                            #Adding GPS
          onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
        addEasyButton(easyButton(
          icon = "fa-globe", title = "Zoom to Level 9",                          # Adding Auto zoon feature and zooiming to level 9 by default
          onClick = JS("function(btn, map){ map.setZoom(9);}")))
      
  
      
    })
  # Filtering data by NY for Map Chart ############################################################################################################################################################ 
    
    Crime_details_by_location_and_demographicsny <-Crime_details_by_location_and_demographics %>% 
      filter(City=="NY")
   # Creating  MAp Chart for NY ##################################################################################################################################################################
    output$map2 <- renderLeaflet({
      
      leaflet(Crime_details_by_location_and_demographicsny) %>% addTiles() %>% addMarkers( 
        popup = ~paste0("Offecnce:",as.character(OFNS_DESC),"<br/> Date of Crime: ", as.character(CMPLNT_FR_DT), "<br/>Location: ", as.character(BORO_NM), "<br/> Gender: ", 
                        as.character(Gender),"<br/> Education: ", as.character(Education) ), 
        label = ~as.character(BORO_NM) , 
        clusterOptions = markerClusterOptions()) %>% 
        
        addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%         # Adding dark,light,OSm layers for map
        addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
        addLayersControl(baseGroups=c('OSM','Dark','Light')) %>% 
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Locate Me",                           # Adding GPS
          onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
        addEasyButton(easyButton(
          icon = "fa-globe", title = "Zoom to Level 9",                          # Adding Auto zoon feature and zooiming to level 9 by default
          onClick = JS("function(btn, map){ map.setZoom(9);}")))
      
      
      
    })

    
    # Defining the reactive function to filter city by user selection ################################################################################################################################################
    filtered_data2<- reactive({
      
      dplyr::filter(Los_angeles_Crime_data, Los_angeles_Crime_data$City == input$city)
       
      
      
    })
    
    
    #Creating a bar chart output to show crime in LA and Ny to the US Average ####################################################################################################################################
    output$barchart<-renderPlot({
      
      filtered_data2() %>%
        # Reorder data
        ggplot( aes(x=Statistic, y= `Los Angeles/100k people`,fill=City , color=City)) +
       
        geom_bar(stat = "identity")+                                             # Creating Bar chart
        geom_point()+
        geom_errorbar( aes(x=Statistic, ymin=`National/100k people`, ymax=`National/100k people`), 
                       width=0.4, colour="red", alpha=0.9, size=1.3)+            # Creating Error BAr
        coord_flip()+                                
        scale_fill_manual(values=c("#05739c", "#70f5ff"))+                       # Filling Manual Colors
        scale_colour_manual(values=c("#05739c", "#70f5ff"))+
        theme_classic()+                                                         # adding classic theme
        labs(title=paste0("Type of Crime for ",input$city),                      # Adding title and labels
             y="Total Crime",
             x="Type of Crime")+ 
        theme(text = element_text(size = 16), element_line(size = 0.9))+
        theme(plot.background = element_rect(fill = "#ffffff"),
              panel.background = element_rect(fill = "#ffffff", colour = "#000000"))+ 
        theme(legend.position="none")                                            # Removing all legends

 
      
    })
    
    

# filtering the data based on the user input in City wide crime tab##########################################################################################################################################################################
    
    filtered_data1<- reactive({
      
      dplyr::filter(Crime_details_by_location_and_demographics, Crime_details_by_location_and_demographics$Age>input$myKnob1 & Crime_details_by_location_and_demographics$Age<input$myKnob & 
                      Crime_details_by_location_and_demographics$City=="NY" & Crime_details_by_location_and_demographics$Gender== input$gender & Crime_details_by_location_and_demographics$Education == input$proffession)%>% 
        summarise(Total_Crime=n())
      
      
    })
    
    # Filtering and calculating data for gauge chart ######################################################################################################################################
    filtered_dataguage<- reactive({
      
      dplyr::filter(Crime_details_by_location_and_demographics, Crime_details_by_location_and_demographics$Age>input$myKnob1 & Crime_details_by_location_and_demographics$Age<input$myKnob & Crime_details_by_location_and_demographics$City==input$selectcityforage
                    & Crime_details_by_location_and_demographics$Gender== input$gender & Crime_details_by_location_and_demographics$Education == input$proffession)%>% 
        summarise(Total_Crime=n()) %>% 
        summarise(Total_Crime=Total_Crime*400) 
      
      
    })
    
   
# Rendering Text to show as label in guage chart#######################################################################################################################################
output$guage = renderText({
  
  if (input$selectcityforage=="NY"){
    "Los Angeles"
  }
  else{
    "New York"
  }

    }
  
 
)
 # creating a guage to show crime in LA and NY  ###################################################################################################################################### 
    output$gauge235 = renderGauge({
      gauge(filtered_dataguage()$Total_Crime, 
            min = 0, 
            max = 80000,                                                         # Defining Range
            sectors = gaugeSectors(success = c(0.5, 1),                          # defining color range
                                   warning = c(0.3, 0.5),
                                   danger = c(0, 0.3)),
            symbol = '',
            label=paste0("Us Average : 67000"))                                  # US Average Precalculated based on anaysis
    })
    
    
# Selecting only required field for Wordcloud ################################################################################################################################################
    
    x<-Crime_details_by_location_and_demographics %>% 
      select(OFNS_DESC,Age) %>% 
      head(1300)
  
#Creadting Word Cloud  ######################################################################################################################################################################
    output$wordd <- renderWordcloud2({
      
      wordcloud2(data=x, size=1.6,shape = 'circle')
    })
   

  
    
    # Filtering data based on user selection for citywide crime tab ####################################################################################################### #####################
    filtered_data4<- reactive({
      
      dplyr::filter(Crime_details_by_location_and_demographics, Crime_details_by_location_and_demographics$Age>input$myKnob1 & Crime_details_by_location_and_demographics$Age<input$myKnob
                    & Crime_details_by_location_and_demographics$Gender== input$gender & Crime_details_by_location_and_demographics$Education == input$proffession
                    )
       
      
      
    })
    
    #Creating Box Plot ###############################################################################################################################################
    output$boxplotage<-renderPlotly({
     
      
     agebox<- filtered_data4() %>% 
        filter(!is.na(Crimes)) %>%                                               # Filtering all NA Values if Present
        ggplot(aes(x = Crimes, y = Age, fill = Gender)) +   # Fill column
        geom_boxplot(outlier.shape=16,notch=FALSE) +                             # Creating Box Plot
       scale_fill_manual(values=c("#b55ba4", "#03a0ad"))+                        # Fixing maual Color
       labs(title="US Crime by Age and Gender") +
        # Centre plot title
        
       theme(                                                                    # Setting Theme
             panel.grid.major.x = element_blank()
       )+theme(text = element_text(size = 13), element_line(size = 0.6))+
       theme_minimal()
    ggplotly(agebox,  height = 230)                                            # Adjusting Height
      
    })
  
    

    # Creating Modal on Tab Click to show details about the tab City Wide Crime Statistics ######################################################################################################
      
    observeEvent(input$sidebarmenu, {
      req(input$sidebarmenu == "map")
      showModal(
        modalDialog(title = "", HTML(paste0("<b><font size=4 color='007782' style='font-family:cursive;'><b>City wide Crime Statistics</b></font>
        <font size=3 color='007782' style='font-family:Courier;'>
        <BR><BR>This analysis is to comapre average crimes in New York and Los Angeles to the average crime in US.
        This also do a analysis between crime and demographics for both the cities.<BR><bR>
        This analysis consit of 3 tabs:<BR><br>
        1.<b>Locate Crime on Map: </b>Helps you to locate crime on map and analyse its type.<BR>
        2.<b>City Wide Crime Analysis: </b>Compares crime between Los Angeles and New York and 
        compares with average US Crime and also shows the effect of the crime with demographics.<BR>
        3.<b>Crime in 2k19: </b>Heps you to analyse the crimes in 2019.</font><BR></b><BR>
                                                                      ")))
      )
    })
    
    
    # Creating Modal on Tab Click to show details about the tab Demographics ####################################################################################################################
    observeEvent(input$sidebarmenu, {
      req(input$sidebarmenu == "demographics")
      showModal(
        modalDialog(title = "", HTML(paste0("<b><font size=4 color='007782' style='font-family:cursive;'><b>Demographic Analysis</b></font>
        <font size=3 color='007782' style='font-family:Courier;'>
        <BR><BR>This analysis is to show the demographics details for New York and Los Angeles and its changes over years.<BR><br>
        This analysis consit of 2 tabs:<BR><br>
        1.<b>Los Angeles: </b>Showing the demographics of Los Angeles. <BR>
        2.<b>New York: </b>Showing the demographics of New York.<BR>
        </font><BR></b>
                                                                      ")))
      )
    })
    
   # Defining Reactive function to filter data based on user selection ########################################################################################################
    
    filtered_datayear1<- reactive({
      
      dplyr::filter(Crime_details_by_location_and_demographics,as.Date(Crime_details_by_location_and_demographics$CMPLNT_FR_DT)>=as.Date(input$daterange4[1]) &
                      as.Date(Crime_details_by_location_and_demographics$CMPLNT_FR_DT)<=as.Date(input$daterange4[2]) &
                      Crime_details_by_location_and_demographics$City ==input$selectcity1 & Crime_details_by_location_and_demographics$Age >= input$Age[1] 
                    & Crime_details_by_location_and_demographics$Age <= input$Age[2] & Crime_details_by_location_and_demographics$Crimes == input$myPicker)
      
    })  

    
    # Create Violin Chrat to show AGE VS Education VS Gender ###############################################################################################################
    
    output$violin <- renderPlotly({
      
      ggplotly(
        filtered_datayear1() %>% 
          filter(!is.na(Crimes)) %>% 
          ggplot(aes(x = Education, y = Age,fill=Gender, color = Gender,label=Age)) +
          geom_violin()+                                                         # Ploting Graph
          scale_fill_manual(values=c("#b55ba4", "#03a0ad"))+                     # Setting Colors Manual
          scale_color_manual(values=c("#b55ba4", "#03a0ad"))+
          labs(title = "Crime Over Year",x="Education")+                         # Adding Title 
          theme(                                                                 # Setting Theme for pane and Grids
            panel.grid.major.x = element_blank(),
            text = element_text(size = 13), element_line(size = 0.6),
            axis.title.x = element_text(colour = "white"),
            axis.title.y = element_text(colour = "white"),
            axis.title = element_text(colour = "white") )+
          theme_minimal(),tooltip = c("y", "x", "colour")                        # Setting theme and Defining Tooltip
        
      )
      
    })
  
    
    # Mutating to find Month and arrange by Month ###################################################################################################################
    Crime_details_by_location_and_demographics1<-Crime_details_by_location_and_demographics %>% 
      mutate(`Month of Crime`=months(as.Date(CMPLNT_FR_DT)), monthnumber=month(as.POSIXlt(CMPLNT_FR_DT, format="%d/%m/%Y"))) %>% 
      arrange(monthnumber)
    # Summarizing to count the crime in each month####################################################################################################################
    Crime_details_by_location_and_demographics2<-Crime_details_by_location_and_demographics1 %>% 
      group_by(`Month of Crime`,Gender) %>% 
      summarise(`Total Crime`=n()) 
    

  
    # Creating a line Chart to show Crime by Month #############################################################################################################################
    output$lineyear<-renderPlotly({
      
      ggplotly(
        ggplot(data=Crime_details_by_location_and_demographics2, 
               aes(x=`Month of Crime`,y=`Total Crime`, group=Gender,color=Gender)) +
          geom_line(linetype="dashed", aes(color=Gender), size=1.2)+             # Adding Line Chart
          geom_point(aes(color=Gender), size=3)+                                 # Adding Points on Graphn
          coord_flip()+                                                          # Fliping the Coordinated
          geom_errorbar(aes(ymin=`Total Crime`-20, ymax=`Total Crime`+20), width=.1, 
                        position=position_dodge(0.05))+                          # Adding Errror Bar
          
          scale_color_manual(values=c("#b55ba4", "#03a0ad"))+                    # Fixing manual Color
          theme_bw()+ 
          labs(x="Month",y="Total Crimes",title = "Total Crimes over year")+
          theme(legend.position="top"),tooltip = c("y","x"))                     # Positioning the legend and Designing ToolTip
      
      
    })
    
    
    # Creating Line Chart To Sho population Over Year #######################################################################################################################
    output$popoveryear<-renderPlotly({
      
      
      ggplotly(
        ggplot(data=LA_Polupation_By_Year, aes(x=factor(LA_Polupation_By_Year$`Year `), y=Population,label= `Year `,group=1)) +
          geom_smooth(se=FALSE,color="#047573",size=2)+                          # Adding Geom-Smooth
          geom_point(color="#00c4c1",size=2)+                                    # Adding points on Plot
          labs(x="Year",y="Population",
               title="Total Population in Los Angeles Over Year")+               # Adding Title
          theme_minimal(), height = 280,tooltip = c("label","y"))                # Setting Theme and designing Tooltip
        
        
    }) 
    
    
    # Creating Line Chart To Sho population Over Year for NY ###############################################################################################################
    output$popoveryearny<-renderPlotly({
      
      
      ggplotly(
        ggplot(data=NY_Population_by_year, aes(x=factor(NY_Population_by_year$`Year `), y=Pop,label=`Year `, group=1)) +
          geom_smooth(se=FALSE,color="#047573",size=2)+                          # Added Smooth Curve
          geom_point(color="#00c4c1",size=2)+                                    # Adding points on Plot
          labs(x="Year",y="Population",title="Total Population in New York Over Year")+
          theme_minimal(), height = 280,tooltip = c("y","label")                # setting theme and designing popup
      )
      
      
      
    }) 
    
    
    
    # Reading Image to show Sex Ration in LA #############################################################################################################################
    output$image2 <- renderImage({
      
      list(
        src = "sexratiola.PNG",                                                 # Reading Png File
        filetype = "png",
        alt = "Los Angeles Median Age"
      )
    }, deleteFile = FALSE)                                                      
    
    
    
    # Reading Image to show Sex Ration in NY#############################################################################################################################
    output$image2ny <- renderImage({
      
      list(
        src = "sexrationy.PNG",
        filetype = "image/png",
        alt = "New York Median Age"
      )
    }, deleteFile = FALSE)                                                      # making delete to false to avoid delete
    
    
    
    #Creating Pyramid Chart for sex ratio in LA ####################################################################################################################
    output$sexratiola<-renderPlotly({
      
      
      ggplotly(
        ggplot(LA_Sex_Ratio, aes(x = factor(Age),
                                 y = Population,label=Age, fill = Gender)) +     # Defining the parameters
          geom_bar( stat = "identity") +                                         # Creating bar Chart
          scale_y_continuous(breaks = seq(-150000, 150000, 50000), 
                             labels = paste0(as.character(c(seq(15, 0, -5), 
                                                            seq(5, 15, 5))), "m")) + 
          coord_flip() +                                                        # Fliping the coordinated
          scale_fill_brewer(palette = "Set1") +                                 # Filling the bars
          labs(x="Age",y="Total Population",
               title = "Los Angeles Population Pyramid 2020")+                  # Adding labels and Title
          theme_bw(),width=560, tooltip = c("y","fill","label"))                # Theme and designing tooltip             
       
      
      
      
    }) 
    
    
    #Creating Pyramid Chart for sex ratio in LA#############################################################################################################################################################
    output$sexrationy<-renderPlotly({
      
      
      ggplotly(
        ggplot(NY_Sex_ratio, aes(x = factor(Age),
                                 y = Population,label= Age, fill = Gender)) +    # Defining the parameters
          geom_bar( stat = "identity") +                                         #Creating bar chart
          scale_y_continuous(breaks = seq(-1500000, 1500000, 500000), 
                             labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
          coord_flip() +                                                         #Fliping the coordinates
          scale_fill_brewer(palette = "Set1") +                                  #filling the bar
          labs(x="Age",y="Total Population",
               title = "New York Population Pyramid 2020")+                      #Adding label and title
          theme_bw(),width=560,tooltip = c("y","fill","label")  )
      
      
      
      
    }) 
    
    
    #Donut Cart to show Education ###########################################################################################################################################
    output$donuteducationla<-renderPlotly({
      
      
      fig <- LA_Education %>% plot_ly(labels = ~title, values = ~percent)       # Reading parameters
      fig <- fig %>% add_pie(hole = 0.6)                                        # Creating Donut
      fig <- fig %>% layout(title = "Education Percentage in Los Angeles in 2020",  showlegend = T,
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),width=560,height=350)  #Setting Height , Width , Title
      fig <- fig %>% layout(legend = list(x = 1.2, y = -1.2))                    # Legend Position 
      fig                                                                        # diplaying the plot
      
      
      
    }) 
    
    
    # donut chart to show education for NA ###################################################################################################################################
    output$donuteducationny<-renderPlotly({
      
      
      fig1 <- NY_education %>% plot_ly(labels = ~title, values = ~percent)       # calling Parameters
      fig1<- fig1 %>% add_pie(hole = 0.6)                                        # Creating Donut
      fig1  <- fig1 %>% layout(title = "Education Percentage in New York in 2020",  showlegend = T,
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),width=560,height=350)  #Setting Height , Width , Title
      fig1 <- fig1 %>% layout(legend = list(x = 1.2, y = -1.2))                  # Legend Position           
      fig1                                                                       # diplaying the plot 
      
      
      
    }) 
    
    # donut chart to show Race for NA########################################################################################################################################### 
    output$donutraceny<-renderPlotly({
      
      fig2 <- NY_POP_BY_RACE %>% plot_ly(labels = ~Race, values = ~Percentage)   # Calling Parameters
      fig2<- fig2 %>% add_pie(hole = 0.6)                                        # Create Donut Chart
      fig2  <- fig2 %>% layout(title = "Population By Race in New York in 2020",  showlegend = T,
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),width=560,height=350) #Setting Height , Width , Title
      fig2 <- fig2 %>% layout(legend = list(x = 1.9, y = -1.2))                 #Positioning Legend
      
      fig2                                                                      # Displaying Plot 
      
      
    }) 
    
    # Donut to Show Race for LA #############################################################################################################################################
    output$donutracela<-renderPlotly({

      fig3 <- LA_population_by_race %>% plot_ly(labels = ~Race, values = ~Percentage) # Setting Parameters
      fig3<- fig3 %>% add_pie(hole = 0.6)                                             # Creating Donut 
      fig3  <- fig3 %>% layout(title = "Population By Race in Los Angeles in 2020",  showlegend = T,
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),width=560,height=350) #Setting Height , Width , Title
      fig3 <- fig3 %>% layout(legend = list(x = 1.9, y = -1.2))                #Positoning Legend        
      fig3                                                                       # Plotting Donut
      
      
    }) 
 
    
})

