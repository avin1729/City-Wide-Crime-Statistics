 #   # ##### #   #     #     # #  ####  ####     #   ##### #  #     #  #####
# #  #   #    # #      #     # #  #  #  #       # #  #   # #  # # # #  #
#    #   #     #       #  #  # #  #  #  ####    #    ##### #  #  #  #  #####
# #  #   #     #       # # # # #  #  #  #       # #  # #   #  #     #  #
 #   #   #     #       #     # #  ####  ####     #   #   # #  #     #  #####

#City Wide Crime Statistics

#Loading all Recuired pakages
library(shiny) # For Shiny Dashboard
library(shinydashboard) # Shiny Dashboard
library(shinycustomloader) #Interactive loaders
library(shinyBS) # for modal 
library(shinyjs) # easy javascript functionalities with shiny
library(shinyWidgets) # for Widgets
library(leaflet)   # To Create Map Chart
library(flexdashboard) # To show Gauge
library(wordcloud2) # To show word cloud
library(plotly)  # Create Visualization
library(ggplot2)  # Create Visualization
library(readr) # Read data file
library(gapminder) #gapminder data frame or "tibble"
library(dplyr) #data manipulation
library(data.table) #aggregation of large datasets
library(png)
library(readxl)  # Read data file
library(scales) # Formating Scales

# Loading popup on Load and setting ESC and Click as True
bsModalNoClose <-function(...) {
b = bsModal(...)
b[[2]]$`data-backdrop` = "TRUE"
b[[2]]$`data-keyboard` = "TRUE"

return(b)
}




# Define UI for application that draws a histogram
shinyUI(
dashboardPage(

dashboardHeader( title="City wide Crime Statistics",titleWidth = 280,

# To display the user name to top
tags$li(
a(

strong(textOutput("value")),
height = 30,
href = "",
title = "",
target = "_blank"
),class = "dropdown")),
dashboardSidebar(width = 60,

# Defining the tabs
sidebarMenu(id = "sidebarmenu",
br(),br(),

menuItem("",tabName = "Home",icon = icon("home", "fa-2x"), badgeColor = "green"),
menuItem("",tabName = "map",icon = icon("binoculars", "fa-2x"), badgeColor = "green"),
menuSubItem("",tabName = "demographics",icon = icon("users", "fa-2x"))
# Adding links to side bar

)),
# Defining the POPUP	 

dashboardBody(


bsModal("window", "Window",


title="",size='Large',
HTML(paste0( 
"<font size='6' color='#007e8c' ><b>City wide Crime Statistics</b></font>",
"<center><IMG SRC='https://media3.giphy.com/media/vLUcTXDJT7egg/giphy.webp?cid=790b761179de94ea3b0425936cac7a67acb609e13934fa38&rid=giphy.webp',
style='width:200px;height:200px;'></center>",
"<p align='justify'  style='line-height:140%, margin-left: auto; margin-right: auto;padding-left: 40px;padding-right: 40px;'>
<font size='3' color='#038099'><b>The crime rate is ever-growing and so is technology. I thought of combining them to produce meaningful insights. 
Though, there is a plethora of work on nation-wide crime analysis, only few on city-wise record comparison. So, I found few data-sets from numerous 
open sources for this study.
The objective of this analysis is to compare Newyork crime stats with that of LA's and find highest crime rate for different demographics 
like age, gender, education etc. Without further ado, let’s get started!
</b></font></p>"

)),
# textInput('username', 'Enter Your Name'),

HTML(paste0( 

"<BR>",textInput("uname", "What should I call you", value="" , width = "100%"  ,placeholder = 'Enter Your Name'))),
tags$head(
tags$style(HTML('#letsgo{background-image: linear-gradient(#2c7a91, #00222b);}.btn1{color: white;} .btn1{font-family: Courier New}
.btn{color: white;} .btn{font-family: Courier New}.btn{background-image: linear-gradient(#2c7a91, #00222b);}'))
),
actionButton('letsgo', 'Lets Go', class = 'btn1', icon = icon('sign-in-alt'),width = '100%', color='#32a883'),


tags$head(tags$style("#window .modal-footer{display:none}
.modal-header .close{display:none}"),
tags$script("$(document).ready(function(){
$('#window').modal();

});"))),


# Designing the dashboard layout
tags$head(tags$style(
HTML('


body, label, input, button, select { 
font-family: "Arial";
}'))),

tags$head(tags$style(HTML('
/* logo */
.skin-blue .main-header .logo {


background-image: linear-gradient(to bottom right,#010812, #5e0202);
}

/* logo when hovered */
.skin-blue .main-header .logo:hover {
background-color: #024659;
}

/* navbar (rest of the header) */
.skin-blue .main-header .navbar {
background-image: linear-gradient(#056675, #00222b);

}

/* main sidebar */
.skin-blue .main-sidebar {

/* For browsers that do not support gradients */
background-image: linear-gradient(to right,#010812, #5e0202); /
}                                

/* other links in the sidebarmenu when hovered */
.skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
background-color: #0579b3;
}
/* toggle button when hovered  */
.skin-blue .main-header .navbar .sidebar-toggle:hover{
background-color: #0579b3;
}
.content-wrapper, .right-side {
background-image: url("https://cdn.pixabay.com/photo/2014/10/13/08/38/walkers-486583__340.jpg");,
background-position: center; /* Center the image */
background-repeat: no-repeat; /* Do not repeat the image */
background-size:100% 100%; /*


}

.html-widget.gauge234 svg {
height: 400px;
width: 800px;
}



'))),
shinyjs::useShinyjs(),

tabItems(

tabItem(tabName = "Home",

fluidPage(
# Home Page			

#Adding Introduction 




HTML(paste0('<table>

<tr>
<td><img src="https://media3.giphy.com/media/Ujhmcc0awrB6g/giphy.webp?cid=790b761186ad388cbadb8f5fdef95f77de01c0047d0b6e09&rid=giphy.webp" alt="Crime Analysis" style="width:200px;height:200px;"></td>
<td><p align="justify"><font color="black" size=3 style="font-family:Courier;">
<BR><b>
<font color="#007e8c" size=5 style="font-family:Courier;"><b> United State CRIME HISTORY</b></font><br>
U.S show a substantial decline in the violent crime rate since it peaked in the early 1990s. One is an annual report by the FBI of serious crimes reported to police in more than 18,500 jurisdictions around the country.
Using the FBI numbers, the violent crime rate fell 51% between 1993 and 2018. Using the BJS data, the rate fell 71% during that span. The long-term decline in violent crime hasn’t been uninterrupted, though.
The FBI, for instance, reported increases in the violent crime rate between 2004 and 2006 and again between 2014 and 2016. Violent crime includes offenses such as rape, robbery and assault.
Like the violent crime rate, the U.S. property crime rate today is far below its peak level. FBI data shows that the rate fell by 54% between 1993 and 2018, while BJS reports a decline of 69% during that span. Property crime includes offenses such as burglary, theft and motor vehicle theft, 
and it is generally far more common than violent crime.
</b></font></p></td>
</table>
<BR><BR>
')),

# Adding Filters

tags$head(tags$style(HTML(".small-box {height: 150px}"))),

box(width=8,background = "black", height = 550, collapsible=TRUE,solidHeader = TRUE,
tabItem(tabName = "Historic Crime",
tags$style(HTML("

.tabbable > .nav > li > a[data-value='Type of Crime'] {background-image: linear-gradient(#056675, #088a9e);   color:white}
.tabbable > .nav > li > a[data-value='Crime Analysis by Age'] {background-image: linear-gradient(#056675, #088a9e);color:white}
.tabbable > .nav > li > a[data-value='Crime Over Year'] {background-image: linear-gradient(#056675, #088a9e);color:white}
.tabbable > .nav > li > a[data-value='Most Crime'] {background-image: linear-gradient(#056675, #088a9e);color:white}
.tabbable > .nav > li > a[data-value='Historic Crime Analysis'] {background-image: linear-gradient(#056675, #088a9e);color:white}
.tabbable > .nav > li > a[data-value='Locate Crime on Map'] {background-image: linear-gradient(to bottom right,#010812, #a31c1c);color:white}
.tabbable > .nav > li > a[data-value='City Wide Crime Analysis'] {background-image: linear-gradient(to bottom right,#010812, #a31c1c);color:white}
.tabbable > .nav > li > a[data-value='Crimes In 2k19'] {background-image: linear-gradient(to bottom right,#010812, #a31c1c);color:white}
.tabbable > .nav > li > a[data-value='Los Angeles'] {background-image: linear-gradient(to bottom right,#010812, #a31c1c);color:white}
.tabbable > .nav > li > a[data-value='New York'] {background-image: linear-gradient(to bottom right,#010812, #a31c1c);color:white}
.tabbable > .nav > li > a[data-value='Crime By Month'] {background-image: linear-gradient(#056675, #088a9e);color:white}
.tabbable > .nav > li > a[data-value='Education'] {background-image: linear-gradient(#056675, #088a9e);color:white}
.tabbable > .nav > li > a[data-value='Population By Race'] {background-image: linear-gradient(#056675, #088a9e);color:white}

")),


tabsetPanel(type = "tabs", position = "right",
tabPanel("Historic Crime Analysis",icon = icon("history"),

radioGroupButtons(
inputId = "selectcity", label = "", 
choices = c("New York"="NY", "Los Angeles"="LA"), 
justified = TRUE, status = "primary",
checkIcon = list(yes = icon("ok", lib = "glyphicon"))
),
tags$script("$(\"input:radio[name='selectcity'][value='NY']\").parent().css('background-color', '#023752');"),
tags$script("$(\"input:radio[name='selectcity'][value='LA']\").parent().css('background-color', '#023752');"),

#Adding Crime trend over year
withLoader(plotOutput("historycrime"),type="image" , loader = 'https://media.giphy.com/media/uhXYeiV43VmDe/giphy.gif')),
tabPanel("",icon = icon("info-circle"),
HTML(paste0('<BR><font color="white" style="font-family:Courier;">
<b> Crime Analyis in Los Angeles and New York Over Year</b>
<br><br>
Please select the filter on the top to toggle between New York and Los Angeles.
<br>
The Report is calculated per 100,000 cases.This chart is used to show the trend in crime over year, added a smooth to make the analysis easier.
Crime analysis in NY/LA over year shows the crime in Los Angeles and New York from 2000 to 2018 with the trend for each major type of crime in both cities.
Reported crimes in Los Angeles is more than the crimes in New York and which is even more than the USA average crime.
Crimes over both cities is decreasing over time except rapes. Number of rapes in both cities is increasing over time.

<br>
For the first time in five years, violent crime was down in Los Angeles in 2018, with the number of homicides on track to be among the lowest in more than 50 years.
<br>
After decades of steady decline, violent crime  which includes homicide, rape, robbery and aggravated assault  had begun climbing in 2014, with a spike in shootings in South L.A. causing particular concern.
</font>')),


))),








),
#Adding Details about US Crime 
box(width = 4,height = 550,background = "black",
HTML(paste0('<BR><p align="justify"><font color="white" size=3 style="font-family:Courier;">Crime in the United States has been recorded since colonization. 
Crime rates have varied over time, with a sharp rise after 1963, 
reaching a broad bulging peak between the 1970s and early 1990s. 
Since then, crime has declined significantly and current crime rates are approximately the same as those of the 1960s.
<BR>

The manner in which Americas crime rate compared to other countries of similar wealth and development depends on the nature
of the crime used in the comparison.
Overall crime statistic comparisons are difficult to conduct, as the definition and categorization of crimes varies across countries.
Thus an agency in a foreign country may include crimes in its annual reports which the United States omits, and vice versa. 
</font></p>'))),






)),

# Adding New Tab to show Crime analytics
tabItem(tabName = "map",
tabsetPanel(type = "pills",
tabPanel("Locate Crime on Map",icon = icon("globe", "fa-1x"), br(),
tags$style(type = "text/css", "html, body {width:100%;height:180%}"),
fluidPage(tags$style(type = "text/css", "#map1 {height: calc(120vh - 100px) !important;}","#map2 {height: calc(120vh - 100px) !important;}"),

# Show Map Chart							   
box(background = "black" ,HTML(paste0('<div style="background-color:black ">
<font color="white" size=4 style="font-family:Courier;"> Los Angeles</font><BR>
<font  color="white"style="font-family:Courier;">  * Click on the markers to drill through the map</font>
</div>')),withLoader(leafletOutput("map1"),type="html" , loader = 'loader1')),
box(background = "black" ,HTML(paste0('<div style="background-color:black ">
<font color="white" size=4 style="font-family:Courier;"> New York</font><BR>
<font  color="white"style="font-family:Courier;">  * Click on the markers to drill through the map</font>
</div>')),withLoader(leafletOutput("map2"),type="html" , loader = 'loader1'))  )),
# Add tab: City Wide Crime Analysis										 
tabPanel("City Wide Crime Analysis", icon = icon("chart-pie", "fa-1x"),br(),
# Add filter Panel										 
box(background = "light-blue" ,width=2,
HTML(paste0("<font color='white'> Toggle between filters</font>")),
height = 750,
tabItem(tabName = "filter",
tabsetPanel(type = "pills",
tabPanel("",icon = icon("filter"),
        
        HTML(paste0("<font color='white' style='font-family:Courier;'>* Min Age of crime is 4 and max age in which crime 
is committed is 99. Select the age filter
</font><BR><BR>")),
        #Add Age Filter as Knob 														 
        
        knobInput(
          inputId = "myKnob1",
          label = "Select min age of crime:",
          width = 130,
          value = 10,
          min = 4,
          max=99,
          displayPrevious = TRUE, 
          lineCap = "round",
          fgColor = "#ff1414",
          inputColor = "#fffff"),
        knobInput(
          inputId = "myKnob",
          label = "Select max age of crime:",
          width = 130,
          value = 35,
          min = 4,
          max=99,
          displayPrevious = TRUE, 
          lineCap = "round",
          fgColor = "#ff1414",
          inputColor = "#fffff")
        
)
#Toggle Between Filters
,tabPanel("",icon = icon("filter"),
         HTML(paste0("<BR><font color='white' style='font-family:Courier;'> Filter by demographics and analyze the crime statistics by demographics.
<br></font><BR>")),
         
         checkboxGroupInput("gender", "Select Gender:",
                            c("Male" = "Male",
                              "Female" = "Female"),selected = list("Male","Female")),
         checkboxGroupInput("proffession", "Select Education Qualification:",
                            c("Primary" = "Primary",
                              "Secondary" = "Secondary",
                              "Graduate" = "Graduate",
                              "Post Graduate"="Post Graduate"),selected = list("Primary","Secondary","Graduate","Post Graduate"))                                                        
         
)
))),
# Adding Box Plot to show Average 										 
box(width = 10,height = 250,
withLoader(plotlyOutput("boxplotage"),type="html" , loader = 'loader1')
),
# Adding Guage to show Total Crime 										 
box(width=4, height = 480,
tabItem(tabName = "gauge",
tabsetPanel(type = "tabs",
tabPanel("Crime Analysis by Age",icon = icon("circle-notch"),
        
        
        radioGroupButtons(
          inputId = "selectcityforage", label = "", 
          choices = c("Los Angeles"="NY", "New York"="LA"), 
          justified = TRUE, status = "primary",
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        ),br(),
        
        
        h3(textOutput("guage")),br(),br(),
        
        
        
        
        
        withLoader(gaugeOutput("gauge235"),type="html", loader = "loader1")),
tabPanel("",icon=icon("info-circle"),
        HTML(paste0('<BR><font color="black" style="font-family:Cursive;">
<b> Crime Analyis by Age for Los Angeles and New York Over Year</b>
<br><br>
Select the age range and demographics filter from the filter panel in left.
<br>
Guage chart shows the total crime in New York and Los Angeles for the selected age group and also shows the average us crime.


<br>
The charts are filterable to all filters on left and also shows color gradient according to the crime rate.When crime increase the guage becomes red showing the intensity of crimes.

</font>')))))),
# Adding Bar Chart to show Crime by type										 
box( width = 6,height = 480,

tabItem(tabName = "Barchart",

tabsetPanel(type = "tabs",
tabPanel("Type of Crime",icon = icon("chart-bar") ,    
         
         # Filter as City										  
         radioGroupButtons(
           inputId = "city", label = "", 
           choices = c("Los Angeles"="Los Angeles", "New York"="New York"), 
           justified = TRUE, status = "primary",
           checkIcon = list(yes = icon("ok", lib = "glyphicon"))
         ),
         
         withLoader(plotOutput("barchart",width = "100%", height = "330px"),loader="loader1"),
         HTML(paste0("<center><font size=3 color='red'><b>--</font><font> Average Us Crime</b></font></center>"))),
tabPanel("",icon = icon("info-circle"),
         HTML(paste0('<BR><font color="black" style="font-family:Cursive;">
<b> Type of Crime for each city for the present year</b>
<br><br>
Select the filter on chart to toggle the city.
<br>
Bar chart shows total crimes for each city for each major type.The data is based on current data and the report is per 100,000 cases. The red line on the graph shows the average US crime.


<br>
The average crime of New York is less than the average crime of Los Angeles and which is less than the US average crime.Average crime of Los Angeles is more than US Average crime.

</font>'))
)))
),
# Add tab: Crime in 2k19											

),
tabPanel( "Crimes In 2k19",icon = icon("chart-area", "fa-1x"),br(),
# Add City Filter on Top								
box(width = 12,collapsible=TRUE,HTML(paste0('
<font color="black" style="font-family:Courier;">
<b>*select city to filter by city.</b></font>')), radioGroupButtons(
inputId = "selectcity1", label = "", 
choices = c("New York"="NY", "Los Angeles"="LA"), 
justified = TRUE, status = "primary",
checkIcon = list(yes = icon("ok", lib = "glyphicon"))
)),
# Add Date Range										 
box(height=150,  HTML(paste0('
<font color="black" style="font-family:Courier;">
<b>*select date to see the plot below.</b></font><BR><BR>')),
dateRangeInput("daterange4", "Select Date:",
start = "2019-01-01",
end = "2019-12-31",
min =   "2019-01-01",
max = "2019-12-31",
autoclose = TRUE

)
# Add Age and Crime Filter							   
),
box(height = 150, 
tabItem(tabName = "filters",
tabsetPanel(type = "tabs",
tabPanel("Select Age",icon = icon("filter"),
        
        sliderInput("Age", 
                    label = "Select the Age(Age starts from 4-99):",
                    min = 4, max = 99,animate = TRUE, value = c(4, 99)
        )),
tabPanel("Select Crime",icon=icon("filter"),
        pickerInput(
          inputId = "myPicker", 
          label = "Select Crimes", 
          choices = c("Murders"= "Murders",
                      "Rapes"="Rapes",
                      "Robberies"="Robberies",
                      "Assaults" = "Assaults" ,
                      "Burglaries" = "Burglaries",
                      "Thefts" = "Thefts",
                      "Auto Thefts" = "Auto Thefts",
                      "Sex Crimes" = "Sex Crimes",
                      "FORGERY" = "FORGERY"), 
          selected = c("Murders"= "Murders",
                       "Rapes"="Rapes",
                       "Robberies"="Robberies",
                       "Assaults" = "Assaults" ,
                       "Burglaries" = "Burglaries",
                       "Thefts" = "Thefts",
                       "Auto Thefts" = "Auto Thefts",
                       "Sex Crimes" = "Sex Crimes",
                       "FORGERY" = "FORGERY"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        ))))),
# Adiing Violin Plot											 
box(height = 530,
tabItem(tabName = "violin",
tabsetPanel(type = "tabs",
tabPanel("Crime Over Year",icon = icon("chart-bar")
        
        ,
        br(),br(),
        withLoader(plotlyOutput("violin"),type="html" , loader = 'loader1')
),
tabPanel("",icon=icon("info-circle"),
        HTML(paste0('<BR><font color="black" style="font-family:Cursive;">
<b> Crime Over Year</b>
<br><br>
Select the date and other filters to analyse the visualisation.*If you see the plot loading select any filter.
<br>
This is a  chart showing crime over year by education and Age for each gender.
The chart is filterable to the dates in 2019 and also to age and crime.The chart can also be put to autoplay using the age filter.
The thickness of the plot depicts the density or number of crime. Plot can also be marked and exported using the icon on the top.

<br>
This plot compares the crime over demographics, the average age of crime is 20-30 which is shown by bulging of plots.This also compares the crimes for each gender.
</font>'))
)))),
# Adding Wordcloud to show crime by words													  
box(height = 530,
tabItem(tabName = "word",
tabsetPanel(type = "pills",
tabPanel("Most Crime",icon = icon("chart-bar"),
        br(),br(),
        withLoader(wordcloud2Output("wordd") , type = "html" ,loader = "loader1")),
tabPanel("Crime By Month",icon=icon("line-chart"),
        # Add Line Chart to show trends																 
        HTML(paste0("This graph shows total crime by Month")),
        withLoader(plotlyOutput("lineyear"),type = "html" , loader = "loader1")
))))
)),

),
# Adding Demographics tab to show demographic details of LA and NY			
tabItem(tabName = "demographics",


tabItem(tabName = "Demograpgics",
tabsetPanel(type = "pills",
tabPanel("Los Angeles",icon = icon("address-card","fa-1x"),br(),
# Population growth by Year								   
box( height = 300,width=9,withLoader(plotlyOutput("popoveryear"))),
box(width = 3,height = 300,
HTML(paste0("<center><font color='#0792b5' size=4><b>Los Angeles Median Age</b></font><br><BR><BR></center>")),
withLoader(imageOutput("image2"))
),
# Sex Ratio	for LA				  
box(withLoader(plotlyOutput("sexratiola"))),
box(height = 425,  tabItem(tabName = "la",
          tabsetPanel(type = "pills",
                      # Donut to show Education	for LA						   
                      tabPanel("Education",icon = icon("university"),br(), withLoader(plotlyOutput("donuteducationla"),type = "html", loader = "loader1")),
                      tabPanel("Population By Race", icon=icon("users"),br(),
                               # Donut to show population by race for LA										   
                               withLoader(plotlyOutput("donutracela")))
                      
          )))),
# Adding New York Tab										   
tabPanel("New York", icon =icon("address-card","fa-1x"),br(),
# Population growth over year for NY							
box( height = 300,width=9,withLoader(plotlyOutput("popoveryearny"))),
box(width = 3,height = 300,
HTML(paste0("<center><font color='#0792b5' size=4><b>New York Median Age</b></font><br><BR><BR></center>")),
withLoader(imageOutput("image2ny"))
),
# Sex ratio for NY							
box(withLoader(plotlyOutput("sexrationy"))),
box(height = 425, tabItem(tabName = "ny",
         tabsetPanel(type = "pills",
                     #	Donut to show Education	for LA									
                     tabPanel("Education",icon = icon("university"),br(),  withLoader(plotlyOutput("donuteducationny"))),
                     # Donut to show population by race for LA													
                     tabPanel("Population By Race", icon=icon("users"),br(),
                              withLoader(plotlyOutput("donutraceny"))
                     )
                     
         )))
)))
)
)
)
)

)
