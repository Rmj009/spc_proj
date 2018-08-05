
# install_pack ------------------------------------------------------------


# install.packages("shiny")
# install.packages("qcc")      oc.curves
install.packages("SixSigma")
install.packages("qicharts")
install.packages("qcr")
install.packages("spc")
install.packages("IQCC")
install.packages("mpcv")           # multivariate spc
install.packages("MSQC")           # mulivariate spc
install.packages("tolerance")     # calucate the CI,  threhold , specification
install.packages("spcadjust")     # 強大  https://journal.r-project.org/archive/2017/RJ-2017-014/index.html
install.packages("edcc")          # economic control chart
# install.packages("knitr")
# install.packages("htmlwidgets")
install.packages("ggplot2")
install.packages("RODBC")
install.packages("RMySQL")
install.packages("XML")
install.packages("rvest")
install.packages("h5")
install.packages("mongolite")
install.packages("lubridate")
library(mongolite)
library(lubridate)
library(MASS)
library(SixSigma,qicharts,qcr);library(spc,IQCC);require(mpcv,spcadjust);library(MSQC,tolerance)
library(edcc)
  # control chart ---------------------------------------------------------------------

  
qcc(data=cars,type = "R")
xbar.thick<-qcc(data= samples.thick,type="xbar")
r.thick<-qcc(data= samples.thick,type="S")
qcc(data=cars,type = "xbar.one")
qcc(data=,type = "p")         # to be form later on
qcc(data=cars,type = "np")
qcc(data=cars,type = "c")
qcc(data=cars,type = "u")
qcc(data=cars,type = "g")
?qcc()

plot(xbar.thick)
qcc.options("beyond.limits"=list(pch=20,col="red3"))
qcc.options(bg.margin="azure2")
plot(xbar.thick,axes.las=1,digits=3,title="X-Bar chart metal plates thickness",xlab="Shift",ylab="Sample mean",ylim=c(0.70,0.80))

      # MR chart ----------------------------------------------------------------

thickness2days<-ss.data.thickness2$thickness[1:24]
mov.samples<-cbind(thickness2days[1:23],thickness2days[2:24])
cci<-qcc(thickness2days,type="xbar.one")
ccmr<-qcc(mov.samples,type="R")
par(mfrow=c(2,1))
plot(cci,restore.par=FALSE,add.stats=FALSE)
plot(ccmr,add.stats=FALSE)


      # CUSUM & EWMA-------------------------------------------------------------------
cusum.thick<-cusum(data= thickness2days)
summary(cusum.thick)
ewma.thick<-ewma(data= thickness2days)

  # Nonlinear Control chart -------------------------------------------------

## The functionsmoothProfilesintheSixSigmapackage makes use of regularization theory in order to smooth the profile
plot(ss.data.wbx, ss.data.wby[,"P1"],type="l") #require(sixsigma)
P1.smooth<-smoothProfiles(profiles= ss.data.wby[,"P1"],x= ss.data.wbx)
plotProfiles(profiles=cbind(P1.smooth,ss.data.wby[,"P1"]),x= ss.data.wbx)
plotProfiles(profiles= ss.data.wby,x= ss.data.wbx)
wby.smooth<-smoothProfiles(profiles= ss.data.wby,x= ss.data.wbx)
plotProfiles(profiles= wby.smooth,x= ss.data.wbx)
## Phase I & phase II

   ## Phase I,an in-control to test baseline subset of profiles is seeking 
   ## the stability of the process, in order to model the in-control process performance
wby.phase1<-ss.data.wby[,1:35]
wb.limits<-climProfiles(profiles= wby.phase1,x= ss.data.wbx,smoothprof=TRUE,smoothlim=TRUE)
plotProfiles(profiles= wby.phase1,x= ss.data.wbx,cLimits= wb.limits)
     ###  function climProfiles calculates confidence bandsand an estimation of the prototype profilef.x/
wb.out.phase1<-outProfiles(profiles= wby.phase1,x= ss.data.wbx,cLimits= wb.limits)
     ### function outProfiles returns  a  list  of  three  vectors. 
     #### The  first  vector(labOut)  contains  the  labels  of  the  out-of-control  profiles.
     #### The  second  vector(idOut)  contains  the  indexes  of  the  out-of-control  profiles.  
     #### This  vector  is  givenfor completeness as in some cases the index may be preferable to the label.
     #### The third vector contains the proportion of times that each profile remains out of theconfidence bands. By default, the function considers a profile to be out of controlif the proportion of times that this profile remains out of the confidence bands isover 0.5. 



   ## Phase II, the goal is to monitor the process
   ## For PhaseI is used for the detection of out-of-control profiles over a set of new profiles notpreviously analyzed.

wby.phase2<-ss.data.wby[,36:50]
wb.out.phase2<-outProfiles(profiles= wby.phase2,x= ss.data.wbx,cLimits= wb.limits,tol=0.8)
plotProfiles(wby.phase2,x= ss.data.wbx,cLimits= wb.limits,outControl= wb.out.phase2$idOut)
plotProfiles(wby.phase2,x= ss.data.wbx,cLimits= wb.limits,outControl= wb.out.phase2$idOut,onlyout=TRUE)
## elimiate glitch

# shiny.ui ----------------------------------------------------------------
library(shiny)
runExample("05_sliders")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}