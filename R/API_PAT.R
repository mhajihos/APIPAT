API_PAT=function()
{

Pack=c("shiny","shinythemes","gridExtra","ggplot2","ggpubr",
         "readr","reshape2","plyr","dplyr","pkr","PKNCA",
         "erer","stringr","psych","DescTools","geepack",
         "broom","mapview","shinyalert","Cairo","shinydashboard",
         "shinydashboardPlus","shinyWidgets","officer","knitr")
suppressPackageStartupMessages(lapply(Pack, require, character.only = TRUE))



#Functions
#%CV: Coefficient of variation function
CV <- function(mean, sd){
      (sd/mean)*100
      }

#Data_summary
data_summary <- function(data, varname, group){

 summary_func <- function(x, col){
    c(Arithmetic_Mean = round(mean(x[[col]], na.rm=TRUE),3),
      SD = round(sd(x[[col]], na.rm=TRUE),3),
	Percent_CV= round(CV(mean(x[[col]], na.rm = T),sd(x[[col]], na.rm =TRUE)),3),
	Geometric_Mean= round(geomean(x[[col]], na.rm =TRUE),3),
	Geometric_SD= round(geosd(x[[col]], na.rm =TRUE),3),
	Geometric_CV= round(geocv(x[[col]],na.rm =TRUE),3)
	)
  }
data_sum<-ddply(data,group, .fun=summary_func,
                  varname)
return(data_sum)
}


#DRA_plot
DRA_plot=function(X,Y,Grp,Col,Data,Fac1,Fac2,Xlim,logscale=FALSE){
attach(Data)

if(logscale==TRUE)
{

if(Fac2=="NULL")
{
P1=ggplot(Data,
		aes(x=as.numeric(X),y=Y,group=Grp,color=factor(Col)))+
		geom_line(size=1.2)+geom_point(size=3)+xlab("Time")+
		ylab(paste0("Average Concentration in Log Scale"))+
		facet_wrap(~Data[,Fac1],ncol=2)+scale_y_log10()+theme_classic()+
		theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(color="Dose",shape="Dose")+
		scale_x_continuous(limits = c(0,max(Data[,Xlim])),breaks=Data[,Xlim])
}else {
		P1=ggplot(Data,
		aes(x=as.numeric(X),y=Y,group=Grp,color=factor(Col)))+
		geom_line(size=1.2)+geom_point(size=3)+xlab("Time")+
		ylab(paste0("Average Concentration in Log Scale"))+ theme_classic()+
		facet_wrap(~Data[,Fac1]+Data[,Fac2],ncol=2)+scale_y_log10()+
		theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(color="Dose",shape="Dose")+
		scale_x_continuous(limits = c(0,max(Data[,Xlim])),breaks=Data[,Xlim])
	}
}else if(logscale==FALSE){

if(Fac2=="NULL")
{
P1=ggplot(Data,
		aes(x=as.numeric(X),y=Y,group=Grp,color=factor(Col)))+
		geom_line(size=1.2)+geom_point(size=3)+xlab("Time")+
		ylab(paste0("Average Concentration"))+theme_classic()+
		facet_wrap(~Data[,Fac1],ncol=2)+
		theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(color="Dose",shape="Dose")+
		scale_x_continuous(limits = c(0,max(Data[,Xlim])),breaks=Data[,Xlim])
}else {
		P1=ggplot(Data,
		aes(x=as.numeric(X),y=Y,group=Grp,color=factor(Col)))+
		geom_line(size=1.2)+geom_point(size=3)+xlab("Time")+
		ylab(paste0("Average Concentration"))+ theme_classic()+
		facet_wrap(~Data[,Fac1]+Data[,Fac2],ncol=2)+
		theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(color="Dose",shape="Dose")+
		scale_x_continuous(limits = c(0,max(Data[,Xlim])),breaks=Data[,Xlim])
	}

}
print(P1)
}



## ui ##
ui <- dashboardPage(
	dashboardHeader(title="API-PAT",titleWidth =200),
	#collapsed = TRUE,
	  dashboardSidebar(width=280,
		sidebarMenu(tags$style(HTML(".main-sidebar { font-size: 16px; color: white;}")),
				menuItem(HTML("Introduction"), tabName = "Intro"),
				menuItem(HTML("Data Entry and <br/> Descriptive Statistics"), tabName = "Data"),
				menuItem(HTML("Dose-Response Analysis"), tabName = "DRA"),
				menuItem(HTML("Non-Compartmental Analysis"), tabName = "NCA"),
				menuItem(HTML("Adverse Events"), tabName = "AE")
				)
				),
	dashboardBody(
				tags$head(tags$style(HTML('
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }'))),
		 tabItems(
				tabItem(tabName = "Intro",
						box(title = "Introduction",background = "light-blue",solidHeader = TRUE, collapsible = TRUE,
							htmlOutput("text1"),
							   tags$head(tags$style("#text1{color: white;
                                 						font-size: 18px;
											font-family: Times New Roman;
                                 						}"))
							)
						),
		tabItem(tabName = "Data",
				box(id="box1",title = "Data Entry",background = "light-blue",solidHeader = TRUE, collapsible = TRUE,
					fileInput(inputId = "file", label = "Please input CSV File", accept='.csv'),
	   				 	selectInput(inputId = "ID", label = "ID Variable", choices =NULL),
          					selectInput(inputId = "Con", label = "Concentration Variable", choices =NULL),
          					selectInput(inputId = "Time", label = "Time Variable", choices =NULL),
          					selectInput(inputId = "Analyte", label = "Analyte Variable",choices =NULL),
	   			 		selectInput(inputId = "Dose", label = "Dose Variable",choices =NULL),
	    					selectInput(inputId = "Str", label = "Stratification",choices =NULL),
  						actionButton("do", "RUN")),

				box(id="box2",title = "Descriptive Statistics",background = "light-blue",solidHeader = TRUE, collapsible =FALSE,
						radioButtons("Stratify",
               					label = HTML('<FONT size="3pt">Descriptive Statistics Based on:</FONT></FONT><br>'),
               					choices = "Updating",
               					inline = T,
               					width = "100%"),
					 downloadButton('download1', 'Download Descriptive Table')
					),

		mainPanel(
		tags$style(type="text/css",
      	".shiny-output-error { visibility: hidden; }",
      	".shiny-output-error:before { visibility: hidden; }"
    		),
 		tabPanel('Results',tableOutput('table')),
 		plotOutput("plot1", height =900, width =900))
		),


		tabItem(tabName = "DRA",
				box(id="box3",title = "Dose-Response Analysis",background = "light-blue",solidHeader = TRUE, collapsible = TRUE,

					radioButtons("Stratify2",
               			label = HTML('<FONT size="3pt">DRA Plots Based on:</FONT></FONT><br>'),
               			choices = "Updating",
               			inline = T,
               			width = "100%"),

					radioButtons("Stratify22",
               			label = HTML('<FONT size="3pt">Log Scale:</FONT></FONT><br>'),
               			choices = list("Log Scale","Normal Scale"),
               			inline = T,
               			width = "100%"),

					downloadButton('download4', 'Download DRA Graphs')),
		mainPanel(
			tags$style(type="text/css",
     		 	".shiny-output-error { visibility: hidden; }",
      			".shiny-output-error:before { visibility: hidden; }"),
			plotOutput("plot2",width="1200px",height="1000px"))),


		tabItem(tabName = "NCA",
				box(id="box4",title = "Non-Compartmental Analysis",background = "light-blue",solidHeader = TRUE, collapsible = TRUE,

					radioButtons("Stratify3",
               			label = HTML('<FONT size="3pt">NCA Based on:</FONT></FONT><br>'),
               			choices = "Updating",
               			inline = T,
               			width = "100%"),

					radioButtons("Stratify4",
               			label = HTML('<FONT size="3pt">Reference Group for Ratios:</FONT></FONT><br>'),
               			choices ="Updating",
               			inline = T,
               			width = "100%"),
					actionButton("do1", "RUN"),
					downloadButton('download2', 'Download NCA Descriptive Tables')),
		mainPanel(
		tags$style(type="text/css",
     		".shiny-output-error { visibility: hidden; }",
      		".shiny-output-error:before { visibility: hidden; }"),
		tabPanel('Results3',tableOutput('table2')),
		plotOutput("plot3", height =500, width =1200))),


		tabItem(tabName = "AE",

		mainPanel(
		tabPanel('Results4',tableOutput('table3')),
		plotOutput("plot4", height =500, width =500)))


)))








#Server
server=function(input, output, session)
{
options(scipen = 100, digits = 4)
options(shiny.maxRequestSize=30*1024^2)


# Read the input file and fix the names
my_data <- reactive({
    	inFile=input$file
    	if (is.null(inFile)) return(NULL)
    	data=read_csv(inFile$datapath)

	data=apply(data,2,function(x) {iconv(x, "latin1", "UTF-8")})
as_tibble(data)

  })


observe({

    # Change values for columns
    s_options <- as.list(c(colnames(my_data()),"Not_Available"))
	updateSelectInput(session = session, inputId = "ID", choices =s_options)
    	updateSelectInput(session = session, inputId = "Con", choices =s_options)
	updateSelectInput(session = session, inputId = "Time", choices =s_options)
	updateSelectInput(session = session, inputId = "Analyte", choices =s_options)
	updateSelectInput(session = session, inputId = "Dose", choices =s_options)
	updateSelectInput(session = session, inputId = "Str", choices =s_options)

  	})





observeEvent(input$do,{
updateBox("box1", action = "toggle")


observe({
			StrOption=list(input$Time)

			if(input$Analyte!="Not_Available")
				{StrOption=append(StrOption,input$Analyte)}

			if(input$Dose!="Not_Available")
				{StrOption=append(StrOption,input$Dose)}

			if(input$Str!="Not_Available")
				{StrOption=append(StrOption,input$Str)}

	updateRadioButtons(session, "Stratify",choices=StrOption)
})


#TABLE DESCRIPTIVE
TableInput <- reactive({
data=my_data()
data=data.frame(data)
data$CONC=as.numeric(data[,input$Con])

Summ=data_summary(data=data,varname="CONC",group=input$Stratify)

return(Summ)

})

#For output
output$table <- renderTable({
print(TableInput())
},
bordered = TRUE)



observe({
			StrOption2=list(input$ID)

			if(input$Analyte!="Not_Available")
				{StrOption2=append(StrOption2,input$Analyte)}

			if(input$Dose!="Not_Available")
				{StrOption2=append(StrOption2,input$Dose)}

			if(input$Str!="Not_Available")
				{StrOption2=append(StrOption2,input$Str)}

	updateRadioButtons(session, "Stratify2",choices=StrOption2)
})



#DRA PLOTS
PlotInput1 <- reactive({
data=my_data()
data=data.frame(data)
data$CONC=as.numeric(data[,input$Con])
data$Time=as.numeric(data[,input$Time])

if(input$Stratify22=="Log Scale")
{

if(input$Stratify2==input$Dose)
{
	Plot=DRA_plot(X=factor(data[,input$Time]),Y=CONC,Grp=factor(data[,input$Dose]),
				Col=factor(data[,input$Dose]),Data=data,Fac1=sort(input$Stratify2),
				Fac2="NULL",Xlim="Time",logscale=TRUE)
}else{
	Plot=DRA_plot(X=factor(data[,input$Time]),Y=CONC,Grp=factor(data[,input$Dose]),
				Col=factor(data[,input$Dose]),Data=data,Fac1=sort(input$Stratify2),
				Fac2=input$Dose,Xlim="Time",logscale=TRUE)
	}

}else if(input$Stratify22=="Normal Scale")
{

if(input$Stratify2==input$Dose)
{
	Plot=DRA_plot(X=factor(data[,input$Time]),Y=CONC,Grp=factor(data[,input$Dose]),
				Col=factor(data[,input$Dose]),Data=data,Fac1=sort(input$Stratify2),
				Fac2="NULL",Xlim="Time",logscale=FALSE)
}else{
	Plot=DRA_plot(X=factor(data[,input$Time]),Y=CONC,Grp=factor(data[,input$Dose]),
				Col=factor(data[,input$Dose]),Data=data,Fac1=sort(input$Stratify2),
				Fac2=input$Dose,Xlim="Time",logscale=FALSE)
	}
}

print(Plot)
})


#For Output
output$plot2 <- renderPlot({
print(PlotInput1())
})


#NCA

observe({
			StrOption3=list()

			if(input$Analyte!="Not_Available")
				{StrOption3=append(StrOption3,input$Analyte)}

			if(input$Str!="Not_Available")
				{StrOption3=append(StrOption3,input$Str)}

	updateRadioButtons(session, "Stratify3",choices=StrOption3)
})


observeEvent(input$do1,{
data=my_data()
data=data.frame(data)
			StrOption4 <- as.list(levels(as.factor(data[,input$Stratify3])))
			updateRadioButtons(session, "Stratify4",choices=StrOption4)
})


TableInput2<-reactive({
data=my_data()
data=data.frame(data)
data$CONC=as.numeric(data[,input$Con])
data$Time=as.numeric(data[,input$Time])
data$ID=as.numeric(data[,input$ID])

Grp=levels(as.factor(data[,input$Stratify3]))
R1=c()
Mat=c()
k=1

while(k<=length(Grp))
{
Data=subset(data,data[,input$Stratify3]==Grp[k])

NCAData=try(NCA(Data,"ID","Time","CONC",fit = "Log"),silent = TRUE)
if("try-error" %in% class(NCAData))
{
Mat=rbind(Mat,cbind(Measure=c("AUCALL","CMAX","TMAX","Half_Life"),
		Grp1=rep(Grp[k],4),Mean=rep(NA,4),
		SD=rep(NA,4),GeoMean=rep(NA,4),GeoSD=rep(NA,4)))
}else{

RESData=NCAData[,c("ID","AUCALL","CMAX","TMAX","LAMZHL")]

Mean1=apply(RESData[,-1],2,mean)
SD1=apply(RESData[,-1],2,sd)

GeoMean1=apply(RESData[,-1],2,geomean)
GeoSD1=apply(RESData[,-1],2,geosd)

Mat=rbind(Mat,cbind(Measure=c("AUCALL","CMAX","TMAX","Half_Life"),
		Grp1=rep(Grp[k],4),Mean=round(Mean1,2),
		SD=round(SD1,1),GeoMean=round(GeoMean1,2),GeoSD=round(GeoSD1,1)))

R1=rbind(R1,cbind(ID=RESData[,1],AUCALL=RESData[,2],CMAX=RESData[,3],
			Tmax=RESData[,4],Thalf=RESData[,5],
			Grp1=rep(Grp[k],dim(RESData)[1])))
}
k=k+1
}

R1=data.frame(R1)
R1$AUCALL=as.numeric(R1$AUCALL)
R1$CMAX=as.numeric(R1$CMAX)
R1$Tmax=as.numeric(R1$Tmax)
R1$Thalf=as.numeric(R1$Thalf)
R1$Grp1=as.factor(R1$Grp1)



if(dim(R1)[1]==0)
{
	shinyalert("Error!", "No Result Generated", type = "error")
}else{

	DataR1=melt(R1,id.vars=c("Grp1"),measure.vars=c("AUCALL","CMAX","Tmax","Thalf"))
	names(DataR1)[2]="Index"

#Output
Param=unique(DataR1$Index)
Group=unique(DataR1$Grp1)
DescRes=matrix(NA,length(Param),length(Group)*2)

for(i in 1:length(Param))
{
mat=c()

	for(j in 1:length(Group))
	{
mat2=matrix(NA,1,2)
	SG1=paste0(round(mean(DataR1$value[DataR1$Grp1==Group[j] & DataR1$Index==Param[i]]),2),
			" ","(",round(sd(DataR1$value[DataR1$Grp1==Group[j] & DataR1$Index==Param[i]]),2),")")

	SG1Med=paste0(round(median(DataR1$value[DataR1$Grp1==Group[j] & DataR1$Index==Param[i]]),2),
			" ","(",round(quantile(DataR1$value[DataR1$Grp1==Group[j] & DataR1$Index==Param[i]],0.25),2),"-",
				round(quantile(DataR1$value[DataR1$Grp1==Group[j] & DataR1$Index==Param[i]],0.75),2),")")
mat2=cbind(SG1,SG1Med)
colnames(mat2)=c(paste0("Mean (SD) of Group ",Group[j]),paste0("Median (IQR) of Group ",Group[j]))
	mat=cbind(mat,mat2)
	}

DescRes[i,]=mat
}
DescRes=data.frame(Param,DescRes)


Names=c()
for(k in 1:length(Group))
{
	Names=c(Names,c(paste0("Mean (SD) of Group ",Group[k]),paste0("Median (IQR) of Group ",Group[k])))
}
names(DescRes)=c("Parameters",Names)

Result=c()
#Ratios
Para=colnames(R1)[-c(1,dim(R1)[2])]
k=1
Ref=input$Stratify4
Grp2=levels(R1$Grp1)[levels(R1$Grp1)!=Ref]

GMR=matrix(NA,length(Param),length(Grp2))
while(k<=length(Grp2))
{
	for(j in 1:length(Para))
	{
		Ratio=(R1[,colnames(R1)==Para[j]][R1$Grp1==Grp2[k]])/(R1[,colnames(R1)==Para[j]][R1$Grp1==Ref])
		GmeanR=Gmean(Ratio, conf.level = 0.9, sides = "two.sided",method ="boot", na.rm = TRUE)
		Ratio_Res=paste0(round(GmeanR[1],2)," ","(","90%CI:"," ","[",round(GmeanR[2],2),",",round(GmeanR[3],2),"]",")")

		GMR[j,k]=Ratio_Res
	}
k=k+1
}

GMR=data.frame(GMR)

Names2=c()
for(k in 1:length(Grp2))
{
	Names2=c(Names2,c(paste0("GMR of Group ",Grp2[k]," ","Vs. ",input$Stratify4)))
}
names(GMR)=Names2

Final_Results=cbind(DescRes,GMR)

print(Final_Results)

}

})

#For Output
output$table2<- renderTable({
print(TableInput2())
},
bordered = TRUE)


#Downloads
output$download1<-downloadHandler(

filename ="APIPAT_Descriptive_Table.csv",

content = function(con) {
inFile=input$file
	  setwd("~/")
 	  file.create("report1.csv")
        tempTemplate <<- file.path(getwd(), "report1.csv")
        file.copy("report1.csv", tempTemplate, overwrite = TRUE)

write.csv(TableInput(), con, row.names = FALSE)

})


output$download4<-downloadHandler(

filename ="APIPAT_DRA_Graphs.jpeg",

content = function(con) {
inFile=input$file
	  setwd("~/")
 	  file.create("report4.jpeg")
        tempTemplate <<- file.path(getwd(), "report4.jpeg")
        file.copy("report4.jpeg", tempTemplate, overwrite = TRUE)
ggsave(con,PlotInput1(),width = 10, height = 12, units = "in")

})


output$download2<-downloadHandler(

filename ="APIPAT_NCA_Table.csv",

content = function(con) {
inFile=input$file
	  setwd("~/")
 	  file.create("report3.csv")
        tempTemplate <<- file.path(getwd(), "report3.csv")
        file.copy("report3.csv", tempTemplate, overwrite = TRUE)

write.csv(TableInput2(),con, row.names = FALSE)
})





})

output$text1 <- renderUI({
    HTML(paste("API-PAT:Applied Pharmaceutical Innovation PK Analysis Tool. API-PAT designed to perform Dose-Response and Non-Compartmental Analysis.",sep = '<br/>'))
  })


}

shinyApp(ui = ui, server = server)

}


