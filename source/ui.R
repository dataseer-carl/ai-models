#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(navbarPage(
	
  "Predictive Analytics and Applied AI for business", # Application title
  
  # Regression ####
  navbarMenu(
  	"Regression", # menu name
  	"Statistical learning", # section title
  	## _OLS ####
  	tabPanel(
  		"OLS", # tab name
  		p("Lorem ipsum")
  	),#tabPanel:OLS
  	"----",
  	"Machine learning", # section title
  	## _DT&RF ####
  	tabPanel("DT & RF"),#tabPanel:Trees&Forests
  	## _Neural net ####
  	tabPanel("Neural net")#tabPanel:NeuralNet
  ),#navbarMenu:Regression
  
  # Classification ####
  navbarMenu(
  	"Classification", # menu name
  	"Statistical learning", # section title
  	## _Logit ####
  	tabPanel("Logistic"),#tabPanel:Logistic
  	"----",
  	"Machine learning", # section title
  	## _DT&RF ####
  	tabPanel("DT & RF"),#tabPanel:Trees&Forests
  	## _Neural net ####
  	tabPanel("Neural net"),#tabPanel:NeuralNet
  	## _SVM ####
  	tabPanel("SVM")#tabPanel:SVM
  ),#navbarMenu:Classification
  
  # About ####
  tabPanel(
  	"About", # tab name
  	p("Lorem ipsum"),
  	verbatimTextOutput("check_WD")
  ),#tabPanel
  
  # Layout config
  # selected = "",
  inverse = TRUE,
  collapsible = TRUE,
  fluid = TRUE
  
))#navbarPage, shinyUI
