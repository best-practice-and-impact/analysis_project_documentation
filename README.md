# Project_documentation
Model documentation is crucial to modelling lifecycle and its a key element in enforcing good governance. Good quality documentation underpins effective model development and use. It captures the key decisions that affect model design, use and why they were made. It also sets out how the model will be assured and who is responsible for this assurance.

This repository stores some templates on commissioing and assumptions log as well as an automated template on quality assurance report. The idea is to make it easier for teams to publish their QA results, to think carefully about assumptions going into their model, how these assumptions would impact their output and to document everything in one place. Moreover, it will also demonstarte how assumptions' package (developed by David Foster) can be used to log assumptions. 

The repository contains the follwing folders:  

1) Commissioning: It contains Excel template on commissioning where the modelling teams can document the scope, limitations and risks around the models as well as providing information on specific roles and responsibilties.
2) Assumptions: It contains a standard template to record the assumptions made about the model, justify their suitability given the modelling scenario, proof of internal and external verification and the quality rating of each assumption. The folder also contains a markdown document and a package that will search the current directory for assumptions and write the log to the same directory.
3) QA_report: It contains an automated template which include some generic functions on data quality checks, for example, checking for any logical or arithmetic relationships in the data and check if values fall inside an acceptable/expected range. The idea is to produce a summary of the results of tests performed on the most recent data run.  
4) Guidance on Quality Questions: The folder contains a tool on guidance on qulaity questions which provides a set of questions to help analytical and statistical teams evaluate the quality of their analysis throughout the analytical cycle. The tool is in the form of an interactive website which can be found in the site folder.  
5) Data: Example dataset on house prices and university dropouts.

# Software requirements  
Python/R  
VSCode  
Quarto  

# Useful links  
UK Government Analytical Community. (2020). Quality assurance of code for analysis and research (version 2022.4). Office for National Statistics, Quality and Improvement division: https://best-practice-and-impact.github.io/qa-of-code-guidance/  

https://gitlab-app-l-01/ASAP/coding-getting-started-guide/-/wikis/home  
