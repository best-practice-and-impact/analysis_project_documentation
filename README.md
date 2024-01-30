# analysis_project_documentation
Model documentation is crucial for the analytical lifecycle and a key element in enforcing good governance. Good quality documentation underpins effective  development and use of analytical workflows. It captures the key decisions that affect the design and use of the analysis and why they were made. It also sets out how the analysis will be assured and who is responsible for this assurance.

This repository stores some templates for recording commissioning decisions and logging assumptions. It also includes an automated template for a quality assurance report. The idea is to make it easier for teams to publish their QA results, to think carefully about assumptions going into their analysis, how these assumptions would impact their output and to document everything in one place. Moreover, it will also demonstrate how the [assumptions](https://github.com/best-practice-and-impact/assumptions) package can be used to log assumptions. 

The repository contains these folders:  

1) **commissioning**: Contains Excel template on commissioning where teams can document the scope, limitations and risks around the analysis as well as providing information on specific roles and responsibilties.
2) **assumptions**: Contains a standard template to record the assumptions made about the analysis, justify their suitability given the modelling scenario, proof of internal and external verification and the quality rating of each assumption. The folder also contains a markdown document and a package that will search the current directory for assumptions and write the log to the same directory.
3) **qa_report**: Contains an automated template which include some generic functions on data quality checks, for example, checking for any logical or arithmetic relationships in the data and check if values fall inside an acceptable/expected range. The idea is to produce a summary of the results of tests performed on the most recent data run.  
4) **website_quality_questions**: The folder provides a set of questions to help analytical and statistical teams evaluate the quality of their analysis throughout the analytical cycle. The tool is in the form of an interactive website which can be found in the site folder.  
5) Data: Example datasets on house prices and university dropouts.

# Software requirements  
Python/R  
VSCode  
Quarto  

# Useful links  
UK Government Analytical Community. (2020). [Quality assurance of code for analysis and research](https://best-practice-and-impact.github.io/qa-of-code-guidance/ ). Office for National Statistics, Quality and Improvement division.  
