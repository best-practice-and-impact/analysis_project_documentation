# analysis_project_documentation
Analysis and model documentation is crucial for the analytical lifecycle and a key element in enforcing good governance. Good quality documentation underpins effective  development and use of analytical workflows. It captures the key decisions that affect the design and use of the analysis and why they were made. It also sets out how the analysis will be assured and who is responsible for this assurance.

This repository stores templates for recording commissioning decisions and logging assumptions. It includes an HTML template for a quality assurance report. The idea is to make it easier for teams to publish their QA results, to think carefully about assumptions going into their analysis, how these assumptions would impact their output and to document everything in one place. As development progresses, it will demonstrate how the [assumptions](https://github.com/best-practice-and-impact/assumptions) package can be used to log assumptions inline. 

The repository contains these folders:  

1) **.assumptions**: Contains a standard template to record the assumptions made about the analysis, justify their suitability given the modelling scenario, proof of internal and external verification and the quality rating of each assumption. The folder also contains a markdown document and a package that will search the current directory for assumptions and write the log to the same directory.
2) **.qa_report**: Contains an automated template which include some generic functions on data quality checks, for example, checking for any logical or arithmetic relationships in the data and check if values fall inside an acceptable/expected range. The idea is to produce a summary of the results of tests performed on the most recent data run.  
4) **docs**: Rendered website created by Quarto when the .qmd files in the main folder are rendered out. This website is rendered automatically to Github.io.
5) **data**: Example datasets about house prices and university dropouts.

The main folder contains the following key files:

**_quarto.yml** - YAML file containing configuration information for the Quarto engine to render the website. Includes page structure and source files.    
**.gitignore** - List of files not to upload to git repository.    
**apd_style.css** - HTML style sheet which sets up table rendering formatting. Edit this to change the way Quality Questions tables are rendered.   
**assumptions_and_issues_log.qmd** - Quarto markdown document about the assumptions log.    
**faqs.qmd** - Quarto markdown document containing frequently asked questions.    
**feedback.qmd** - Quarto markdown document linking to feedback form.    
**index.qmd** - Quarto markdown document setting up the landing page for the quality questions website.    
**issues_log.qmd** - Quarto markdown document about the issues log.    
**quality_questions.qmd** - Quarto markdown document setting out the Quality Questions in table format.    
**sample_assumptions_log.qmd** - Quarto markdown document containing an example of a RAG-rated assumption log.    
**README.md** - Project README file.

# Working with the project code

We recommend using Visual Studio Code to work with this project. Open the code workspace in VS Code to load the project. To render the website, open any of the .qmd documents and select the Preview button. Edits to qmd files will only be reflected in the project website after you have re-rendered using Preview.

# Software requirements  
Python 3.8 or later    
Visual Studio Code with the Quarto, Python and autopep8 extensions    
Quarto

# Useful links
HM Government [AQuA book analytical quality assurance manual](https://www.gov.uk/government/publications/the-aqua-book-guidance-on-producing-quality-analysis-for-government). HM Government.

HM Government Analysis Function. (2020). [Quality assurance of code for analysis and research](https://best-practice-and-impact.github.io/qa-of-code-guidance/ ). Office for National Statistics, Quality and Improvement division.  
