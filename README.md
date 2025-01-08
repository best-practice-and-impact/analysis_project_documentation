# analysis_project_documentation
Analysis and model documentation is crucial for the analytical lifecycle and a key element in enforcing good governance. Good quality documentation underpins effective  development and use of analytical workflows. It captures the key decisions that affect the design and use of the analysis and why they were made. It also sets out how the analysis will be assured and who is responsible for this assurance.

## What is in the repository?

This repository provides resources for analysts who need to assure and document their projects. It contains:    
1. Quality Questions. A set of themed questions, structured according to the AQuA Book and the Code of Practice for Statistics, which are designed to help identify areas of risk in an analytical workflow.
2. Downloadable templates for recording commissioning decisions and logging project decisions and assumptions.
3. An HTML template for recording answers to the Quality Questions.

The idea is to make it easier for teams to publish their QA results, to think carefully about assumptions going into their analysis, how these assumptions would impact their output and to document everything in one place. As development progresses, it will demonstrate how the [assumptions](https://github.com/best-practice-and-impact/assumptions) package can be used to log assumptions inline.

## How does the repository work?

The repository uses [Quarto](https://quarto.org/) to construct and deploy an HTML site containing the Quality Questions and associated resources. Web pages are written in Quarto markdown documents (.qmd files) with associated templates in HTML and Excel.  The site is indexed using the `_quarto.yml` YAML markdown file which sets out website structure, templating and formatting.   

The final HTML site is built using Quarto's render function. Rendering automatically creates HTML versions of the .qmd files in the main folder and deploys them to a /docs subfolder along with downloadable resources. The content of /docs then forms the HTML website. The website itself is hosted on Github pages and will automatically update following re-rendering.    

**There should be no need to manually edit the /docs folder**. Edits to site content should be made to the Quarto markdown documents (.qmd) and the site re-rendered in Quarto.  This will automatically overwrite the content of docs. When the updated site is pushed to Github, the website will update automatically.

The repository contains these folders:  

1) **.assumptions**: Contains a standard template to record the assumptions made about the analysis, justify their suitability given the modelling scenario, proof of internal and external verification and the quality rating of each assumption. The folder also contains a markdown document and a package that will search the current directory for assumptions and write the log to the same directory. At the moment these are not integrated into the main Quality Questions website.
2) **.qa_report**: Contains an automated template which include some generic functions on data quality checks, for example, checking for any logical or arithmetic relationships in the data and check if values fall inside an acceptable/expected range. The idea is to produce a summary of the results of tests performed on the most recent data run.  At the moment the QA report is not integrated into the main Quality Questions website - this is a future step.
4) **docs**: Rendered website created by Quarto when the .qmd files in the main folder are rendered out. This website is rendered automatically to Github.io. There should be no need to edit this folder manually as it is automatically updated during rendering. The html contents are not designed to be human-readable.
5) **data**: Example datasets about house prices and university dropouts. At the moment these are not used in Quality Questions. They will be deployed in a future update.

The main folder contains the following key files:

**_quarto.yml** - [YAML](https://yaml.org/spec/1.2.2/) configuration file containing configuration information for the Quarto engine to render the website. Includes page structure and source files.    
**.gitignore** - List of files not to upload to git repository.    
**apd_style.css** - HTML [CSS style sheet](https://www.w3schools.com/html/html_css.asp) which sets up table rendering formatting. Edit this to change the way Quality Questions tables are rendered.   
**assumptions_and_issues_log.qmd** - Quarto markdown document setting out the assumptions and decisions log.    
**faqs.qmd** - Quarto markdown document setting out frequently asked questions.    
**feedback.qmd** - Quarto markdown document linking to a feedback form.    
**index.qmd** - Quarto markdown document setting up the landing page for the quality questions website.    
**issues_log.qmd** - Quarto markdown document setting up the the issues log.    
**quality_questions.qmd** - Quarto markdown document setting out the Quality Questions in table format. This contains the main Quality Questions resource.    
**sample_assumptions_table.qmd** - Quarto markdown document containing an example of a RAG-rated assumption log.    
**README.md** - Project README file.

# Working with the project code

We recommend using Visual Studio Code to work with this project. Open the code workspace in VS Code to load the project. To render the website, open any of the .qmd documents and select the Preview button. Edits to qmd files will only be reflected in the project website after you have re-rendered using Preview.

# Software requirements  
Python 3.8 or later. For ONS users, Python can be installed from ONS Software Centre.   
[Visual Studio Code](https://code.visualstudio.com/Download) with the Quarto, Python and autopep8 extensions installed
[Quarto](https://quarto.org/docs/get-started/)
Git. For ONS users, Git can be installed from ONS Software Centre.

# Useful links
HM Government [AQuA book analytical quality assurance manual](https://www.gov.uk/government/publications/the-aqua-book-guidance-on-producing-quality-analysis-for-government). HM Government.

HM Government Analysis Function. (2020). [Quality assurance of code for analysis and research](https://best-practice-and-impact.github.io/qa-of-code-guidance/ ). Office for National Statistics, Quality and Improvement division.  
