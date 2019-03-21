# presentations
Use this repository to share presentations developed in R or other plain text languages. 

### Rmarkdown
Developing a presentation in Rmarkdown is a relatively painless process *if* you are relying on an existing codebase for your project. A useful resource for producing Rmarkdown presentations can be found [here](https://bookdown.org/yihui/rmarkdown/presentations.html), which gives an overview of the common packages and themes used to build slides. 

If you are compiling your Rmd to HTML, then this repository will allow you to share your product with the world through a URL. Simply save your .html file to the `docs` folder, and voli&agrave;! Your presentation is now available through the URL: `noaa-edab.github.io/presentations/[filename.html]`. Note that it may take a few minutes before this link is active.

### Directory structure & naming scheme
Why is naming files so difficult? To ameliorate your anxiety, please use the following directory structure and naming scheme for your presentation (numbers indicate outer->inner directory order):


#### Directory Structure
1.  Name of project (e.g. `State of the Ecosystem`)
2.  Name of meeting(s) (e.g. `Council meetings`)
3.  Create a subfolder for each presentation with the meeting title and date (e.g. `NEFMC YYYY-MM-DD`). 
4.  All associated subdirectories and a .Rproj file may be stored here. For example, a folder where R scripts are kept should be called `R`, and this should be on the same level as your .Rproj file. 

#### Presentation naming scheme
Presentation filenames should follow the format of `YYYYMMDD_meeting_surname`.


