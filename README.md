# presentations
Use this repository to share presentations developed in R or other plain text languages. Before hosting your presentation, please read through the following guidelines to ensure that your presentation goes off without a hitch.

### Rmarkdown
Developing a presentation in Rmarkdown is a relatively painless process *if* you are relying on an existing codebase for your project. A useful resource for producing Rmarkdown presentations can be found [here](https://bookdown.org/yihui/rmarkdown/presentations.html), which gives an overview of the common packages and themes used to build slides. 

**Note**: The following guidelines are based on a presentation built using the package `xaringan`.

### Directory structure & naming scheme
Why is naming files so difficult? To ameliorate your anxiety, please use the following directory structure and naming scheme for your presentation (numbers indicate outer->inner directory order):

#### Directory Structure
1.  Name of project (e.g. `State of the Ecosystem`)
2.  Name of meeting(s) (e.g. `Council meetings`)
3.  Create a subfolder for each presentation with the meeting title and date (e.g. `NEFMC YYYY-MM-DD`). 
4.  All associated subdirectories and a .Rproj file may be stored here. For example, a folder where R scripts are kept should be called `R`, and this should be on the same level as your .Rproj file. 

#### Presentation naming scheme
Presentation filenames should follow the format of `YYYYMMDD_meeting_surname`.

### Hosting your presentation
If you are compiling your Rmd to HTML, then this repository will allow you to share your product with the world through a URL. However, the following steps must be followed in order to make sure your presentation loads properly: 

1.  Move the compiled HTML document (your presentation) to the `/docs` folder
2.  The JavaScript libraries associated with your presentation will likely already be in `/docs/libs`. However, be sure to add any missing libraries.
3.  Move associated image directories to `/docs`<sup>1</sup> 

[1] During the development process, you will inevitably be sourcing external images (.png, jpg, etc.), and creating your own (i.e. through your code). When compiled to HTML, `knitr` will create a subfolder for the figures you created called `[presentation_name]_files`. If you are sourcing images from your own image folder, name it `[presentation_name]_images`. In an Rmd, images in this folder would be sourced using the path `[presentation_name]_images/[your_image.png]`.
