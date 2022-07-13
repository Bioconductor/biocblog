# biocblog
A Distill Blog for Bioconductor community

## How to contribute biocblog

1. Fork and clone this repo.
2. Create an RStudio project from the repo.
3. Run the following commands in the R interactive shell in RStudio.
  ```
  library(distill)
  create_post("FILL YOUR POST TITLE")
  ```
4. Write your article into `_posts > DATE-YOURTITLE > YOURTITLE.Rmd`
5. Click "Knit" button under the RStudio YOURTITLE.Rmd tab.
6. Git add and commit the whole `_posts > DATE-YOURTITLE` folder.
7. Send 6. as a pull request.

## How to build biocblog

1. Fork and clone this repo.
2. Create an RStudio project from the repo.
3. Click "Build Website" button under the RStudio "Build" tab.
