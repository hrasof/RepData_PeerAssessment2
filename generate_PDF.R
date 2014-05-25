Init <- function(workDirStr="C:/Users/hsofoian/Desktop/DataScience/Course 5 - Reproducible Research/RepData_PeerAssessment2"){
        setwd(workDirStr)      
}
Init()

require(knitr)
require(markdown)

knit("Final_Result.Rmd")
markdownToHTML('Final_Result.md', 'Final_Result.html', options=c("use_xhml"))
system("pandoc -s Final_Result.html -o Final_Result.pdf")
