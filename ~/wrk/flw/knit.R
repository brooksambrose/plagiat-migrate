#knit2md
rm(list=ls())
cat('\014')
require(knitr)
for(ki in 4) {
  ki<-sprintf('%02d',ki)
  knit(
    input=paste0('~/wrk/Rmd/',ki,'.Rmd')
    ,output=paste0('~/wrk/md/',ki,'.md')
  )

  call<-paste0(
    '/usr/lib/rstudio-server/bin/pandoc/pandoc +RTS -K512m -RTS ~/wrk/md/'
    ,ki
    ,'.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output ~/prd/tex/'
    ,ki
    ,'.tex --filter /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc'
    ,' && '
    ,'/usr/lib/rstudio-server/bin/pandoc/pandoc +RTS -K512m -RTS ~/prd/tex/'
    ,ki
    ,'.tex --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output ~/prd/pdf/'
    ,ki
    ,'.pdf --latex-engine pdflatex --bibliography ~/prd/tex/00.bib --filter /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc'
  )
  cat(call,'\n',sep='')
  system(call)
}
if(F){
system('cd ~/prd/tex && latexpand 00.tex > ~/prd/tex/0.tex')
#/usr/lib/rstudio-server/bin/pandoc/pandoc +RTS -K512m -RTS ~/prd/tex/0.tex --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash --output ~/prd/+Ambrose_dissertation-draft_do-not-circulate.pdf --latex-engine pdflatex --bibliography ~/prd/tex/00.bib --filter /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc
system('cd ~/prd/tex && pdflatex ~/prd/tex/00.tex')
}