library(readxl)

modelling_refs <- read_excel(
  "data/background/LiteratureSearch.xlsx",
  sheet = "Modelling"
  )

latex_ids <- modelling_refs$LatexID
sim_types <- modelling_refs$`Simulation Type`

cat("\n\n\n\n")

for (i in 1:nrow(modelling_refs)) {
  cat(sprintf(
    "\\citetitle{%s} & \\citeauthor*{%s} & \\citeyear{%s} & %s & \\cite{%s} \\\\ %s",
    latex_ids[i], latex_ids[i], latex_ids[i], sim_types[i], latex_ids[i],
    ifelse(i == nrow(modelling_refs), "", "\\midrule") 
  ))
  cat("\n")
}