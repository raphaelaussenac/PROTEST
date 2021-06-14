# exporter l'initialisation
for (scenario in c("ADA", "TRE", "DYN", "PTR", "SCT"))
{
  # title list inside loop because rm in rmd document
  scenario.title <- data.frame(abrv = c("ADA", "TRE", "DYN", "PTR", "SCT"), titre = c("Adaptation forcée", "Transition énergétique", "Redynamisation", "Patrimonialisation", "Sanctuarisation"))
  row.names(scenario.title) <- scenario.title$abrv
  rmarkdown::render("13cd_Initialisation_gestion.Rmd", 
                    params = list(scenario = scenario, titre=scenario.title[scenario, 'titre']),
                    output_file=paste0("./export/initialisation_", scenario,".html"))
}
# 
# exporter la description
for (scenario in c("ADA", "TRE", "DYN", "PTR", "SCT"))
{
  # title list inside loop because rm in rmd document
  scenario.title <- data.frame(abrv = c("ADA", "TRE", "DYN", "PTR", "SCT"), titre = c("Adaptation forcée", "Transition énergétique", "Redynamisation", "Patrimonialisation", "Sanctuarisation"))
  row.names(scenario.title) <- scenario.title$abrv
  rmarkdown::render("14_Description_simulation.Rmd", 
                    params = list(scenario = scenario, titre=scenario.title[scenario, 'titre']),
                    output_file=paste0("./export/description_", scenario,".html"))
}
