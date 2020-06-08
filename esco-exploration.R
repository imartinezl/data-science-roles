library(magrittr)

df_skills <- read.csv(file = "data/esco-bundle/v1.0.5/skills_en.csv", stringsAsFactors = F)
df_skill_groups <- read.csv(file = "data/esco-bundle/v1.0.5/skillGroups_en.csv", stringsAsFactors = F)
df_skill_hierarchy <- read.csv(file = "data/esco-bundle/v1.0.5/skillsHierarchy_en.csv", stringsAsFactors = F)
df_transversal_skills <- read.csv(file = "data/esco-bundle/v1.0.5/transversalSkillsCollection_en.csv", stringsAsFactors = F)

# df_concepts_schemes <- read.csv(file = "data/esco-bundle/v1.0.5/conceptSchemes_en.csv", stringsAsFactors = F)
df_ict_skills <- read.csv(file = "data/esco-bundle/v1.0.5/ictSkillsCollection_en.csv", stringsAsFactors = F)
df_isco_groups <- read.csv(file = "data/esco-bundle/v1.0.5/ISCOGroups_en.csv", stringsAsFactors = F)
df_isco_groups$preferredLabel[1]

df_language_skills <- read.csv(file = "data/esco-bundle/v1.0.5/languageSkillsCollection_en.csv", stringsAsFactors = F)
df_occupations <- read.csv(file = "data/esco-bundle/v1.0.5/occupations_en.csv", stringsAsFactors = F)


# Occupations
isco_group_selection <- df_isco_groups %>% 
  dplyr::mutate(code_char = as.character(code),
                code_filter = stringr::str_starts(code_char, "(25|212|133|1219)")) %>% 
  dplyr::filter(code_filter) %>%  # dplyr::pull(code_char) %>% unique
  # dplyr::mutate(code_pad = stringr::str_pad(string = code_char, width = 5, side = "right", pad = '0')) %>% 
  # dplyr::distinct(code_pad, .keep_all=T) %>% 
  # dplyr::select(code_char, preferredLabel)
  dplyr::pull(code_char)

df_occupations %>% 
  dplyr::filter(iscoGroup %in% isco_group_selection) %>% 
  dplyr::select(iscoGroup, preferredLabel, altLabels) %>% 
  dplyr::mutate(label = paste0(preferredLabel, "\n", altLabels),
                label = strsplit(label, "\n")) %>% 
  tidyr::unnest(label) %>% 
  dplyr::select(iscoGroup, label)

# Skills
# A. attitudes and values
# K. Knowledge
# L. Language skills => df_language_skills
# S. Skills => df_skill_groups represents the main index and df_skill_hierarchy represents the full tree (3 levels)

df_ict_skills %>% 
  dplyr::mutate(id=1:dplyr::n(),
                label = ifelse(nchar(altLabels)>0, paste0(preferredLabel, " \n ", altLabels), preferredLabel),
                label = ifelse(nchar(altLabels)>0, gsub("\n", "|", label), label),
                label = paste0(label, " | ", broaderConceptPT),
                label = strsplit(label, " | ", fixed = T)) %>% 
  tidyr::unnest(label) %>% 
  dplyr::select(id, label)

df_skills %>% 
  dplyr::mutate(id=1:dplyr::n(),
                label = paste0(preferredLabel, "\n", altLabels)) %>% 
  dplyr::mutate(label = strsplit(label, "\n")) %>% 
  tidyr::unnest(label) %>% 
  dplyr::select(id, label)

df_skill_groups
