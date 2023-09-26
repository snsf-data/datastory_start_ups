# This script produces all the files required to deploy an SNSF data story.
#
# Data story template: https://github.com/snsf-data/datastory_template
#
# By running this file, the following components of a data story are generated
# and stored in the output directory:
#
# 1) a HTML file (self-contained), which contains all visualizations and
#   images in encoded form, one for every specified language.
# 2) one file "metadata.json", which contains the metadata essential for
#   the story (including all language versions in one file).
#
# The files are stored in output/xxx, where xxx stands for the title of the
# data story in English, how it can also be used for the vanity URL to the
# story, that means: no special characters, only lowercase.

# Unique name of this data story in English (all lowercase, underscore as
# space, no special characters etc.)
# -> Don't put "datastory" at the start or end!
datastory_name <- "start_ups"

# Language-specific names, do adapt! (used for vanity URL! Format: all
# lowercase, minus as white-space (!) and no special characters, no special
# characters etc.)
# -> Don't put "datastory" at the start or end!
datastory_name_en <- "start-ups"
datastory_name_de <- "start-ups"
datastory_name_fr <- "start-ups"

# English title and lead of the story (Mandatory, even if no EN version)
title_en <- "Start-ups: what is the contribution of research funding?"
lead_en <-
    paste0(
      "A study has shown: with their research funding, the SNSF and ",
      "Innosuisse facilitate the founding of start-ups. Many business ideas ",
      "originate at higher education institutions and other research centres. ",
      "This benefits society as a whole."
    )

# German title and lead of the story (Mandatory, even if no DE version)
title_de <- "Start-up-Unternehmen: Welchen Beitrag leistet die Forschungsförderung?"
lead_de <-
    paste0(
      "Eine Studie belegt: Mit ihrer Forschungsförderung begünstigen der SNF ",
      "und Innosuisse die Gründung von Start-ups. Viele Geschäftsideen ",
      "entstehen an den Hochschulen und anderen Forschungsinstitutionen. ",
      "Davon profitiert die Gesellschaft."
    )

# French title and lead of the story (Mandatory, even if no FR version)
title_fr <- "Importance du soutien à la recherche pour la création de start-up"
lead_fr <-
    paste0(
      "Une étude le prouve : le FNS et Innosuisse favorisent la création de ",
      "start-up en soutenant la recherche. De nombreuses idées commerciales ",
      "naissent dans les hautes écoles et d’autres institutions de recherche. ",
      "Toute la société en profite."
    )

# Contact persons, always (first name + last name)
contact_person <- c("Manuel Klaus")
# Mail address to be displayed as contact persons, put "datastories@snf.ch" for
# every name of a contact person listed above.
contact_person_mail <- c("datastories@snf.ch")
# One of the following categories:  "standard", "briefing", "techreport",
# "policybrief", "flagship", "figure". Category descriptions are
datastory_category <- "standard"
# Date, after which the story should be published. Stories not displayed if the
# date lies in the future.
publication_date <- "2023-09-27 04:00:00"
# Available language versions in lowercase, possible: "en", "de", "fr".
languages <- c("en", "de", "fr")
# Whether this story should be a "Feature Story" story
feature_story <- FALSE
# DOI URL of the story (optional) -> e.g. must be an URL, is used as link!
# e.g. https://doi.org/10.46446/datastory.leaky-pipeline
doi_url <- paste0("https://doi.org/10.46446/datastory.", datastory_name_en)
# URL to Github page (optional)
github_url <- paste0("https://github.com/snsf-data/datastory_", datastory_name)
# Put Tag IDs here. Only choose already existing tags.
tags_ids <- c(
  100, # Bridge
  110, # Innosuisse
  160, # value of research
  260  #research and development
)

# IMPORTANT: Put a title image (as .jpg) into the output directory.
# example: "output/datastory-template.jpg"

# Install snf.datastory package if not available, otherwise load it
if (!require("snf.datastory")) {
  if (!require("devtools")) {
    install.packages("devtools")
    library(devtools)
  }
  install_github("snsf-data/snf.datastory")
  library(snf.datastory)
}

# Load packages
library(tidyverse)
library(scales)
library(conflicted)
library(glue)
library(jsonlite)
library(here)

# Conflict preferences
conflict_prefer("filter", "dplyr")

# Function to validate a mandatory parameter value
is_valid <- function(param_value) {
  if (is.null(param_value)) {
    return(FALSE)
  }
  if (is.na(param_value)) {
    return(FALSE)
  }
  if (str_trim(param_value) == "") {
    return(FALSE)
  }
  return(TRUE)
}

all_params <-
  c(
    "datastory_name", "title_en", "title_de", "title_fr", "datastory_category",
    "publication_date", "languages", "lead_en", "lead_de", "lead_fr", "doi_url",
    "github_url", "tags_ids"
  )

are_params_valid <-
  c(
    !is_valid(datastory_name),
    !is_valid(title_en),
    !is_valid(title_de),
    !is_valid(title_fr),
    !is_valid(datastory_category),
    !is_valid(publication_date),
    mean(c("en", "de", "fr") %in% (languages)) < 1,
    !is_valid(lead_en),
    !is_valid(lead_de),
    !is_valid(lead_fr),
    !is_valid(doi_url),
    !is_valid(github_url),
    length(tags_ids) == 0
  )

# Validate parameters and throw error message when not correctly filled
if (any(are_params_valid)) {
  stop(
    paste0(
      "\nIncorrect value for the following mandatory metadata values:\n",
      "- ", paste0(all_params[are_params_valid], collapse = "\n- ")
    )
  )
}

# Check that the github repo is not
if (github_url == "https://github.com/snsf-data/datastory_template_datastory") {
  stop(
    "\nThe link to the Github repository corresponds to the placeholder from ",
    "the template. Please enter a valid link before continuing."
  )
}

# Check titles/leads length and throw an error when they are too long
too_long <-
  c(
    c(nchar(title_en), nchar(title_de), nchar(title_fr)) > 90,
    c(nchar(lead_en), nchar(lead_de), nchar(lead_fr)) > 240
  )

which_too_long <-
  c("title_en", "title_de", "title_fr", "lead_en", "lead_de", "lead_fr")[too_long]

if (any(too_long)) {
  stop(
    paste0(
      "\nTitle and leads should not exceed 230 and 90 characters, respecrively. ",
      "The following parameters are too long:\n",
      "- ", paste0(which_too_long, collapse = "\n- ")
    )
  )
}

# Check whether an image exists and throw a warning if there is a png or no
# image at all.
if (length(grep("jpg$", list.files(here("output", datastory_name)))) == 0){
  warning(
    "It seems like there is no title image in 'output/", datastory_name, "'."
  )
}
if (length(grep("png$", list.files(here("output", datastory_name)))) != 0){
  warning(
    paste0(
      "It seems like the title image in 'output/", datastory_name, "' ",
      "is a .png file. Only .jpg file should be provided.")
  )
}

# Create output directory in main directory
if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}

# Create story directory in output directory
if (!dir.exists(here("output", datastory_name))) {
  dir.create(here("output", datastory_name))
}

# Create a JSON file with the metadata and save it in the output directory
tibble(
  title_en = title_en,
  title_de = title_de,
  title_fr = title_fr,
  author = paste(contact_person, collapse = ";"),
  datastory_category = datastory_category,
  publication_date = publication_date,
  languages = paste(languages, collapse = ";"),
  short_desc_en = lead_en,
  short_desc_de = lead_de,
  short_desc_fr = lead_fr,
  tags = paste(paste0("T", tags_ids, "T"), collapse = ","),
  author_url = paste(contact_person_mail, collapse = ";"),
  top_story = feature_story,
  github_url = github_url,
  doi = doi_url
) %>%
  toJSON() %>%
  write_lines(here("output", datastory_name, "metadata.json"))

# Knit HTML output for each language version
for (idx in seq_len(length(languages))) {
  current_lang <- languages[idx]
  filename <- paste0(
    str_replace_all(
      get(paste0("datastory_name_", current_lang)), "_", "-"
    ),
    "-", current_lang, ".html"
  )
  output_file <- here(
    "output", datastory_name,
    filename
  )
  print(paste0("Generating output for ", current_lang, " version..."))
  quarto::quarto_render(
    input = here(paste0(current_lang, ".qmd")),
    output_file = filename,
    execute_params = list(
      title = get(paste0("title_", current_lang)),
      publication_date = publication_date,
      github_url = github_url,
      doi = doi_url,
      lang = current_lang
    )
  )
  fs::file_move(path = here(filename),new_path = output_file)
}
