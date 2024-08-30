library(jsonlite)
library(openxlsx)
library(tidyverse)
library(readxl)
library(sf)
library(googlesheets4)

rm(list = ls())


#### take manually inputted IUCN realm and biome information from google sheets

my_sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1HgqxMdcz_YqxOgCIrapNC1h-cSGALLxUgu4PDrG6eoc/edit#gid=1380712147")

app_df <- read_sheet(my_sheet, sheet = "app_df")

iucn_class <- app_df %>% select(digitalObjectIdentifier, realm, biome)

iucn_class <- iucn_class %>% drop_na(digitalObjectIdentifier)




file_path <- "C:/Users/Lenovo/Dropbox/fao_lit/FAO_lit/open asfa json/"
json_files <- list.files(path = file_path, pattern = "\\.json$", full.names = TRUE)

data_frames <- lapply(json_files, function(file) {
  json_data <- fromJSON(file)
  
  #### JSON data reading
  
  # Check and assign authors or editors' name
  authors <- if(!is.null(json_data[]$authors) && 
                !is.null(json_data[]$authors$name) &&
                length(json_data[]$authors$name) >= 1 &&
                !is.na(json_data[]$authors$name[1])) {
    paste(json_data[]$authors$name, collapse = "; ")
  } else if(!is.null(json_data[]$editors) && 
            !is.null(json_data[]$editors$name)) {
    paste(json_data[]$editors$name, collapse = "; ")
  } else {
    NA
  }
  
  
  # Check and assign doi
  doi <- if(!is.null(json_data[]$dois) && 
            !is.null(json_data[]$dois$value) &&
            length(json_data[]$dois$value) >= 1 &&
            !is.na(json_data[]$dois$value[1])) {
    json_data[]$dois$value[1]
  } else if(!is.null(json_data[]$id) && 
            !is.null(json_data[]$id)) {
    paste(json_data[]$id, collapse = "; ")
  } else {
    NA
  }
  
  
  # Check and assign affiliation 
  affiliationNarrow <- if(!is.null(json_data[]$authors) && 
                          !is.null(json_data[]$authors$institutionName) &&
                          length(json_data[]$authors$institutionName) >= 1 &&
                          !is.na(json_data[]$authors$institutionName[1])) {
    json_data[]$authors$institutionName[1]
  } else if(!is.null(json_data[]$publishers) && 
            !is.null(json_data[]$publishers$name)) {
    paste(json_data[]$publishers$name, collapse = "; ")
  } else {
    NA
  }
  
  ecosystemClassification <- if(is.null(json_data$environmentalRegime$value) || length(json_data$environmentalRegime$value) == 0) {
    NA
  } else {
    paste(json_data$environmentalRegime$value, collapse = ", ")
  }
  
  taxonomicKeywords <- if(is.null(json_data$taxonomicKeywords$value) || length(json_data$taxonomicKeywords$value) == 0) {
    NA
  } else {
    paste(json_data$taxonomicKeywords$value, collapse = ", ")
  }
  
  geographicKeywords <- if(is.null(json_data$geographicKeywords$value) || length(json_data$geographicKeywords$value) == 0) {
    NA
  } else {
    paste(json_data$geographicKeywords$value, collapse = "; ")
  }
  
  marineRegionKeywords <- if(is.null(json_data$marineRegionKeywords$value) || length(json_data$marineRegionKeywords$value) == 0) {
    NA
  } else {
    paste(json_data$marineRegionKeywords$value, collapse = "; ")
  }
  
  
  publishers <- if(is.null(json_data$publishers$name) || length(json_data$publishers$name) == 0) {
    NA_character_  # Using NA_character_ to explicitly specify NA of type character
  } else {
    paste(json_data$publishers$name, collapse = ", ")
  }
  documentType <- if(is.null(json_data$type)) {
    NA_character_  # Assuming documentType is a character
  } else {
    json_data$type
  }
  urls <- if(is.null(json_data$urls$value) || length(json_data$urls$value) == 0) {
    NA_character_
  } else {
    paste(json_data$urls$value, collapse = "; ")
  }
  summaries <- if(is.null(json_data$summaries$value) || length(json_data$summaries$value) == 0) {
    NA_character_
  } else {
    paste(json_data$summaries$value, collapse = "; ")
  }
  keywords <- if(is.null(json_data$subjectKeywords$value) || length(json_data$subjectKeywords$value) == 0) {
    NA_character_
  } else {
    paste(json_data$subjectKeywords$value, collapse = "; ")
  }
data_list <- list(

recordNumber = unlist(json_data[]$id),

dates = paste(json_data$year[1]),

titles = json_data[]$titles$value,

authors = authors,

#affiliationNarrow =json_data[]$authors$institutionName[1],

affiliationNarrow = affiliationNarrow,

publishers = publishers, 

documentType = documentType,

#countryFirstauthor = json_data,

urls = urls,

summaries = summaries,

keywords = keywords,

taxonomicKeywords = taxonomicKeywords,

geographicKeywords = geographicKeywords,

marineRegionKeywords = marineRegionKeywords,

environmentalRegimes = paste(json_data$environmentalRegime, collapse = "; "),

#ecosystemClassification = paste(json_data$environmentalRegime, collapse = ", "),


# Concatenate all elements into a single string
ecosystemClassification = ecosystemClassification,

language = paste(json_data$languages[1]),

digitalObjectIdentifier = doi

)

as.data.frame(data_list, stringsAsFactors = FALSE)
})



openasfa_list <- do.call(rbind, data_frames)



# keep only one in cases of two languages: 
openasfa_list <- openasfa_list %>% distinct(recordNumber, .keep_all = TRUE)
openasfa_list$collection <- "openASFA"


openasfa_list$dates <- as.double(openasfa_list$dates)


my_sheet <- gs4_get("https://docs.google.com/spreadsheets/d/1HgqxMdcz_YqxOgCIrapNC1h-cSGALLxUgu4PDrG6eoc/edit#gid=1380712147")

sheet_write(openasfa_list, ss = "https://docs.google.com/spreadsheets/d/1HgqxMdcz_YqxOgCIrapNC1h-cSGALLxUgu4PDrG6eoc/edit#gid=1380712147",
            sheet = "openasfa_list")


##############################################
#SCOPUS list

scopus <- read.csv("C:/Users/Lenovo/Dropbox/fao_lit/FAO_lit/scopus list/scopus.csv")



# rename columns
scopus <- scopus %>% 
  rename(
    titles = Title,
    summaries = Abstract ,
    documentType = Document.Type,
    authors =  Authors,
    affiliationNarrow = Affiliations,
    #publishers = Publisher,
    keywords = Author.Keywords  , 
    #taxonomicKeywords = Index.Keywords, 
    languages = Language.of.Original.Document,
    digitalObjectIdentifier = DOI,
    dates = Year,
    publishers = Source.title,
    urls = Link
  )



scopus <- scopus %>%
  mutate(keywords = ifelse(is.na(Index.Keywords) | Index.Keywords == "",
                                    keywords,
                                    ifelse(is.na(keywords) | keywords == "", 
                                           Index.Keywords, 
                                           paste(keywords, Index.Keywords, sep = ", "))))

scopus <- scopus %>%
  mutate(digitalObjectIdentifier = ifelse(is.na(digitalObjectIdentifier) | digitalObjectIdentifier == "",
                           EID, digitalObjectIdentifier))


scopus$collection <- "scopus"


app_df <- bind_rows(openasfa_list, scopus)

################################# web of science




# sheet_write(scopus, ss = "https://docs.google.com/spreadsheets/d/1HgqxMdcz_YqxOgCIrapNC1h-cSGALLxUgu4PDrG6eoc/edit#gid=1380712147",
#             sheet = "scopus_list")
# 



#write.xlsx(openasfa_list, "C:/Users/Lenovo/Dropbox/fao_lit/FAO_lit/open asfa json/open_asfa_list.xlsx")



########################### merge with proquest list


folder_path <- "C:/Users/Lenovo/Dropbox/fao_lit/FAO_lit/proquest lists/"
excel_files <- list.files(folder_path, pattern = "\\.xls$", full.names = TRUE)
# Initialize an empty dataframe
combined_df <- data.frame()

# Loop through files and combine
for(file in excel_files) {
  # Read the Excel file
  temp_df <- read_excel(file)
  
  
  
  # Combine with the main dataframe
  combined_df <- bind_rows(combined_df, temp_df)
}

proquest_list <- bind_rows(lapply(excel_files, read_excel))


#sheet_write(proquest_list, ss = "https://docs.google.com/spreadsheets/d/1HgqxMdcz_YqxOgCIrapNC1h-cSGALLxUgu4PDrG6eoc/edit#gid=1380712147",
#            sheet = "proquest collection")

# rename columns
proquest_list <- proquest_list %>% 
  rename(
   titles = Title,
    summaries = Abstract ,
   documentType_detail  = documentType,
    documentType = ArticleType,
   authors =  Authors,
   affiliationNarrow = AuthorAffiliation,
   publishers = pubtitle,
   city = placeOfPublication  ,
   keywords = identifierKeywords  , 
#    taxonomicKeywords = subjectTerms  , 
   languages = language,
urls = URL
    
  )

proquest_list <- proquest_list %>%
  mutate(keywords = ifelse(is.na(subjectTerms) | subjectTerms == "",
                                    keywords,
                                    ifelse(is.na(keywords) | keywords == "", 
                                           subjectTerms, 
                                           paste(keywords, subjectTerms, sep = ", "))))



proquest_list <- proquest_list %>%
  mutate(urls = ifelse(is.na(urls) & is.na(digitalObjectIdentifier),
                       DocumentURL,
                       ifelse(is.na(urls) & !is.na(digitalObjectIdentifier),
                       paste0("https://doi.org/",digitalObjectIdentifier),
                                  urls)))

proquest_list$digitalObjectIdentifier <- ifelse(is.na(proquest_list$digitalObjectIdentifier), proquest_list$StoreId, proquest_list$digitalObjectIdentifier)


proquest_list$dates <- substr(proquest_list$elecPubDate, 0, 4)
proquest_list <- proquest_list %>%
  mutate(dates = ifelse(is.na(dates) | dates == "", 
                        str_extract(pubdate, "\\b\\d{4}\\b"),  # Extracts exactly four digits
                        dates))

proquest_list <- proquest_list %>%
  mutate(dates = as.integer(dates)) %>%
  filter(!is.na(dates) & dates >= 1000 & dates <= 9999) 


proquest_list$dates <- as.double(proquest_list$dates)
proquest_list$collection <- "proquest"

app_df <- bind_rows(app_df, proquest_list)


######################################
# merge with web of science list

folder_path <- "C:/Users/Lenovo/Dropbox/fao_lit/FAO_lit/web of science lists/"
excel_files <- list.files(folder_path, pattern = "\\.xls$", full.names = TRUE)
# Initialize an empty dataframe
combined_df <- data.frame()

# Loop through files and combine
for(file in excel_files) {
  # Read the Excel file
  temp_df <- read_excel(file)
  
  
  temp_df$Volume <- as.character(temp_df$Volume)
  temp_df$Supplement <- as.character(temp_df$Supplement)
  

    # Combine with the main dataframe
  combined_df <- bind_rows(combined_df, temp_df)
}

wos_list <- combined_df


# rename columns
wos_list <- wos_list %>% 
  rename(
    titles = `Article Title`,
    summaries = Abstract ,
    documentType = `Document Type`,
    authors =  Authors,
    #affiliationNarrow = AuthorAffiliation,
    publishers = `Source Title`,
    #city = placeOfPublication  ,
    keywords = `Author Keywords`, 
    #taxonomicKeywords = subjectTerms  , 
    languages = Language,
    digitalObjectIdentifier = DOI,
    dates = `Publication Year`,

    
  )

wos_list$publishers <- str_to_lower(wos_list$publishers)



wos_list <- wos_list[wos_list$digitalObjectIdentifier != 0, ]

wos_list <- wos_list %>% filter_all(any_vars(!is.na(.)))


wos_list$collection <- "web of science"

wos_list$urls <- paste("https://doi.org/", wos_list$digitalObjectIdentifier)


app_df <- bind_rows(app_df, wos_list)




######################################
#merge with manual list

manual_list <- read_sheet(my_sheet, sheet = "manual collection")

manual_list$recordNumber <- as.character(manual_list$recordNumber)
manual_list$collection <- "manually collected"

############ check manually inputted keywords = ; / ,

manual_list$digitalObjectIdentifier <- as.character(manual_list$digitalObjectIdentifier)
app_df <- bind_rows(manual_list, app_df)

app_df$documentType <- str_to_lower(app_df$documentType)
