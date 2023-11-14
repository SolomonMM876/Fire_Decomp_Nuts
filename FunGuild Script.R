#Function to read (parse FunGuild) which I obtained after a search in google
parse_funguild <- function(url = 'http://www.stbates.org/funguild_db.php', tax_name = TRUE){
  # require(XML)
  # require(jsonlite)
  # require(RCurl)
  ## Parse data
  tmp <- XML::htmlParse(url)
  tmp <- XML::xpathSApply(doc = tmp, path = "//body", fun = XML::xmlValue)
  ## Read url and convert to data.frame
  db <- jsonlite::fromJSON(txt=tmp)
  ## Remove IDs
  db$`_id` <- NULL
  if(tax_name == TRUE){
    ## Code legend
    ## Taxon Level: A numeral corresponding the correct taxonomic level for the taxon
    taxons <- c(
      "keyword",                                                       # 0
      "Phylum", "Subphylum", "Class", "Subclass", "Order", "Suborder", # 3:8
      "Family", "Subfamily", "Tribe", "Subtribe", "Genus",             # 9:13
      "Subgenus", "Section", "Subsection", "Series", "Subseries",      # 15:19
      "Species", "Subspecies", "Variety", "Subvariety", "Form",        # 20:24
      "Subform", "Form Species")
    ## Table with coding
    taxmatch <- data.frame(
      TaxID = c(0, 3:13, 15:26),
      Taxon = factor(taxons, levels = taxons))
    ## Match taxon codes
    db$taxonomicLevel <- taxmatch[match(x = db$taxonomicLevel, table = taxmatch$TaxID), "Taxon"]
  }
  # remove rows with missing data
  # which(
  # 	with(db, trophicMode == "NULL" & guild == "NULL" & growthForm == "NULL" & trait == "NULL" & notes == "NULL")
  # 	)
  ## Add database dump date as attributes to the result
  attr(db, "DownloadDate") <- date()
  return(db)
}
#FunGuild
#install.packages(c("httr", "jsonlite", "lubridate"))
library(httr)
library(jsonlite)
library(lubridate)
url  <- "http://www.stbates.org/funguild_db.php"
FunGuildData_org<-parse_funguild(url)


Mycorhiz<-FunGuildData_org%>%
  subset(guild %in% c('Arbuscular Mycorrhizal','Ectomycorrhizal'))%>%
  subset(taxonomicLevel== 'Genus')
