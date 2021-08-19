# Review 1 Clinical Neuroradiology
# trying to merge two databases to get devices types
# first database: evt.reg.id.xslx - containts EVT_Reg_v4 ids
# second database: devices.xslx - contains name of devices in columns

# write orbis.ids in database
#write.csv2(rb[!is.na(rb$orbis.id),c("orbis.id")], "C:/Sci/Stroke/EVT_Registry/orb.id.csv")
# merge sets
library("rlang")
db.id <- as_tibble(rb[!is.na(rb$orbis.id),c("orbis.id")])
db.dev <- as_tibble(read_xlsx("C:/Sci/Stroke/EVT_Registry/devices.xlsx", sheet = "Work_dev"))

db.comp <- left_join(db.id, db.dev, by = "orbis.id")

# will pivot by device name
db.dev <-  db.comp %>% tidyr::pivot_longer(cols = starts_with("dev_"), names_to = "device", values_to="count",
                             values_drop_na = T)

# gather similar devices
#db.dev$device <- factor(db.dev$device)
table(db.dev$device)
# pRESET
db.dev$device[grep("pRESET", db.dev$device)] <- "pRESET"
# Revive
db.dev$device[grep("Revive", db.dev$device)] <- "Revive"
# Solitaire
db.dev$device[grep("Solitaire", db.dev$device)] <- "Solitaire"
# Embo
db.dev$device[grep("embpo", db.dev$device)] <- "Embo"
db.dev$device[grep("Embo", db.dev$device)] <- "Embo"
db.dev$device[grep("embo", db.dev$device)] <- "Embo"
# Aperio
db.dev$device[grep("Aperio", db.dev$device)] <- "Aperio"
# TREVO
db.dev$device[grep("Trevo", db.dev$device)] <- "Trevo"
# ERIC
db.dev$device[grep("ERIC", db.dev$device)] <- "ERIC"

db.dev$device <- factor(db.dev$device)

# will show how many devices in each patient
db.dev.2 <- db.dev %>% group_by(device, orbis.id) %>% summarise(n.devices=n())
db.dev.2

# get catheter

# the same database
db.cath <- as_tibble(read_xlsx("C:/Sci/Stroke/EVT_Registry/devices.xlsx", sheet = "Work_cath"))
db.comp.cath <- left_join(db.id, db.cath, by = "orbis.id")

# will pivot by device name
db.dev.cath <-  db.comp.cath %>% tidyr::pivot_longer(cols = starts_with("cath_"), names_to = "cath", values_to="count",
                                           values_drop_na = T)
table(db.dev.cath$cath)
# Acandis
db.dev.cath$cath[grep("Acandis", db.dev.cath$cath)] <- "Acandis"
# Catalyst
db.dev.cath$cath[grep("Catalyst", db.dev.cath$cath)] <- "Catalyst"
# Penumbra
db.dev.cath$cath[grep("Penumbr", db.dev.cath$cath)] <- "Penumbra"
# SOFIA
db.dev.cath$cath[grep("SOF", db.dev.cath$cath)] <- "Sofia"

db.dev.cath$cath <- factor(db.dev.cath$cath)
table(db.dev.cath$cath)
db.cath.2 <- db.dev.cath %>% group_by(cath, orbis.id) %>% summarise(n.cath=n())
db.cath.2

# merge all to rb
# left join
rb2 <- left_join(rb, db.dev.2, by = "orbis.id")
rb2 <- left_join(rb2, db.cath.2, by = "orbis.id")

# join dev and cath together, make many x many crosstable
db.tog <- inner_join(db.dev.cath, db.dev, by="orbis.id")

db.tog2 <- db.tog[db.tog$device == "Aperio" | db.tog$device == "dev_Balt Catch 4x20 mm" | db.tog$device == "ERIC" |
  db.tog$device == "pRESET" | db.tog$device == "Revive" | db.tog$device == "Solitaire" | db.tog$device == "Trevo" | 
    db.tog$cath == "Catalyst" |  db.tog$cath == "cath_DAC " | db.tog$cath == "Penumbra" | db.tog$cath == "Sofia",]

# per device - intermediate catheter
lvls <- attributes(db.tog$device)$levels
lvls2 <- attributes(db.tog$cath)$levels
db.tog2 <- db.tog %>% filter(!as.integer(device) %in% c(2, 4, 5))
db.tog2 <- db.tog2 %>% filter(!as.integer(cath) %in%  c(1, 4, 5))

# per cobination Supplemental Table II
CrossTable(db.tog2$cath, db.tog2$device)

# part II, only stent retriever w/o aspiration (only with DAC)
db.tog2 <- db.tog2 %>% filter(!as.integer(cath) %in% c(1, 2, 4:7))
CrossTable(db.tog2$device)


# FUNCTION getting PNG
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
