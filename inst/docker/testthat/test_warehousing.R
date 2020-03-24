
library(DBI)
library(odbc)
 

# SQLite ------------------------------------------------------------------


test_that("Ramses on SQLite", {
  
  drug_data <- Ramses:::.prepare_drugs()
  conSQLite <- suppressWarnings(connect_db_local("ramses-db.sqlite"))
  
  expect_null(validate_prescriptions(drug_data$drug_rx))
  
  load_medications(conn = conSQLite, 
                   prescriptions = drug_data$drug_rx,
                   administrations = drug_data$drug_admins,
                   overwrite = TRUE)

  DBI::dbRemoveTable(conSQLite, "ramses_TC_edges", fail_if_missing = F)
  
  edges_table <- tbl(conSQLite, "drug_prescriptions_edges") %>% 
    # filter(patient_id %in% c("1374569474", "2775668346")) %>%
    transmute(id1 = from_id, id2 = to_id) %>% 
    compute(name = "ramses_TC_edges", temporary = F)
  
  DBI::dbExecute(conSQLite, "CREATE INDEX ramses_TC_edges_idx1 ON ramses_TC_edges (id1, id2);")
  DBI::dbExecute(conSQLite, "CREATE INDEX ramses_TC_edges_idx2 ON ramses_TC_edges (id2, id1);")
  tutu <- Ramses:::.do_TC_SQLITE(conn = conSQLite, edge_table = "ramses_TC_edges")
  tutu <- collect(filter(tutu, grp == 2))$id
  expect_equal(tutu, 1:2)
  
  DBI::dbDisconnect(conSQLite)
  file.remove("ramses-db.sqlite")
  
})



# PostgreSQL --------------------------------------------------------------



# conPostgreSQL <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
#                                 user = "user", password = "password", 
#                                 host = "db-postgres", dbname="RamsesDB")
# 
# conSQLServer <- dbConnect(
#   odbc::odbc(),
#   .connection_string = "Driver={ODBC Driver 17 for SQL Server}; Server=db-mssql,1433;Uid=Sa; Pwd=bidultrukCestLeurTruc!;", timeout = 10)



# test_that("Ramses on PosgreSQL", {
#   test_warehousing(conPostgreSQL, drug_data, overwrite = T)
#   
# })


# MS SQL Server -----------------------------------------------------------


# 
# test_that("Ramses on MS SQL Server", {
#   test_warehousing(conMS, drug_data, overwrite = T)
# 
# })



