
library(DBI)
library(odbc)
 

# SQLite ------------------------------------------------------------------


test_that("Ramses on SQLite", {
  
  drug_data <- Ramses:::.prepare_example_drug_records()
  conSQLite <- suppressWarnings(connect_db_local("ramses-db.sqlite"))
  
  expect_null(validate_prescriptions(drug_data$drug_rx))
  
  load_medications(conn = conSQLite, 
                   prescriptions = drug_data$drug_rx,
                   administrations = drug_data$drug_admins,
                   overwrite = TRUE)

  # tutu <- tbl(conSQlite,
  #             "drug_prescriptions") %>% 
  #   filter(grp == 2))$id
  # expect_equal(tutu, 1:2)
  
  DBI::dbDisconnect(conSQLite)
  file.remove("ramses-db.sqlite")
  
})


test_that("SQLite does transitive closure", {
  
  test_edges <- tibble(
    id1 = as.character(c(1,1,2,5,6,7)),
    id2 = as.character(c(2,3,4,6,7,8))
  )
  
  test_solution <- tibble(
    id =  as.integer(c(1,2,3,4,5,6,7,8)),
    grp = as.integer(c(1,1,1,1,5,5,5,5))
  )
  
  conSQLite <- suppressWarnings(
    connect_db_local("test.sqlite"))
  
  dbplyr::db_copy_to(conSQLite,
                     "ramses_test_edges",
                     test_edges,
                     overwrite = T,
                     temporary = F)
  
  test_output <- Ramses:::.run_transitive_closure.SQLiteConnection(
    conSQLite,"ramses_test_edges") %>% 
    dplyr::select(id, grp) %>% 
    dplyr::arrange(id) %>% 
    dplyr::collect()
  
  expect_equal(test_output,
               test_solution)
  DBI::dbDisconnect(conSQLite)
  file.remove("test.sqlite")  
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



