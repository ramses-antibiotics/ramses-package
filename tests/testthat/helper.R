expect_table <- function(x, table) {
  
  if (packageVersion("dbplyr") > "2.4.0") {
    expect_is(x, "dbplyr_table_path")
    expect_equal(as.character(x), table)
  } else {
    expect_is(x, "dbplyr_table_ident")
    expect_equal(vctrs::field(x, "table"), table)
  }

}