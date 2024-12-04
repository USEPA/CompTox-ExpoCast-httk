
# Testing check_model argument forcing human clint and fup
rm(list = ls())

devtools::load_all("httk")

human_chems <- get_cheminfo(info = "DTXSID", species = "Human", model = "pbtk",
                            default.to.human = FALSE,
                            suppress.messages = TRUE)

rat_chems <- get_cheminfo(info = "DTXSID", species = "Rat", model = "pbtk",
                          default.to.human = FALSE,
                          suppress.messages = TRUE)

common_chems <- intersect(rat_chems, human_chems) # Should work regardless
diff_chems <- setdiff(human_chems, rat_chems) # Has human but no rat data

# Uses rat values (also has human values)
parameterize_pbtk(
  dtxsid = common_chems[1],
  species = "Rat",
  default.to.human = FALSE,
  force.human.clint.fup = FALSE,
  suppress.messages = TRUE
)
# Uses human values (but has rat values as well)
parameterize_pbtk(
  dtxsid = common_chems[1],
  species = "Rat",
  default.to.human = FALSE,
  force.human.clint.fup = TRUE,
  suppress.messages = TRUE
)

# Uses human values through default.to.human
parameterize_pbtk(
  dtxsid = diff_chems[1],
  species = "Rat",
  default.to.human = TRUE,
  force.human.clint.fup = FALSE,
  suppress.messages = TRUE
)

# Should not error, but uses the human values
parameterize_pbtk(
  dtxsid = diff_chems[1],
  species = "Rat",
  default.to.human = FALSE,
  force.human.clint.fup = TRUE,
  suppress.messages = TRUE
)
