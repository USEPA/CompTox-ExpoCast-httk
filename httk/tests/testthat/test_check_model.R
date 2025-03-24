testthat::test_that(
  "Check whether the force.human.clint.fup argument is appropriately handled by calls to check_model()",
  {
    rm(list = ls())
    
    human_chems <- get_cheminfo(info = "DTXSID", species = "Human", model = "pbtk",
                                default.to.human = FALSE,
                                suppress.messages = TRUE)
    
    rat_chems <- get_cheminfo(info = "DTXSID", species = "Rat", model = "pbtk",
                              default.to.human = FALSE,
                              suppress.messages = TRUE)
    
    common_chems <- intersect(rat_chems, human_chems) # Should work regardless
    diff_chems <- setdiff(human_chems, rat_chems) # Has human but no rat data
    
    # Must check that each set of chemicals is not empty
    
    if (length(common_chems) > 0) {
      # Uses rat values (also has human values)
      expect_no_error(
        parameterize_pbtk(
          dtxsid = common_chems[1],
          species = "Rat",
          default.to.human = FALSE,
          force.human.clint.fup = FALSE,
          suppress.messages = TRUE
        )
      )
      # Uses human values (but has rat values as well)
      expect_no_error(
        parameterize_pbtk(
          dtxsid = common_chems[1],
          species = "Rat",
          default.to.human = FALSE,
          force.human.clint.fup = TRUE,
          suppress.messages = TRUE
        )
      )
      # Both arguments are TRUE, uses human data
      expect_no_error(
        parameterize_pbtk(
          dtxsid = common_chems[1],
          species = "Rat",
          default.to.human = TRUE,
          force.human.clint.fup = TRUE,
          suppress.messages = TRUE
        )
      )
      # Default to human is TRUE, but this has no effect since rat data is available
      expect_no_error(
        parameterize_pbtk(
          dtxsid = common_chems[1],
          species = "Rat",
          default.to.human = TRUE,
          force.human.clint.fup = FALSE,
          suppress.messages = TRUE
        )
      )
      
    }
    
    if (length(diff_chems) > 0) {
      # Uses human values through default.to.human
      expect_no_error(
        parameterize_pbtk(
          dtxsid = diff_chems[1],
          species = "Rat",
          default.to.human = TRUE,
          force.human.clint.fup = FALSE,
          suppress.messages = TRUE
        ) 
      )
      
      # Should not error, but uses the human values
      expect_no_error(
        parameterize_pbtk(
          dtxsid = diff_chems[1],
          species = "Rat",
          default.to.human = FALSE,
          force.human.clint.fup = TRUE,
          suppress.messages = TRUE
        )
      )
      
      # Both are TRUE, uses human values
      expect_no_error(
        parameterize_pbtk(
          dtxsid = diff_chems[1],
          species = "Rat",
          default.to.human = TRUE,
          force.human.clint.fup = TRUE,
          suppress.messages = TRUE
        )
      )
      
      # Since both default.to.human and force.human.clint.fup are FALSE, should throw error
      expect_error(
        parameterize_pbtk(
          dtxsid = diff_chems[1],
          species = "Rat",
          default.to.human = FALSE,
          force.human.clint.fup = FALSE,
          suppress.messages = TRUE
        )
      )
      
    }
    
  }
)
