app <- ShinyDriver$new("../../")
app$snapshotInit("test_gw_district")

app$setInputs(maintabs = "prepare")
app$setInputs(usermode = "files")
app$setInputs(exampleUpload = "Groundwater district")
app$snapshot()
app$setInputs(maintabs = "results")
app$snapshot()
