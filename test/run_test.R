# Run all tests 

require('RUnit')

source("landscape.R")
source("model.R")

test.suite <- defineTestSuite("tests",
                              dirs = file.path("../test/unit"),
                              testFileRegexp = '^.+\\.R$')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
printJUnitProtocol(test.result,"../result/result.xml")