# Run all tests 

require('RUnit')

source("src/landscape.R")
source("src/model.R")

test.suite <- defineTestSuite("tests",
                              dirs = "./test/unit",
                              testFileRegexp = '^.+\\.R$')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
printJUnitProtocol(test.result,"./result/result.xml")
