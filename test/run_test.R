# Run landscape tests 

require('RUnit')

source("src/landscape.R")
source("src/model.R")

test.suite <- defineTestSuite("tests",
                              dirs = "./test/unit",
                              testFileRegexp = '^.+\\landscape.R$')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
printJUnitProtocol(test.result,"./result/landscape.xml")

test.suite <- defineTestSuite("tests",
                              dirs = "./test/unit",
                              testFileRegexp = '^.+\\model.R$')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
printJUnitProtocol(test.result,"./result/model.xml")
