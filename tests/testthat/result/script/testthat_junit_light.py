#!/usr/bin/python

"""
    This script reduce the size of junit.xml create by testthat.
    With testthat, if in test we have several verification you 
    have several test with same name.
    The script take tests in same context and with same name and
    create one test.
    The test contain all failures and errors
    
    The script take as argument complete path of junit xml and 
    overwrite it
"""

import sys
import imp
try:
    imp.find_module('defusedxml')
    import defusedxml.ElementTree as dET
    import defusedxml.minidom as dmd
except ImportError:
    import xml.etree.ElementTree as dET
    import xml.dom.minidom as dmd

import xml.etree.ElementTree as ET

# Take junit.xml
testthat_junit = dET.parse(sys.argv[1])
# Create testsuites tag
tss = ET.Element("testsuites")
# read all testsuite
for testsuite in testthat_junit.findall("testsuite"):
    # copy testsuite
    ts = ET.SubElement(tss, "testsuite", testsuite.attrib)
    name_testcase = None
    # real all testcases
    for testcase in testsuite.findall("testcase"):
        tc_attrib = testcase.attrib
        # if already see this test, sum execution time
        if name_testcase == (tc_attrib["classname"] + tc_attrib["name"]):
            tc.attrib["time"] = str(float(tc_attrib["time"]) + float(tc.attrib["time"]))
        else:
            # if new testcase =, create new testcase tag
            tc = ET.SubElement(ts, "testcase", testcase.attrib)
            name_testcase = tc_attrib["classname"] + tc_attrib["name"]
        # copy all failure
        for failure in testcase.findall("failure"):
            tc.append(failure)
        # copy all error
        for err in testcase.findall("error"):
            tc.append(err)

# overwrite the file with ligth xml
node = dmd.parseString(ET.tostring(tss, encoding="utf-8"))
with open(sys.argv[1],"w") as f:
    f.write(node.toprettyxml(indent=" ", encoding="utf-8"))