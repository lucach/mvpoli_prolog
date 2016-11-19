echo '' > report.xml
echo '<testsuite tests="3">' >> report.xml
echo '    <testcase classname="foo1" name="ASuccessfulTest"/>' >> report.xml
echo '	    <testcase classname="foo3" name="AFailingTest">' >> report.xml
echo '<failure type="NotEnoughFoo"> details about failure </failure>' >> report.xml
echo '	        </testcase>' >> report.xml
echo '	</testsuite>' >> report.xml

