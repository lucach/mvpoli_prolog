#!/bin/bash
lines=`cat tests.pl | wc -l`
echo '<testsuite tests="'$lines'">' > report.xml
i=1
while read test
do
	if [[ "$test" != "" ]] && [[ "$test" != %* ]]; then
		res=`echo "$test" | swipl -q -s mvpoli.pl`
		if [ "$res" != "false." ]; then
                	echo '<testcase classname="test'$i'" name="test'$i'"/>' >> report.xml
        	else
                	echo '<testcase classname="test'$i'" name="test'$i'">' >> report.xml
			echo '<failure type="Fail"> '$res' </failure>' >> report.xml
			echo '</testcase>' >> report.xml
        	fi
	fi
	i=$((i+1))

done < tests.pl
echo '</testsuite>' >> report.xml

