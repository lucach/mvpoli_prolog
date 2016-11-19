#!/bin/bash
while read test
do
	echo "$test"
        res=`echo $test | swipl -q -s mvpoli.pl`
	if [ "$res" == "true." ]; then
		exit 0
	else
		exit 1
	fi
done < tests.pl
