#!/bin/bash
res=`echo $1 | swipl -q -s $2`
if [ "$res" == "true." ]; then
	exit 0
else
	exit 1
fi
