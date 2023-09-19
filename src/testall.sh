#!/bin/bash

function run_test {
    diff=""
    ./HeBGB "./tests/"$1".hebgb" > /tmp/HeBGB_output.txt
    if [ $? -ne 0 ]
    then
	diff=$(diff /tmp/HeBGB_output.txt "./tests/"$1".gold.txt")
    else
	diff=$(./a.out | diff - "./tests/"$1".gold.txt")
    fi

    if [ "$diff" == "" ]
    then
        echo "PASSED "$1
        ((passed++))
    else
        echo "FAILED "$1
        echo "  "$diff
    fi
}

passed=0
total=$(ls ./tests/*.hebgb | wc -l)
for test in ./tests/*.hebgb; do
    test_name=$(basename $test)
    run_test "${test_name%.*}"
done

#rm ./tests/*.ll ./tests/*.s
echo "TOTAL: ["$passed"/"$total"] PASSED"
