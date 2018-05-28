#!/bin/bash

inFile=$1
outDir=$2
passNum=$3
if [[ $passNum == "" ]]; then
    passNum=0
fi

counter=0

function foo {
    let "counter += 1"
    if [ $counter -ge $passNum ]; then
        makeHeader "$counter: $1"
        $2
        assertOk
    else
        echo "skipping $1"
    fi
}

function assertOk {
    rc=$?
    if [[ $rc != 0 ]]; then
        echo "Last command did not exit normally; exiting"
        exit $rc
    fi
}

function makeHeader {
    echo "----------------------------------"
    echo $1
    echo "----------------------------------"
}

foo "Create output folder" "mkdir $outDir"

foo "Stack build" "stack build"

# foo "Filter raw logs" "gunzip -c $inFile | python3 filterPass.py $outDir"
let "counter += 1"
if [ $counter -ge $passNum ]; then
    makeHeader "$counter: Filter raw logs"
    gunzip -c $inFile | python3 filterPass.py $outDir
    assertOk
else
    echo "skipping Filter raw logs"
fi

foo "ANF transformation" "python3 anfPass.py $outDir/py3-web_exec $outDir/updated-with-anf"

foo "Docker build" "docker build -t python-pipeline ."
foo "Run slicer" "docker run -v $(realpath $outDir/updated-with-anf):/app/data -v $(realpath $outDir/sliced):/app/sliced python-pipeline data sliced"

foo "Make pairs" "python3 3-make-pairs.py $outDir/sliced $outDir/pairs"

foo "Filter pairs" "python3 4-run-on-single-lines.py $outDir/pairs $outDir"

# foo "Generate features (oneHot)" "stack exec -- generate-features --source $outDir/goodPairs.jsonl --oneHot --context --slice --size --out $outDir/foo"
foo "Generate features (categorical)" "stack exec -- generate-features --source $outDir/goodPairs.jsonl --context --slice --size --out $outDir/foo"

# foo "Filter results" "python3 twoSided.py $outDir/foo/op+context+slice $outDir"