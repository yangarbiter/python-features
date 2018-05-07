#!/bin/bash

inDir=$1
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

foo "Filter raw logs" "python3 filterPass.py $inDir $outDir"

foo "ANF transformation" "python3 dispatch.py anf $2/py3-web_exec"

foo "Docker build" "docker build -t python-pipeline ."
foo "Run slicer" "docker run -v $(realpath $outDir/py3-web_exec/updated-with-anf):/app/data python-pipeline slice"

foo "Make pairs" "python3 3-make-pairs.py $outDir/py3-web_exec/updated-with-anf/sliced"

foo "Filter pairs" "python3 4-run-on-single-lines.py $outDir/py3-web_exec/updated-with-anf/sliced/pairs $outDir"

foo "Generate features" "stack exec -- generate-features --source $outDir/goodPairs.jsonl --features op+context --out $outDir/foo"

foo "Filter results" "python3 twoSided.py $outDir/foo/op+context $outDir"