#!/bin/bash

inFile=$1
outDir=$2
passNum=$3
rerun=$4
if [[ $passNum == "" ]]; then
    passNum=0
fi
if [[ $rerun != "" ]]; then
    rerun="--rerun"
fi

counter=0

function foo {
    let "counter += 1"
    if [ $counter -ge $passNum ]; then
        makeHeader "$counter: $1"
        eval $2
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

foo "Filter raw logs" "gunzip -c $inFile | python3 filterPass.py | gzip > $outDir/py3-web_exec.jsonl.gz"

foo "Docker build" "docker build -t python-pipeline ."

foo "Run slicer" "gunzip -c $outDir/py3-web_exec.jsonl.gz | docker run -i python-pipeline /bin/bash -c 'python3 slicePass.py $rerun' | gzip > $outDir/sliced.jsonl.gz"

foo "Group by session" "gunzip -c $outDir/sliced.jsonl.gz | python3 groupAndSort.py $outDir/sessions"

foo "Make pairs" "python3 3-make-pairs.py $outDir/sessions $outDir/pairs"

foo "Filter pairs" "python3 4-run-on-single-lines.py $outDir/pairs $outDir"

# foo "Generate features (oneHot)" "stack exec -- generate-features --source $outDir/goodPairs.jsonl --oneHot --context --slice --size --out $outDir/foo"
foo "Generate features (categorical)" "stack exec -- generate-features --source $outDir/goodPairs.jsonl --context --slice --size --out $outDir/foo"

foo "Remove outliers" "python3 removeOutliers.py $outDir/foo/blah+context+slice+size $outDir/outliersRemoved"
