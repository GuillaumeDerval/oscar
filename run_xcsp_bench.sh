#!/bin/bash

#SCALA=/home/oscar/scala-2.11.8/bin
#SBT=/home/oscar/sbt-3/bin

ReRun=false
Timeout=120
Memory=1000

Date=$(date +"%m-%d-%y")
VNum=`hg id -i`
BenchDir="data/xcsp3/xcspBench"
SolversDir="oscar-cp-xcsp3/src/main/scala/oscar/xcsp3/competition/solvers"
SolversRoot="oscar.xcsp3.competition.solvers"
SbtOutput=`sbt "project oscar-cp-xcsp3" "export runtime:fullClasspath"`
#echo $SbtOutput
CP=${SbtOutput##*$'\n'}
Out="XCSP-bench-results/$VNum"

SolversToRun="solvers_to_run.txt"
InstancesToRun="instances_to_run.txt"

echo -e "\n\n\n"
echo "Version Number -> $VNum"
echo "Benchmarks directory -> $BenchDir"
echo "Classpath -> $CP"
echo "Output directory -> $Out"
echo -e "\n"

run_search () {
    echo "Starting new search:"
#    echo "Classpath: $1"
#    echo "Output: $2"
    InstanceName=${3##*/}
    SolverName=${4##*.}
    echo "Instance: $InstanceName"
    echo "Solver: $SolverName"

#    scala -J-Xmx1g -cp $1 $4 $3 > "$2/${InstanceName%.*}-$SolverName"
    echo -e "\n\n\n"
}

export -f run_search

for s in `ls ${SolversDir}`; do
    if [ "${s: -6}" == ".scala" ]; then
        f=${s%%??????}
        echo "$SolversRoot.$f" >> ${SolversToRun}
    else
        echo "file $s is not a scala file!"
    fi
done

for i in `ls ${BenchDir}`; do
    if [ "${s: -4}" == ".xml" ]; then
        echo "$BenchDir/$i" >> ${InstancesToRun}
    else
        echo "file $s is not a xml file!"
    fi
done

echo -e "\nSolvers:"
cat ${SolversToRun}
echo -e "\nInstances:"
cat ${InstancesToRun}
echo -e "\n\n\n"

parallel --gnu --jobs 50% run_search ${CP} ${Out} :::: ${InstancesToRun} :::: ${SolversToRun}

rm ${InstancesToRun}
rm ${SolversToRun}

#scala -J-Xmx1g -cp ${CP} ${BenchRoot}.utils.HtmlReporter ${Out}
