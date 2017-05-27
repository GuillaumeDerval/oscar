#!/bin/bash

#SCALA=/home/oscar/scala-2.11.8/bin
SBT_HOME=/etinfo/users2/cthomas/sbt/bin
BIN=/etinfo/users2/cthomas/bin

ReRun="false"
Timeout=240
Memory=1000

Date=$(date +"%m-%d-%y")
VNum=`hg id -i`
BenchDir="data/xcsp3/xcspBench"
SolversDir="oscar-cp-xcsp3/src/main/scala/oscar/xcsp3/competition/solvers"
SolversRoot="oscar.xcsp3.competition.solvers"
SbtOutput=`$SBT_HOME/sbt "project oscar-cp-xcsp3" "export runtime:fullClasspath"`
#echo $SbtOutput
CP=${SbtOutput##*$'\n'}
Out="XCSP-bench-results"

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
    InstanceName=${6##*/}
    SolverName=${7##*.}
    echo "Instance: $InstanceName"
    echo "Solver: $SolverName"

    OutPath="${2}/${3}/${InstanceName%.*}-${SolverName}.txt"

    if [ -e "$OutPath" ]
    then
        echo "$OutPath already exists!"
        if [ "$ReRun" = "true" ]
        then
            echo "Re-running bench"
            echo "c $InstanceName" > $OutPath
            echo "c $SolverName" >> $OutPath
            scala -J-Xmx${5}m -cp $1 $7 --timelimit $4 $6 >> $OutPath
        fi
    else
        echo "Running bench"
        echo "c $InstanceName" > $OutPath
        echo "c $SolverName" >> $OutPath
        scala -J-Xmx${5}m -cp $1 $7 --timelimit $4 $6 >> $OutPath
    fi

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
    echo $i
    if [ "${i: -4}" == ".xml" ]; then
        echo "$BenchDir/$i" >> ${InstancesToRun}
    else
        echo "file $i is not a xml file!"
    fi
done

echo -e "\nSolvers:"
cat ${SolversToRun}
echo -e "\nInstances:"
cat ${InstancesToRun}
echo -e "\n\n\n"
mk
if [ ! -e "${Out}/${VNum}" ]
then
    echo "Creating directory: ${Out}/${VNum}"
    mkdir "${Out}/${VNum}"
fi

$BIN/parallel --gnu --jobs 50% run_search ${CP} ${Out} ${VNum} ${Timeout} ${Memory} :::: ${InstancesToRun} :::: ${SolversToRun}

rm ${InstancesToRun}
rm ${SolversToRun}

#scala -J-Xmx1g -cp ${CP} oscar.xcsp3.competition.Reporter ${Out}
