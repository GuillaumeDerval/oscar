#!/bin/sh


Date=$(date +"%m-%d-%y")
VNum=`hg id -i`
BenchDir=$"oscar-perf/src/main/scala/oscar/anytime/lns"
BenchRoot=$"oscar.anytime.lns"
#CP=$"./target/scala-2.11/classes/.:./oscar-cp/target/scala-2.11/classes/.:./oscar-algo/target/scala-2.11/classes/.:./oscar-modeling/target/scala-2.11/classes/.:./oscar-perf/target/scala-2.11/classes/."
#CP=`cat $BenchDir/classpath.txt`
SbtOutput=`sbt "project oscar-perf" "export runtime:fullClasspath"`
CP=${SbtOutput##*$'\n'}
ConfigsFile=$"$BenchDir/configs.txt"
Out=$"../ALNS-bench-results/$Date-$VNum"

echo "Date -> $Date"
echo "Version Number -> $VNum"
#echo "Benchmarks directory -> $BenchDir"
#echo "Benchmark root -> $BenchRoot"
#echo "Classpath -> $CP"
#echo "Configs file -> $ConfigsFile"
#echo "Output parameter -> $Out"

while read config; do
    echo "Config -> $config"
    for d in `ls $BenchDir/benchmarks`; do
        echo "Directory -> $d"
        for d2 in `ls $BenchDir/benchmarks/$d`; do
            if [ ${d2: -6} == ".scala" ]; then
                f=${d2%%??????}
                echo "File -> $f"
                scala -J-Xmx1g -cp $CP $BenchRoot.benchmarks.$d.$f $config --out $Out
            else
                echo "Directory -> $d2"
                for d3 in `ls $BenchDir/benchmarks/$d/$d2`; do
                    if [ ${d3: -6} == ".scala" ]; then
                        f=${d3%%??????}
                        echo "File -> $f"
                        scala -J-Xmx1g -cp $CP $BenchRoot.benchmarks.$d.$d2.$f $config --out $Out
                    fi
                done
            fi
        done
    done
done <$ConfigsFile

scala -J-Xmx1g -cp $CP $BenchRoot.utils.HtmlReporter $Out
