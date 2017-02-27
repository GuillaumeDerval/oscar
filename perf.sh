#!/bin/sh
echo $PATH
#/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin
D1=$(date +"%m-%d-%y")
D2=$(date +%s)
C=`hg id -i`
SCALA=/usr/local/bin
JAVA_HOME=/home/oscar/jdk1.7.0_71/
sbt perf:test
sed -ne '/<testcase/p' oscar-perf/target/test-reports/oscar.cp.perf.AllAppsPerfTest.xml | sed 's/.*name="\(.*\)\$" time="\(.*\)\..*">.*$/\1.scala \2/' | sed "s/$/ $D2 $D1 $C/" >> perfresults.txt
scala analyze
