#!/bin/sh
echo $PATH
#/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin
cp target/oscar.jar perf/oscar.jar
cd perf
rm -f *.class
D1=$(date +"%m-%d-%y")
D2=$(date +%s)
C=`hg id -i`
SCALA=/home/pschaus/scala-2.10.0/bin
echo "scala10 directory $SCALA"
for f in `ls -1 *.scala`; do
  echo "File -> $f"
  f2=${f%%??????}
  echo "class file: $f2"
  $SCALA/scalac  -cp oscar.jar $f
  SECONDS=0; $SCALA/scala -J-Xmx1g -cp oscar.jar:.  $f2 ; echo "that took approximately $SECONDS seconds"
  echo $f $SECONDS $D2 $D1 $C >> ../perfresults.txt
  echo $f $SECONDS $D $C
done
cd ..
scala analyze
