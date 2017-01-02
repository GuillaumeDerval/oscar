#!/bin/sh
echo $PATH
#/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin
rm perf/*.jar
cd perf
rm -f *.class
D1=$(date +"%m-%d-%y")
D2=$(date +%s)
C=`hg id -i`
#JAVA_HOME=/usr/local/bin
SCALA=/usr/local/bin
echo "scala10 directory $SCALA"
for f in `ls -1 *.scala`; do
  echo "File -> $f"
  f2=${f%%??????}
  echo "class file: $f2"
  $SCALA/scalac -cp "/Users/pschaus/Documents/IdeaProjects/oscar/target/pack/lib/*" $f
  echo "init time $SECONDS"
  SECONDS=0; $SCALA/scala -encoding Cp1252 -J-Xmx1g -cp "/home/oscar/.jenkins/workspace/oscar-dev/target/pack/lib/*":. $f2 ;
  echo "that took approximately $SECONDS seconds"
  echo $f $SECONDS $D2 $D1 $C >> ../perfresults.txt
  echo $f $SECONDS $D $C
done
cd ..
JAVA_HOME=/home/oscar/jdk1.7.0_71/
scala analyze


