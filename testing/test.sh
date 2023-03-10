#!/bin/sh
# Shell script to test some examples

CURRYHOME=$(dirname $(dirname $(realpath $0)))
CURRYBIN=$CURRYHOME/bin

PATH=$CURRYBIN:$PATH
export PATH

# Clean old stuff:
clean() {
  /bin/rm -rf .curry
  for P in $PROGRAMS ; do
    /bin/rm -f $P
  done
}

run() {
  for P in $PROGRAMS ; do
    echo "Executing: $P"
    #echo kmcc -n -q $KMCCOPTS :load $P :eval main :q
    kmcc -n -q $KMCCOPTS :load $P :eval main :q
  done
}

testall() {
  TESTRESULT=$1
  echo "TESTING ALL PROGRAMS WITH OPTIONS: $KMCCOPTS"
  LOGFILE=XXX$$
  clean
  run | tee $LOGFILE
  clean
  
  # Check differences:
  DIFF=diff$$
  diff $TESTRESULT $LOGFILE > $DIFF
  if [ "`cat $DIFF`" = "" ] ; then
    echo
    echo "REGRESSION TEST SUCCESSFULLY EXECUTED!"
    /bin/rm -f $LOGFILETEE $LOGFILE $DIFF
  else
    echo
    echo "DIFFERENCES IN REGRESSION TEST OCCURRED:"
    cat $DIFF
    /bin/rm -f $DIFF $LOGFILETEE
    /bin/mv -f $LOGFILE LOGFILE
    echo "Test output saved in file 'LOGFILE'."
    exit 1
  fi
}

# Tests where strategy is not relevant:
PROGRAMS="Fac FreeBool Higher Last InfList PermSort PermSortInt Rev Xor Zip"
KMCCOPTS=":set dfs"
testall TESTANYSTRAT.txt
KMCCOPTS=":set bfs"
testall TESTANYSTRAT.txt
KMCCOPTS=":set fs"
testall TESTANYSTRAT.txt

# Test with DFS strategy (to check fixed order of results):
PROGRAMS="CaseLiteral Colormap ColormapFree Data Half NonDet Perm PullTabOwnerTask"
KMCCOPTS=":set dfs"
testall TESTDFS.txt

#  # Tests where BFS strategy is relevant:
#  PROGRAMS="NDNums Strategy"
#  KMCCOPTS=":set bfs :set +first"
#  testall TESTBFS.txt
 
#  # Tests where fair strategy is relevant:
#  PROGRAMS="FairSearch"
#  KMCCOPTS="-:set fs :set +first"
#  testall TESTFS.txt
 
# Tests with functional patterns:
PROGRAMS="Dutch FunPatsLast FunPatsPali FunPatsExpSimp FunPatsExpVar"
KMCCOPTS=":set dfs"
testall TESTFUNPATS.txt
KMCCOPTS=":set bfs"
testall TESTFUNPATS.txt
KMCCOPTS=":set fs"
testall TESTFUNPATS.txt
