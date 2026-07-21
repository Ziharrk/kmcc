#!/bin/sh
# Script to compile and generate the REPL executable with bin/kmcc_c
#
# Requirements:
# - bin/kmcc_c exists
# - repl/.cpm/packages contains all packages required by the REPL

KMCCHOME="$(dirname $(dirname $(realpath "$0")))"
cd $KMCCHOME

# generate main module:
MAINMOD=Main$$
MAINFILE=repl/$MAINMOD.curry
echo "module $MAINMOD where"        > $MAINFILE
echo "import KMCC.ReplConfig"      >> $MAINFILE
echo "main :: IO ()"               >> $MAINFILE
echo "main = KMCC.ReplConfig.main" >> $MAINFILE

# packages used by the REPL:
PACKAGES=`cd repl/.cpm/packages && ls`

# generate compile command:
CMD="$KMCCHOME/bin/kmcc_c -v1 -i$KMCCHOME/repl/src"
for P in $PACKAGES ; do
  CMD="$CMD -i$KMCCHOME/repl/.cpm/packages/$P/src"
done
CMD="$CMD -i$KMCCHOME/libs/src $MAINMOD"

# compile the KMCC repl and move executable to bin
echo "Executing in directory 'repl': $CMD"
cd repl && $CMD && mv -f $MAINMOD ../bin/kmcc_repl

# clean
rm -rf .curry
cd $KMCCHOME
rm -f $MAINFILE
