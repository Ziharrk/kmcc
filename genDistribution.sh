#!/bin/bash

# Script generating the external Curry.Compiler.Distribution implementation.

### get distribution information ###

# get name from cabal file
name=$(sed -n "s/^name: *//p" compiler/kmcc.cabal)

# get version from cabal file
version=$(sed -n "s/^version: *//p" compiler/kmcc.cabal)

# parse version
oIFS="$IFS"
IFS=.
read major minor revision <<< "$version"
IFS="$oIFS"

# get base version
baseVersion=$(cat lib/VERSION)

### update distribution file ###

# path to distribution file
distributionFile="lib/Curry/Compiler/Distribution.kmcc.hs"

# compiler name
compilerOld="P.error \"No implementation of curryCompiler\""
compilerNew="fromForeign \"$name\""

# major version
majVerOld="P.error \"No implementation of curryCompilerMajorVersion\""
majVerNew="fromForeign $major"

# minor version
minVerOld="P.error \"No implementation of curryCompilerMinorVersion\""
minVerNew="fromForeign $minor"

# revision version
revVerOld="P.error \"No implementation of curryCompilerRevisionVersion\""
revVerNew="fromForeign $revision"

# base version
baseVerOld="P.error \"No implementation of baseVersion\""
baseVerNew="fromForeign \"$baseVersion\""

# install directory
instDirOld="P.error \"No implementation of installDir\""
instDirNew="fromForeign \"$PWD\""

# replace functions
sed -i "s|$compilerOld|$compilerNew|" $distributionFile
sed -i "s|$majVerOld|$majVerNew|" $distributionFile
sed -i "s|$minVerOld|$minVerNew|" $distributionFile
sed -i "s|$revVerOld|$revVerNew|" $distributionFile
sed -i "s|$baseVerOld|$baseVerNew|" $distributionFile
sed -i "s|$instDirOld|$instDirNew|" $distributionFile