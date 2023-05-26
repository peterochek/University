#!/bin/bash

mkdir -p tmp

mainRepo="../../java-advanced-2023"
myRepo="../java-solutions"
myModules="info/kgeorgiy/ja/korolev"

javac -d tmp \
	--module-path $mainRepo/lib:$mainRepo/artifacts \
	--source-path $myRepo/$myModules \
	--source-path ../java-solutions \
	$myRepo/$myModules/implementor/Implementor.java

cd tmp || exit

jar --create \
  --file=../implementor.jar \
	--manifest=../MANIFEST.MF \
	$myModules/implementor/*.class \
	module-info.class

rm -rf ../tmp

