#!/bin/bash

path="info.kgeorgiy.java.advanced.implementor"
pathSlash="${path//.//}"
mainRepoModules="../../java-advanced-2023/modules/${path}/${pathSlash}"

javadoc -d javadoc \
  -private \
  -link https://docs.oracle.com/en/java/javase/17/docs/api/ \
  ../java-solutions/info/kgeorgiy/ja/korolev/implementor/*.java \
  $mainRepoModules/Impler.java \
  $mainRepoModules/JarImpler.java \
  $mainRepoModules/ImplerException.java
