#!/usr/bin/env sh

egrep "^version" ./tip.cabal | awk '{print $2}'
