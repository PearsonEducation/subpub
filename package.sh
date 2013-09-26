#!/bin/sh


./rebar compile

rm -vfR dist
mkdir -vp dist/prospero

cp -vR deploy deps ebin include priv sbin src www dist/prospero
mv dist/prospero/deploy/environmental/EnvVars.sh dist/prospero/deploy/environmental/EnvVars.sh.sample
