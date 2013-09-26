#!/bin/sh

HOME=/root
export HOME
export ERL\_LIBS="deps/erlang:$ERL\_LIBS"

./clean

mkdir tmp

#Install erlang deps
PREVPWD=`pwd`
cd deps/erlang


cd $PREVPWD

git rm dist/*.tar.gz
ERL_LIBS=$ERL_LIBS:$PREVPWD/tmp
export ERL_LIBS

./package.sh

#Make a tarball...
cd dist
mv prospero prospero-${BUILD_TAG}
tar cvf prospero-${BUILD_TAG}.tar prospero-${BUILD_TAG}
gzip prospero-${BUILD_TAG}.tar

echo "ARTIFACT=prospero-${BUILD_TAG}.tar.gz" >> info.include
echo "FOLDER=prospero-${BUILD_TAG}" >> info.include
echo "PROSPERO_BUILD_NUMBER=${BUILD_TAG}" >> info.include

git checkout master
git pull origin master
git add -f prospero-${BUILD_TAG}.tar.gz
git commit -m "prospero-${BUILD_TAG}.tar.gz"
git push origin master

cd ..
