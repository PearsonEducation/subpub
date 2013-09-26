#!/bin/bash
# Installs erlang to /opt/erlang-$ERLANG_VERSION and symlinks /opt/erlang and then /opt/erlang/bin/* to /usr/bin (so that it is on everybody's path)

ERLANG_VERSION=R15B

ORIG_PWD=`pwd`

sudo apt-get update
sudo apt-get upgrade
sudo apt-get install m4 build-essential libncurses5-dev openssl libssl-dev
cd
wget http://www.erlang.org/download/otp_src_$ERLANG_VERSION.tar.gz
tar zxvf otp_src_$ERLANG_VERSION.tar.gz
cd otp_src_$ERLANG_VERSION
./configure --prefix=/opt/erlang-$ERLANG_VERSION
make
sudo make install

sudo apt-get remove erlang erlang-base erlang-nox
sudo apt-get autoremove
sudo rm -fR /usr/lib/erlang/lib/*

sudo rm /opt/erlang
sudo ln -s /opt/erlang-$ERLANG_VERSION /opt/erlang
cd /usr/bin
sudo ln -fs /opt/erlang/bin/* .

cd $ORIG_PWD
echo "Done installing Erlang $ERLANG_VERSION."
