#!/bin/sh

# resolve links - $0 may be a softlink
scriptname="$0"
while [ -h "$scriptname" ]; do
  ls=`ls -ld "$scriptname"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    scriptname="$link"
  else
    scriptname=`dirname "$scriptname"`/"$link"
  fi
done

# Get standard environment variables
scriptdir=`dirname "$scriptname"`
scriptdir=`cd "${scriptdir}";pwd` # get absolute path

tmpscriptdirbase=`basename "$scriptdir"`
if [ "$tmpscriptdirbase" = "sbin" ]; then
	scriptdir="$scriptdir/../deploy/scripts"
fi

prosperodir=`cd "${scriptdir}/../..";pwd`

envdir=`cd "${scriptdir}/../environmental";pwd`

. "${envdir}/EnvVars.sh"

#gives chance to overwrite or set values per machine
machineName=`hostname -f`
if [ -e "${envdir}/${machineName:-NoMachineNameSet123}-vars.sh" ]; then  #if machine is not set use something random so will not find file but will not fail if nounset is set.
   . "${envdir}/${machineName}-vars.sh"
fi

sbindir=`cd "${scriptdir}/../../sbin";pwd`
depsdir=`cd "${scriptdir}/../../deps";pwd`
installed_erlang_libs_file="$depsdir/erlang/installed_erlang_libs.history"

AUDIT_LOG_PATH="$LOG_PATH/prospero.audit.log"
SASL_LOG_PATH="$LOG_PATH/prospero.sasl.log"
KERNEL_LOG_PATH="$LOG_PATH/prospero.kernel.log"

PROSPERO_MNESIA_DIR="${PROSPERO_MNESIA_DIR_BASE}/Mnesia.prospero.`hostname`"

