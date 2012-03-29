#!/bin/bash -e

# new-project.sh <project-dir>

SCRIPT=`readlink -f $0`
SCRIPTPATH=`dirname $SCRIPT`
BASEDIR=$SCRIPTPATH/../
JSLIBDIR=$BASEDIR/jslib/

PROJECTDIR=$1

mkdir -p $PROJECTDIR && cd $PROJECTDIR

mkdir jslib www src

ln -s $JSLIBDIR/extjs jslib/
ln -s $JSLIBDIR/jquery.js jslib/
ln -s $JSLIBDIR/socket.io-client jslib/
ln -s $JSLIBDIR/jit jslib/
ln -s $JSLIBDIR/processing-api.js jslib/

cp $BASEDIR/client-src/index.html www/
cp $BASEDIR/client-src/client.js www/

LOADER=";; -*- Mode: common-lisp; Package: cl-user -*-

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error \"This file is not intended to be compiled!\"))

(load \"$BASEDIR/loader.lisp\")

(load \"$PROJECTDIR.asd\")

(asdf:oos 'asdf:load-op :$PROJECTDIR)

(in-package :cl-gui)

"
echo "$LOADER" > loader.lisp

ASDF=";; -*- Mode: common-lisp; Package: cl-user -*-

(in-package :cl-user)

(asdf:defsystem :$PROJECTDIR
  :serial t
  :components
  ((:module src
    :components (;; (:file \"package\")
                 )
    :serial t)
   (:module client-src
    :components (;; (:ps-file \"main\")
                 )
    :serial t))
  :depends-on (:cl-gui))
"

echo "$ASDF" > $PROJECTDIR.asd

