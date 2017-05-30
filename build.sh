#!/bin/bash

if [ ! -e './rosa.asd' ]; then
    echo 'error: here is not in rosa project root.'
    exit 
fi

# build ASDF
ASDF_LATEST=https://common-lisp.net/project/asdf/archives/asdf.lisp
curl -L $ASDF_LATEST > asdf.lisp

# get executable which will be created
EXECUTABLE_PATH=$(ros run \
                      -l ./asdf.lisp \
                      -e '(progn
                            (require :asdf)
                            (push (uiop:getcwd) asdf:*central-registry*)
                            (format t "~a~%" (asdf:output-file '\''asdf:program-op :rosa/cli))
                            (uiop:quit))')
echo "executable will be created into: $EXECUTABLE_PATH"

if [ -e "$EXECUTABLE_PATH" ]; then
    rm "$EXECUTABLE_PATH"
fi
ros run \
    -l ./asdf.lisp \
    -e '(progn
          (require :asdf)
          (push (uiop:getcwd) asdf:*central-registry*)
          (asdf:operate '\''asdf:program-op :rosa/cli))'

EXECUTABLE_NAME=$(basename "$EXECUTABLE_PATH")

if [[ $EXECUTABLE_NAME =~ . ]]; then
mv "$EXECUTABLE_PATH" ./rosa
else
mv "$EXECUTABLE_PATH" \
   ./$(echo $EXECUTABLE_NAME | sed -E -e 's/^.+(\..+)$/rosa\1/')
fi
