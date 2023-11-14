#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
echo "Script location: $SCRIPT_DIR"
COMPLETE_PATH="$SCRIPT_DIR/src/app/manager.clj"
CLASS_PATH="$SCRIPT_DIR/src"
bb -f "$COMPLETE_PATH" --classpath "$CLASS_PATH" "$1" "$2" "$3" "$4" "$5"