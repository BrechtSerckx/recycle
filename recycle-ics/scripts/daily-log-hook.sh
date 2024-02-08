#! /usr/bin/env bash

severity=$1
callstack=$2
msg=$3
if [ -n "$RECYCLE_ICS_LOG_HOOK_DIR" ]; then
    dir="$RECYCLE_ICS_LOG_HOOK_DIR"
else
    echo "Please specify RECYCLE_ICS_LOG_HOOK_DIR"
    exit 1
fi

logfile="$dir/$(date +%F).log"
logmsg=$(cat <<EOF
New log message with verbosity $severity:
$msg
EOF
)
function mail() {
    echo "$@"
}

mkdir -p "$dir"
if [ ! -f "$logfile" ]; then
    mail "$logmsg"
    echo "$logmsg" > "$logfile"
else
    echo "$logmsg" >> "$logfile"
fi
