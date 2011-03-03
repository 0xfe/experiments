#!/bin/bash

# SPOOL=/var/spool/asterisk/outgoing
SPOOL=/opt/asterisk/var/spool/asterisk/outgoing
TEMP=`/bin/mktemp /tmp/$0.XXXXXX`
SOURCE=172.25.189.35
TARGET=$1
MESSAGE=$2

cat >$TEMP <<GROK
Channel: SIP/$TARGET@yahoo_ame
MaxRetries: 0
RetryTime: 30
WaitTime: 60
Context: yahootest
Extension: s
Priority: 1
SetVar: SourceNumber=$SOURCE
SetVar: TargetNumber=$TARGET
SetVar: Message=$MESSAGE
GROK

# chown asterisk:asterisk $TEMP

# Callerid: $SOURCE
# Callerid: 12125657243

mv $TEMP $SPOOL
