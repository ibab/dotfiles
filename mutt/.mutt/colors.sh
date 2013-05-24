#!/bin/bash

# Enhanced colors for color terminals
if echo $TERM | grep -q '^.*256color$'
then
    echo 'source ~/.mutt/colors.muttrc'
fi

