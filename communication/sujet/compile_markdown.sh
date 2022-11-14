#!/bin/sh

#  compiler_markdown.sh
#  
#
#  Created by Laure Runser on 01/11/2022.
#  

echo "compiles the file optimizer.md into optimizer.pdf"

pandoc -V geometry:left=4cm -V geometry:right=4cm -V colorlinks=true -V urlcolor=blue optimizer.md -o optimizer.pdf

