#! bin/bash

# Dependencies
echo '
Dependencies
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep 'also installing the dependencies'

# Installed packages
echo '
Installed packages
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep ' DONE '

# Warnings
echo '
Warnings
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep 'Warning'

# Non-zero exist status
echo '
Non-zero exist status
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep 'non-zero exit status'
