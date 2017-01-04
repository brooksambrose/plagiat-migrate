#! bin/bash

# Dependencies
echo '
Dependencies
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep 'also installing the dependenc'

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

# Fail
echo '
Fail
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep '[Ff][Aa][Ii][Ll]'

# Error
echo '
Error
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep '[Ee][Rr][Rr][Oo][Rr][^=.]'

# Cannot find
echo '
Cannot find
'
cat '/Users/bambrose/Dropbox/GitHub/hos/plagiat/logs/bench.log' | grep 'cannot find'

apt-get install -y apt-file && apt-file update
apt-file search