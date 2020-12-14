#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
COLOR_RESET='\033[0m'

ECHO_ARGUMENTS=(
    # Check basic flags
    '--help'
    '--version'
    # Errors
    'Tests/encode/64 --wrap==64'
    'Tests/encode/1 --wrap='
    'Tests/encode/1 --wrap'
    'Tests/encode/1 -w=64'
    'Tests/encode/1 -- -w=64'
    'Tests/esteArquivoNaoExiste'
    # Test encoding
    'Tests/encode/1 --'
    'Tests/encode/1'
    'Tests/encode/20'
    'Tests/encode/21'
    'Tests/encode/22'
    'Tests/encode/23'
    'Tests/encode/64'
    ## Wrap
    'Tests/encode/20 -w 2'
    'Tests/encode/21 -w 3'
    'Tests/encode/22 -w 5'
    'Tests/encode/64 -w 63'
    'Tests/encode/64 -w 64'
    'Tests/encode/64 --w 64'
    '-w 64 Tests/encode/64'
    'Tests/encode/64 --wrap=64'
    '--wrap=64 Tests/encode/64'
    'Tests/encode/64 -w 32'
    '-w 32 Tests/encode/64'
    # Decoding
    ## Valid strings
    '-d Tests/decode/1'
    '-d Tests/decode/2'
    'Tests/decode/2 -d'
    '-i Tests/decode/2 -d'
    '-d Tests/decode/2 -i'
    '-d Tests/decode/3'
    '-d Tests/decode/4'
    '-d Tests/decode/5'
    ## Invalid strings
    '-d Tests/decode/20'
    '-d Tests/decode/21'
    '-d Tests/decode/22'
    '-d Tests/decode/23'
    '-d Tests/decode/24'

    '-d Tests/decode/20 -i'
    '-d -i Tests/decode/20'
    '-d -i Tests/decode/21'
    '-d -i Tests/decode/22'
    '-d Tests/decode/23 -i'
    '-d Tests/decode/24 -i'
)

# Remove output files if avaliable
rm -f -- mybase64.out base64.out

# Execute base64 and mybase64
for echo_argument in "${ECHO_ARGUMENTS[@]}"; do
    eval $"base64 $echo_argument" >>base64.out 2>>base64.out
    eval $"./mybase64 $echo_argument" >>mybase64.out 2>>mybase64.out
done

# Show difference between files
sdiff --text --suppress-common-lines --minimal base64.out mybase64.out

