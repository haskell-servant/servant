#!/bin/sh

set -e

SIZE=10000000
SLOWURL="http://localhost:8000/slow/2"
FASTURL="http://localhost:8000/get/$SIZE"
PROXYURL="http://localhost:8000/proxy"

# we use binary, as there's some size
TESTFILE="$(cabal-plan list-bin servant-machines:test:example)"
TMPFILE="$(mktemp)"

CURLSTATS="time: %{time_total}, size:  %{size_download}, download speed: %{speed_download}\n"

# cleanup

cleanup() {
  if [ ! -z "$MACHINES_PID" ]; then
    kill "$MACHINES_PID" || true
  fi

  if [ ! -z "$CONDUIT_PID" ]; then
    kill "$CONDUIT_PID" || true
  fi

  if [ ! -z "$PIPES_PID" ]; then
    kill "$PIPES_PID" || true
  fi

  if [ ! -z "$COOKBOOK_PID" ]; then
    kill "$COOKBOOK_PID" || true
  fi

  rm -f "$TMPFILE"
}

trap cleanup EXIT

bench() {
  echo "\e[36m=== $1 ===\e[0m"
}

# Server
#######################################################################

## Machines

bench "server machines"

$(cabal-plan list-bin servant-machines:test:example) server +RTS -sbench-machines-server-rts.txt &
MACHINES_PID=$!
echo "Starting servant-machines server. PID=$MACHINES_PID"

# Time to startup
sleep 1

# Run slow url to test & warm-up server
curl "$SLOWURL"

curl --silent --show-error "$FASTURL" --output /dev/null --write-out "$CURLSTATS" > bench-machines-server.txt

curl --silent --show-error "$PROXYURL" --request POST --data-binary @"$TESTFILE" --output "$TMPFILE" --write-out "$CURLSTATS" > bench-machines-server-proxy.txt

kill -INT $MACHINES_PID
unset MACHINES_PID

## Pipes

bench "server pipes"

$(cabal-plan list-bin servant-pipes:test:example) server +RTS -sbench-pipes-server-rts.txt &
PIPES_PID=$!
echo "Starting servant-pipes server. PID=$PIPES_PID"

# Time to startup
sleep 1

# Run slow url to test & warm-up server
curl "$SLOWURL"

curl --silent --show-error "$FASTURL" --output /dev/null --write-out "$CURLSTATS" > bench-pipes-server.txt

curl --silent --show-error "$PROXYURL" --request POST --data-binary @"$TESTFILE" --output "$TMPFILE" --write-out "$CURLSTATS" > bench-pipes-server-proxy.txt

kill -INT $PIPES_PID
unset PIPES_PID

## Cookbook

bench "server cookbook"

$(cabal-plan list-bin cookbook-basic-streaming) server +RTS -sbench-cookbook-server-rts.txt &
COOKBOOK_PID=$!
echo "Starting servant-cookbook server. PID=$COOKBOOK_PID"

# Time to startup
sleep 1

# Run slow url to test & warm-up server
curl "$SLOWURL"

curl --silent --show-error "$FASTURL" --output /dev/null --write-out "$CURLSTATS" > bench-cookbook-server.txt

curl --silent --show-error "$PROXYURL" --request POST --data-binary @"$TESTFILE" --output "$TMPFILE" --write-out "$CURLSTATS" > bench-cookbook-server-proxy.txt

kill -INT $COOKBOOK_PID
unset COOKBOOK_PID

## Conduit

bench "server conduit"

$(cabal-plan list-bin servant-conduit:test:example) server +RTS -sbench-conduit-server-rts.txt &
CONDUIT_PID=$!
echo "Starting servant-conduit server. PID=$CONDUIT_PID"

# Time to startup
sleep 1

# Run slow url to test & warm-up server
curl "$SLOWURL"

curl --silent --show-error "$FASTURL" --output /dev/null --write-out "$CURLSTATS" > bench-conduit-server.txt

curl --silent --show-error "$PROXYURL" --request POST --data-binary @"$TESTFILE" --output "$TMPFILE" --write-out "$CURLSTATS" > bench-conduit-server-proxy.txt

# kill -INT $CONDUIT_PID
# unset CONDUIT_PID

# Client
#######################################################################

# Uses conduit as server

## Machines

bench "client machines"

# Test run
$(cabal-plan list-bin servant-machines:test:example) client 10

# Real run
/usr/bin/time --verbose --output bench-machines-client-time.txt \
  "$(cabal-plan list-bin servant-machines:test:example)" client "$SIZE" +RTS -sbench-machines-client-rts.txt

## Pipes

bench "server pipes"

# Test run
$(cabal-plan list-bin servant-pipes:test:example) client 10

# Real run
/usr/bin/time --verbose --output bench-pipes-client-time.txt \
  "$(cabal-plan list-bin servant-pipes:test:example)" client "$SIZE" +RTS -sbench-pipes-client-rts.txt

## Conduit

bench "client conduit"

# Test run
$(cabal-plan list-bin servant-conduit:test:example) client 10

# Real run
/usr/bin/time --verbose --output bench-conduit-client-time.txt \
  "$(cabal-plan list-bin servant-conduit:test:example)" client "$SIZE" +RTS -sbench-conduit-client-rts.txt

## Cookbook

bench "server cookbook"

# Test run
$(cabal-plan list-bin cookbook-basic-streaming) client 10

# Real run
/usr/bin/time --verbose --output bench-cookbook-client-time.txt \
  "$(cabal-plan list-bin cookbook-basic-streaming)" client "$SIZE" +RTS -sbench-cookbook-client-rts.txt

## Kill server

kill -INT $CONDUIT_PID
unset CONDUIT_PID
sleep 1

# Exit
#######################################################################

header() {
  { echo "$1 $2";
    echo ""
  } >> bench.md
}

report() {
  echo "\`\`\`" >> bench.md
  cat "$1"      >> bench.md
  echo "\`\`\`" >> bench.md
  echo ""       >> bench.md
}

report2() {
  echo "\`\`\`" >> bench.md
  cat "$1" | sed 's/^\s*//' >> bench.md
  echo "\`\`\`" >> bench.md
  echo ""       >> bench.md
}

note() {
  echo "$1"     >> bench.md
  echo ""       >> bench.md
}

rm -f bench.md

header "#" "Streaming test benchmark"
note "size parameter: $SIZE"

header "##" Server

note "- /fast/$SIZE\n- /proxy"

header "###" machines
report bench-machines-server.txt
report bench-machines-server-proxy.txt
report bench-machines-server-rts.txt

header "###" pipes
report bench-pipes-server.txt
report bench-pipes-server-proxy.txt
report bench-pipes-server-rts.txt

header "###" conduit
note "Conduit server is also used for client tests below"
report bench-conduit-server.txt
report bench-conduit-server-proxy.txt
report bench-conduit-server-rts.txt

header "###" cookbook
report bench-cookbook-server.txt
report bench-cookbook-server-proxy.txt
report bench-cookbook-server-rts.txt

header "##" Client

header "###" machines
report2 bench-machines-client-time.txt
report bench-machines-client-rts.txt

header "###" pipes
report2 bench-pipes-client-time.txt
report bench-pipes-client-rts.txt

header "###" conduit
report2 bench-conduit-client-time.txt
report bench-conduit-client-rts.txt

header "###" cookbook
report2 bench-cookbook-client-time.txt
report bench-cookbook-client-rts.txt

# Cleanup filepaths
sed -E -i 's/\/[^ ]*machines[^ ]*\/example/...machines:example/' bench.md
sed -E -i 's/\/[^ ]*conduit[^ ]*\/example/...conduit:example/' bench.md
sed -E -i 's/\/[^ ]*pipes[^ ]*\/example/...pipes:example/' bench.md
sed -E -i 's/\/[^ ]*\/cookbook-basic-streaming/...cookbook-basic-streaming/' bench.md

sleep 3
