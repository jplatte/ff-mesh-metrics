# ff-mesh-metrics

A small program that parses graph.json from ffmap-backend and outputs some metrics.

## How to build

* Install [Stack][]
* Execute `stack build`

[Stack]: http://docs.haskellstack.org/en/stable/README.html

## How to use

Here is an example for how to integrate the data from this program into graphite,
using a carbon line-feed socket. Requires `jshon` and `nc` (netcat).

```bash
#!/bin/bash

pathToGraphJson="/home/someuser/ffmap-backend/data/graph.json"
wspPath="ffxy.globalstats"
carbonHost="localhost"
carbonPort="2003"

jsonData=$(./ff-mesh-metrics < "$pathToGraphJson")
carbonInput=""

for attr in $(jshon -k <<< "$jsonData"); do
	value=$(jshon -e $attr <<< "$jsonData")
	timestamp=$(date +%s)
	carbonInput+="$wspPath.$attr $value $timestamp"$'\n'
done

nc -q0 $carbonHost $carbonPort <<< "$carbonInput"
```
