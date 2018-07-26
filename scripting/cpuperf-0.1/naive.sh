#!/bin/sh

# s=`sysctl hw.setperf`
# old=`echo $s | sed 's/.*=//'`
# if [ "100" = $old ] ; then
#     new=0
# else
#     new=100
# fi
# sudo sysctl -w hw.setperf=$new > /dev/null
# printf "cpu: %d -> %d\n" $old $new

speed=`sysctl hw.cpuspeed`
clock=`echo $speed | sed 's/.*=//'`
clock=`bc -l -e "$clock / 1000" -e quit`
printf "clock: %0.1f Ghz\n" $clock
