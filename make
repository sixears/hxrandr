#!/home/martyn/bin/bash

pkgs_xrandr=$(dirname $( dirname $( realpath $(type -p xrandr) )))

for f in $( find proto/ -type f -name \*.hs ); do
  t=src/"${f#proto/}"
  perl -plE "s{__xrandr__}{$pkgs_xrandr}g" "$f" > "$t"
done
