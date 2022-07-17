#!/bin/sh
echo type e =
sed -n "s/^Blockly.Blocks\['\([^']*\)'.*/  | \u\1 [@name \"\1\"]/p" $*
echo "  [@@deriving yojson]"
