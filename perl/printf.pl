#!/usr/bin/perl

$f = -44.4234255234;
$g = -44.4234223499816986969279472346213455234;

printf "%2.6f", $f;

print "\n";

print unpack("d", pack("d", $f)) . "\n";
print unpack("d", pack("d", $g)) . "\n";

print "\n";
