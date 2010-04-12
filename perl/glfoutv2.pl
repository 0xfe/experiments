#!/usr/bin/perl

#
# Usage:
#   glfout filename|<wildcard>
#   this puppy decodes the glf files in a manner easily opened by Excel. COMMA DELIMITED

sub decode_glf {
  $file = shift;
  ($prefix,$suffix) = split(/\./,$file);
  $CSV = $prefix . ".csv";
  $TXT = $prefix . ".txt";
  open(OUTCSV,">$CSV");
  open(OUTTXT,">$TXT");
  open (GLF, $file) or die "Can't open file: $!";
  binmode (GLF);

  read(GLF, $title, 24);
  $title =~ s/\0/ /g;
  print OUTCSV "Title: " . $title . "\n\n";
  print OUTTXT "name, lat, long " . "\n\n";

  $entry = 0;

  while (1) {
    last unless read(GLF, $lat, 8);
    last unless read(GLF, $long, 8);
    if ($entry != 11) {
      last unless read(GLF, $text, 16);
    } else {
      last unless read(GLF, $data1, 8);
      last unless read(GLF, $data2, 8);
    }


    # The 12th entry is always garbage.
    if (++$entry == 12) {
      $entry = 0;
      $value1 = unpack("d", pack("H8", $data1));
      $value2 = unpack("d", pack("H8", $data2));
      $data = sprintf(",%f, %f\n",$value1, $value2);
      print OUTCSV "$data";
      $data = sprintf("%f, %f\n",$value1, $value2);
      print OUTTXT "$data";
      next;
    }

    $lat_float = unpack("d", $lat);
    $long_float = unpack("d", $long);

    # Strip \0s from text
    $text =~ s/\0/ /g;

    # Trim spaces
    $text =~ s/\s+$//g;

    $data = sprintf("%s, %f, %f\n", $text, $lat_float, $long_float);
    print OUTTXT "$data";

    print OUTCSV "$file" if $text =~ /front/;
    $text = substr($text,0,8);
    # $text =~ s/\s+$// if $text =~ /STM/;
    $data = sprintf (",%s, %f, %f", $text, $lat_float, $long_float);
    print OUTCSV "$data";
  }

  close GLF;
  close OUTCSV;
  close OUTTXT;
}

# ----- MAIN ---- #

if ((scalar @ARGV) == 0) {
  print "Usage: $0 filename|<wildcard>\n";
  exit;
}

for $filename (@ARGV) {
  decode_glf($filename);
}

