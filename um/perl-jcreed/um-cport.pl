#!/usr/bin/perl

$| = 1;
use constant MO => 0xffffffff;
use constant O => 2 ** 32;

if (@ARGV < 1) {
    print STDERR "\nusage:\n$0 program.um\n";
    exit -1;
}
open IN, $ARGV[0] or die $!;

my @reg = (0) x 8;
my @arr;
my $zero;
{local $/; $zero = [unpack "N*", <IN>]}
$arr[0] = $zero;
my $ip = 0;

MAIN: while(1) {
    my $w = $zero->[$ip++];

    # my $c = $w & 7;
    # my $b = ($w >> 3) & 7;
    # my $a = ($w >> 6) & 7;
    
    # $count{$w >> 28}++; # collect instruction profiling
    # debug(($w >> 6) & 7, ($w >> 3) & 7, $w & 7, $w);
    
    goto (chr((ord "A") + ($w >> 28))); 

  A: $reg[$w & 7] and $reg[($w >> 6) & 7] = $reg[($w >> 3) & 7]; next;
  B: $reg[($w >> 6) & 7] = $arr[$reg[($w >> 3) & 7]][$reg[$w & 7]]; next;
  C: $arr[$reg[($w >> 6) & 7]][$reg[($w >> 3) & 7]] = $reg[$w & 7]; next;
  D: $reg[($w >> 6) & 7] = ($reg[($w >> 3) & 7] + $reg[$w & 7]) % O; next;
  E: $reg[($w >> 6) & 7] = ($reg[($w >> 3) & 7] * $reg[$w & 7]) % O; next;
  F: $reg[($w >> 6) & 7] = int($reg[($w >> 3) & 7] / $reg[$w & 7]); next;
  G: $reg[($w >> 6) & 7] = MO & (~($reg[($w >> 3) & 7] & $reg[$w & 7])); next;
  H: print "\n\n== Halted. ==\n"; last;
  I: $reg[($w >> 3) & 7] = @arr; 
    $arr[$reg[($w >> 3) & 7]] = [];
    next;
  J: $arr[$reg[$w & 7]] = 0; next;
  K: print chr($reg[$w & 7]); next;
  L: my $ch = getc; 
    $ch = defined $ch ? ord $ch : MO;
    $reg[$w & 7] = $ch;
    next;
  M: $reg[($w >> 3) & 7] and
      @{$zero} = @{$arr[$reg[($w >> 3) & 7]]};
    $ip = $reg[$w & 7]; 
    next;
  N: $reg[7 & ($w >> 25)] = $w & 0x1ffffff; next;
}

# profile(); # give report

sub profile {
    for (sort {$count{$b} <=> $count{$a}} keys %count) {
	print "inst $_: $count{$_}\n";
    }
}

sub sig {
    $_[0] > (1 << 31) ?  ($_[0] - MO) - 1 : $_[0];
}
sub debug {
    my ($a, $b, $c, $w) = @_;
    my $inst = $w >> 28;
    my $regs = join ", ", map {sig($_)} @reg;
    if ($inst == 13) {
	my $reg = 7 & ($w >> 25);
	my $val = $w & 0x1ffffff;
	print "$ip: ldi\tr$reg = $val  \t";
    }
    elsif ($inst == 6) {
	print "$ip: nand\tr$a = r$b | r$c \t";
    }

    elsif ($inst == 12) {
	print "$ip: jmp\tr${b}[r$c]  \t";
    }
    else {
	print "$ip: $inst \t$a $b $c   \t";
    }
    print "regs = ($regs)\n";
}
