From: ohmega@cbv.net
Newsgroups: cult.cbv.discuss
Message-ID: <2C9F8CC7.3ED3@cbv.net>
Date: 22 Jun 19106 06:44:29
X-Organization: Cult of the Bound Variable
Subject: Programming in Two Dimensions


Dear cult.cbv.discuss:

I'm pleased to announce a new programming language called 2D. This
language frees the programmer from the shackles of linear programming
by allowing programs to occupy two dimensions. However, unlike 3- and
4- dimensional languages such as CUBOL and Hypercard, it does not
distract the programmer's attention with needless dimensional abandon.

I first present an overview of the language and then delve into a more
careful description of its syntax and semantics.

== 2D Overview ==

2D programs are built from boxes connected together by wires. A box takes
the following form:

    *=======*
    !command!
    *=======*

Wires can connect boxes:


    *========*       *========*
    !command1!------>!command2!
    *========*       *========*

Each box has two input interfaces: its North and West sides. It also
has two output interfaces, its South and East sides. The following box
sends the input that it receives on its North interface to its East
interface:

       |
       v
    *============*
    !send [(N,E)]!----->
    *============*

Wires carry values from one box to another. Each wire starts out with
no value. When a value is sent along a wire, the wire keeps that same
value forever. A box will only activate when all of its inputs (zero,
one, or two) have values.

The values flowing along wires take on the following forms:

val ::= () | (val, val) | Inl val | Inr val

The () value is the single base value. Two values can be paired
together. They can also be stamped with the disjoint constructors Inl
and Inr. Commands manipulate the structure of values and the control
flow of the program by selectively sending along their outputs. For
example, the 'case' command distinguishes between values stamped with
Inl and Inr:

     |
     v
 *=============*
 !case N of E,S!----
 *=============*
     |
     +--------------

If this box is sent Inl () to its North interface, then () is sent
along the wire connecting to the east interface. If it is sent
Inr ((), ()) then ((), ()) is sent along the south interface instead.


2D programs can be organized into modules. A module encapsulates a
collection of boxes and wires and gives them a name. The following
module, called stamp, encapsulates the operation of applying the Inl
and Inr constructors to the first and second components of a pair:

,........|.......................................,
:stamp   |                                       :
:        v                                       :
:     *=======*                                  :
:     !split N!-----+                            :
:     *=======*     v                            :
:        |       *=========================*     :
:        +------>!send [((Inl W, Inr N),E)]!------
:                *=========================*     :
:                                                :
,................................................,

(The split command splits a pair, sending the first component
 south and the second component east.)

A module can be used as a box itself. The following circuit sends
(Inl (), Inr Inl ()) along the wire to the east:

        *========================*
        !send [(((), Inl ()), E)]!---+
        *========================*   |
    +--------------------------------+
    v
  *=========*
  !use stamp!-----------------------------------
  *=========*

Each time a "use" box is executed, a new copy of the referenced module
is made (with wires carrying no values). Recursion is just a
particular use of modules: modules may also "use" themselves. Mutual
recursion between modules is also permitted.

A module is limited to at most one input along each of its north and
west faces. It may have multiple outputs, all along its east face.
When a module is executed, exactly one of its output wires must be
sent a value; this is the value that the "use" box sends along its
interface.

== 2D Syntax ==

=== Box syntax ===

A box's north and south edges are written with the = symbol. Its west
and east edges, which must be exactly one character long, are written
with the ! symbol. The box's corners are written *. No whitespace is
allowed between the command and the box that surrounds it.

The concrete syntax for commands is as follows:

inface ::= N | W

outface ::= S | E

exp ::= () | (exp, exp) | Inl exp | Inr exp | inface

command ::= send []
          | send [(exp, outface)]
          | send [(exp, outface), (exp, outface)]
          | case exp of outface, outface
          | split exp
          | use "name"

Note that extra parentheses are neither required nor permitted.
A space character may be omitted when the character to its left or to
its right is one of ,()[] and two consecutive space characters are
never allowed.

A name consists of one or more characters from the following set:

0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ

If a wire is connected to the north side of a box, the v character
must be used as follows:

    |
    v
  *=======*
  !command!
  *=======*

The wire can connect above any = character. If a wire is connected to
the west side of a box, the > character must be used as follows:

    *=======*
 -->!command!
    *=======*

At most one wire can be connected to each of a box's four faces.

=== Wire syntax ===

Wires are made from the following characters:

|-+#

Every wire must use at least one of these characters.  That is, 
> and v alone are not valid wires.

Each character is "open" on some of its sides. The | character is
open on its north and south sides. The - character is open on its
west and east sides. The + and # characters are both open on all
four sides.

The = character on the south face of a box is open to its south,
and the ! character on the east side of a box is open to its east.
The v character is open to its north, and the > character is open
to its west.

All wire characters within a module must obey the following rules of
connectedness:

  For each - character, its west and east neighbors must both
  be open on their east and west sides, respectively.

  For each | character, its north and south neighbors must
  both be open on their south and north sides, respectively.

  For each # character, its north, south, west, and east neighbors
  must each be open on their south, north, east, and west sides,
  respectively.

  For each + character, exactly two of the following conditions must
  be met:
    a. its north neighbor is open on its south side
    b. its south neighbor is open on its north side
    c. its west neighbor is open on its east side
    d. its east neighbor is open on its west side

Only the | and - wire characters are allowed along module boundaries, and
they only require a single open neighbor on the inside of the module.
(They do not syntactically connect to anything on the outside.)

=== Module syntax ===

The input consists of an arrangement of non-overlapping modules. Each
module is bordered by the . character on its north and south face, the
: character on its west and east face, and the , character in each
corner. Additionally, the north face may optionally have one
occurrence of the | character; this is the north input to the module.
Similarly, the west input (if any) is represented by a - character.
The east side of the module may have any number of occurrences of the
- character; these are its outputs. A module's name must appear in the
upper left corner of the module and be followed by a space.

== 2D Semantics ==

Evaluation of 2D programs revolves around a function for computing the
value of a module instance. A module instance is a collection of
wires, some of which have values, and the boxes that these wires
connect.

A module instance evaluates in a series of evaluation steps. In each
step, the "ready" boxes are identified as those boxes for which all of
their inputs wires have values, and which have not yet executed in
this instance. All ready boxes are evaluated (see below) in an
arbitrary order. If no boxes are ready, then the module instance is
finished. Its output is the value of the single output wire that has a
value. If more than one wire has a value, or if no wire has a value,
then evaluation fails.

=== Box evaluation ===

Boxes only execute when all of their input wires have values. This is
true even if the command does not reference all of the wires.

Commands are executed as follows. First, all expressions in the
command are evaluated. The expressions N and W are replaced with the
values on the North and West wires, respectively. If a value is needed
but no wire is connected, then evaluation fails. Then, commands are
executed as follows:


send []
  nothing happens.

send [(val, outface)]
  val is sent along the specified outface.

send [(val1, outface1), (val2, outface2)]
  val1 is sent to outface1, and val2 is sent to outface2.
  The two outfaces may not be equal.

split (val1, val2)
  val1 is sent south, and val2 is sent east.

case Inl val of outface1, outface2
  val is sent to outface1.

case Inr val of outface1, outface2
  val is sent to outface2.

use mod
  a new instance of the module mod is evaluated. The inputs to 
  the module must match the inputs to this box, and are instantiated
  with the values along those wires. The output along the east
  face is the output of the module instance.


In any other situation (for example, split ()), the machine fails. 
If a value is sent along an outface, then there must be a wire
connected, or the machine fails.



I've developed a prototype interpreter for 2D, which runs on Umix.
Please try it out!

 - Bill


---------------------------------------------
 Bill Ohmega      "Hell is other programming
ohmega@cbv.net     languages." -- Sartran
---------------------------------------------
