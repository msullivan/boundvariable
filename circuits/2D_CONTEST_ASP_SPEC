From: ohmega@cbv.net
Newsgroups: cult.cbv.discuss
Message-ID: <82F28FD4.A4DE@cbv.net>
Date: 23 Jul 19106 13:11:57
X-Organization: Cult of the Bound Variable
Subject: 2D the Ultimate Programming Language?


Hey 2D users,

 You may have noticed my colleague Harmonious Monk's recent release of
his O'Cult language.  I've tried to convince him that O'Cult is based
on misguided anti-modular design principles, but with all the grant
tablets available for Advice research, he won't give it up.

 Clearly, the 2D module system is more powerful than O'Cult.  I'd like
to demonstrate this by implementing O'Cult using 2D modules.
Unfortunately, with the Principles Of Sand Languages deadline
approaching, I don't have time to hammer out an implementation.  Perhaps
one of you would like to undertake this challenge?  

 I've modified verify to accept "ocult" tests.  Your program should
supply a module "step" which accepts a 2D-encoded O'Cult program
(defined below) on its West input, and outputs the result of applying
the supplied advice to the supplied O'Cult term exactly once.

The function [[-]] represents an O'Cult program as data in 2D:

   We think of the names used in variables and constants as numbers 
   and represent them as follows:
   [[zero]] = Inl ()
   [[s(n)]] = Inr [[n]] 

   Representation of terms:
   [[App(e1,e2)]] = Inl ([[e1]], [[e2]])
   [[Const s]]    = Inr [[s]]

   Representation of patterns:
   [[App(p1,p2)]] = Inl ([[p1]], [[p2]])
   [[Const s]]    = Inr Inl [[s]]
   [[Var s]]      = Inr Inr [[s]]

   Representation of rules:
   [[p1 => p2]]    = ([[p1]], [[p2]])

   A sentence of advise is represented as a list of patterns, 
   where lists are represented as follows:
   [[nil]]       = Inl ()
   [[cons(h,t)]] = Inr ([[h]], [[t]])

 For example, the O'Cult program 

    Add Z y => y; 

 which is a list with a single rule

    App(App(Const 0, Const 1), Var 2) => Var 2

 and so is represented by the 2D value

    Inr ((Inl (Inl (Inr Inl Inl (),
		    Inr Inl Inr Inl ()),
	       Inr Inr Inr Inr Inl ()),
	  Inr Inr Inr Inr Inl ()),
	 Inl ())

 Your step module will be suppled the representation of an (advice,term)
pair, and should output the term that results from applying the advice
once to term.

 As incentive, I'll be happy to provide anyone who completes the
O'Cult-in-2D interpreter with a copy of my forthcoming book, 
"Type Sand Programming Languages".

- Bill

---------------------------------------------
 Bill Ohmega     "Hell is other programming
ohmega@cbv.net    languages." -- Sartran
---------------------------------------------
