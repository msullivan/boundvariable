(* printed results compare bests *)

(* avg of how many times bigger res2 is than res1 *)
fun average l =
    (foldl (op+) 0.0 l) / (Real.fromInt (length l))

fun factors (list1, list2) = 
    (map (fn (x1, x2) => x2 / x1) (ListPair.zipEq (list1, list2)))

val labels = ["fact.161", "fact.345", (* micro-10k *) "micro-add", "micro-allocfree", "micro-asub0", "micro-cmov0", "micro-cmov1", "micro-div", "micro-literal", "micro-loadprog0", "micro-mul", "micro-nand", (* "micro-null", *) "micro-upd0", "quicksort.161", "quicksort.345"] 

(* pairs are from the same run *)

val cTom = [0.71, 0.44, (* 0, *) 2.42, 6.68, 2.18, 2.3, 2.75, 5.74, 2.12, 3.29, 2.37, 2.2, (* 0.03, *) 2.71, 0.88, 0.61]
val schemeDanPlt = [222.56, 119.51, (* 0.14, *) 1021.1, 2528.65, 985.68, 507.5, 560.5, 978.8, 1277.75, 2170.82, 938.83, 1144.92, (* 0.02, *) 1072.6, 264.54, 174.54] 
val ratiosPltToC = factors (cTom, schemeDanPlt)

(* no compiler flags *)
val cTom2 = [0.67, 0.44, (* 0.02 , *) 2.0, 8.55, 1.95, 2.42, 2.28, 5.06, 1.77, 3.38, 2.07, 2.18, (* 0.03, *) 2.23, 0.72, 0.61]
val schemeDanBigloo = [133.77, 77.85, (* 0.16, *) 522.4, 982.85, 529.8, 450.15, 445.48, 523.02, 863.94, 1184.28, 551.31, 516.91, (* 0.03, *) 560.51, 189.43, 120.54]
val ratiosBiglooToC = factors (cTom2, schemeDanBigloo)

(* -O bench *)
val cTom3 = [0.36, 0.22, (* 0 *) 1.09, 5.06, 1.1, 2.7, 1.25, 2.97, 0.91, 1.92, 1.2, 1.19, (* 0 *) 1.37, 0.46, 0.33]
val schemeDanBiglooBench = [27.04, 16.0, (* 0.05 *) 109.71, 138.89, 183.62, 169.28, 86.73, 112.19, 145.82, 235.49, 107.34, 103.89, (* 0 *) 108.42, 35.47, 25.63]
val ratiosBiglooBenchToC = factors (cTom3, schemeDanBiglooBench)

val printReal = Real.fmt (StringCvt.FIX (SOME 3))
fun printResults (pcratios, bcratios) =
    (   (* print ratios *)
     print "Benchmark\tPLT to C\tBigloo to C\tPLT Ratio to Bigloo Ratio\n";
	
     app (fn (l, (pltToC, biglooToC)) =>
	  print (l ^
		 "\t" ^
		 (printReal pltToC) ^ 
		 "\t\t" ^
		 (printReal biglooToC) ^ 
		 "\t\t" ^
		 (printReal (pltToC / biglooToC)) ^ 
		 "\n"
		 ))
         (ListPair.zipEq (labels, 
			  (ListPair.zipEq (pcratios, bcratios))));
	(* print avgs *)
	print ("\nAverage PLT to C Ratio: " ^ 
	       (Real.toString (average pcratios)) ^
	       "\nAverage Bigloo to C Ratio: " ^ 
	       (Real.toString (average bcratios)) ^ 
	       "\nAverage PLT Ratio to Average Bigloo Ratio: " ^ 
	       (Real.toString ((average pcratios) / (average bcratios))) ^ 
	       "\n"))

(* 
val _ = printResults (ratiosPltToC, ratiosBiglooToC)

Benchmark	PLT to C	Bigloo to C	PLT Ratio to Bigloo Ratio
fact.161	313.465		199.657		1.570
fact.345	271.614		176.932		1.535
micro-add	421.942		261.200		1.615
micro-allocfree	378.540		114.953		3.293
micro-asub0	452.147		271.692		1.664
micro-cmov0	220.652		186.012		1.186
micro-cmov1	203.818		195.386		1.043
micro-div	170.523		103.364		1.650
micro-literal	602.712		488.102		1.235
micro-loadprog0	659.824		350.379		1.883
micro-mul	396.131		266.333		1.487
micro-nand	520.418		237.115		2.195
micro-upd0	395.793		251.350		1.575
quicksort.161	300.614		263.097		1.143
quicksort.345	286.131		197.607		1.448

Average PLT to C Ratio: 372.954925568
Average Bigloo to C Ratio: 237.54520116
Average PLT Ratio to Average Bigloo Ratio: 1.57003771808
*)

(*
val _ = printResults (ratiosPltToC, ratiosBiglooBenchToC)

Benchmark	PLT to C	Bigloo to C	PLT Ratio to Bigloo Ratio
fact.161	313.465		75.111		4.173
fact.345	271.614		72.727		3.735
micro-add	421.942		100.651		4.192
micro-allocfree	378.540		27.449		13.791
micro-asub0	452.147		166.927		2.709
micro-cmov0	220.652		62.696		3.519
micro-cmov1	203.818		69.384		2.938
micro-div	170.523		37.774		4.514
micro-literal	602.712		160.242		3.761
micro-loadprog0	659.824		122.651		5.380
micro-mul	396.131		89.450		4.429
micro-nand	520.418		87.303		5.961
micro-upd0	395.793		79.139		5.001
quicksort.161	300.614		77.109		3.899
quicksort.345	286.131		77.667		3.684

Average PLT to C Ratio: 372.954925568
Average Bigloo to C Ratio: 87.0853150501
Average PLT Ratio to Average Bigloo Ratio: 4.28263852929
*)