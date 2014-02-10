(* Graham Hayes*)
(*For loops for SML, From: http://mlton.org/ForLoops*)
datatype for = to of int * int
             | downto of int * int

infix to downto

val for =
    fn lo to up =>
       (fn f => let fun loop lo = if lo > up then ()
                                  else (f lo; loop (lo+1))
                in loop lo end)
     | up downto lo =>
       (fn f => let fun loop up = if up < lo then ()
                                  else (f up; loop (up-1))
                in loop up end);
(*-----------------------------------------------------------*)				

(*returns '_' if empty, '-' for invalid, or char*)
fun getInputChar(prompt:string, forceValid:bool, allowEmpty:bool, in_min:char, in_max:char)=
	(print prompt;
	let
		val emptyFlag= #"\n";
		val invalidFlag = #"-";		
		val line =TextIO.inputLine TextIO.stdIn;
		val value = Option.getOpt(Char.fromString(valOf(line)),invalidFlag); (*Integer.fromString(line);*)
	in
		(*ensure its correct*)
		if ((allowEmpty = true) andalso(valOf(line) = "\n"))then
			emptyFlag
		else if ((value <> invalidFlag) andalso (Char.ord(value) >= Char.ord(in_min)) andalso (Char.ord(value) <= Char.ord(in_max))) then
			value
		else if (forceValid = true) then (*if we are here, invalid Input, call again to reprompt*)
			(print "[INVALID] ";
			getInputChar(prompt, forceValid, allowEmpty, in_min, in_max))
		else			
			invalidFlag
	end);
	
(*returns ~2 if empty, ~1 for invalid, or number*)
fun getInputInt(prompt:string, forceValid:bool, allowEmpty:bool, in_min:int, in_max:int)=
	(print prompt;
	let
		val emptyFlag= ~2;
		val invalidFlag = ~1;
		val line =TextIO.inputLine TextIO.stdIn;
		val value = Option.getOpt(Int.fromString(valOf(line)),invalidFlag); (*Integer.fromString(line);*)
	in
		(*ensure its correct*)
		if ((allowEmpty = true) andalso(valOf(line) = "\n"))then
			emptyFlag
		else if ((value <> invalidFlag) andalso (value >= in_min) andalso (value <= in_max)) then
			value
		else if (forceValid = true) then (*if we are here, invalid Input, call again to reprompt*)
			(print "[INVALID] ";
			getInputInt(prompt, forceValid, allowEmpty, in_min, in_max))
		else			
			value
	end);	
	
fun removeElemAt(i, myList) = List.filter (fn x => x <> List.nth(myList, i)) myList;
fun removeElem elem myList = List.filter (fn x => x <> elem) myList;

fun alterIndex(i, newval, in_list) =
	(List.tabulate(List.length in_list, 
		(fn j=> (
			let 
				val cur= List.nth(in_list, j); 
			in
				if(j=i)then
					newval
				else
					cur
			end))
	
	));

fun RandomlyGetNd (x, n) =
	let
		val r = Random.rand (IntInf.toInt (IntInf.mod (Time.toSeconds (Time.now ()), 65536)), 0);
		val intmap=ref [1]; 
		val ret = ref [];
		val chosen = ref 0;
	in
		intmap := List.tabulate(length x, (fn i=> i));
		for(0 to (n-1))
		(fn i => (
			chosen := Random.randRange(0,(length (!intmap))-1) r;
			ret := List.nth(!intmap, !chosen) :: (!ret); (*List.nth(x, !chosen) :: (!ret);*)			
			intmap := removeElemAt(!chosen, !intmap)));			
		!ret
	end;

	
fun RandomlyGetN (x, n) =
	let
		val r = Random.rand (IntInf.toInt (IntInf.mod (Time.toSeconds (Time.now ()), 65536)), 0);
		val intmap=ref [1]; 
		val ret = ref [];
		val chosen = ref 0;
	in
		intmap := List.tabulate(length x, (fn i=> i));
		for(0 to (n-1))
		(fn i => (
			chosen := Random.randRange(0,(length (!intmap))-1) r;
			ret := List.nth(x, List.nth(!intmap, !chosen)) :: (!ret); (*List.nth(x, !chosen) :: (!ret);*)			
			intmap := removeElemAt(!chosen, !intmap)));			
		!ret
	end;

fun three _ =
	let		
		(*TODO: parse for player count*)
		val in_players = getInputInt("Enter Number of Players: ", true, false, 1,3); (*~1*)
		val players = ref (List.tabulate(in_players, (fn i=> true)));
		val crims= ref [(#"a", false, false),
					(#"b", false, false),
					(#"c", false, false),
					(#"d", false, false),
					(#"e", false, false),
					(#"f", false, false),					
					(#"g", false, false)];
		val perps = ref (RandomlyGetNd(!crims,3));
		fun isPlayerLeft x = (List.exists (fn i=> i=true) x);
		fun isPerpsLeft x = ((List.length x) >0);
		val r = Random.rand (IntInf.toInt (IntInf.mod (Time.toSeconds (Time.now ()), 65536)), 0);
		fun min (x, y) = (if(y<x) then y else x);
		fun max (x, y) = (if(y>x) then y else x);
		val nonperps = ref [1];
		fun alterCrims(i, isPerp, isIdentified) =
			crims := (List.tabulate(List.length (!crims), 
				(fn j=> (
					let 
						val cur= List.nth(!crims, j); 
					in
						if(j=i)then
							(#1 cur, isPerp, isIdentified)
						else
							cur
					end))
			
			));
			
		fun getNonPerps _ =
			List.filter (fn e=> e<> ~1) (List.tabulate(List.length (!crims), 
				(fn j=> (
					let 
						val cur= List.nth(!crims, j); 
					in
						if((#2 cur) <> true)then
							j
						else
							~1
					end))
			
			));
		fun mapToList(inMap, inList) = (List.map (fn i=> (List.nth(inList,i))) inMap);		
		fun printCrimNames inMap = List.app (fn e => (print (Char.toString(#1 e) ^ "\n"))) (mapToList(inMap, !crims));
		fun getActivePlayers _ =
			List.filter (fn e=> e<> ~1) (List.tabulate(List.length (!players), 
				(fn j=> (if(List.nth(!players, j) = true)then j else ~1))			
			));
		fun GetIndexForName in_name =
			let
				val frm_name = Char.toLower(in_name);
			in
			hd(List.filter (fn e=> e<> ~1) (List.tabulate(List.length (!crims), 
				(fn j=> (
					let 
						val cur= List.nth(!crims, j); 
					in
						if((#1 cur) = frm_name)then
							j
						else
							~1
					end))
			
			)))end;
		fun debug_print(prompt, in_list) = 
		(
			print("\tDEBUG: " ^ prompt ^ " are now: [");
			List.app (fn e => print(" " ^ Char.toString(#1(List.nth((!crims),e))))) in_list;
			print("] raw: ["); List.app (fn e => print(" " ^ Int.toString(e))) in_list; print("]\n")
		);
		fun debug_print_bool(prompt, in_list) = 
		(
			print("\tDEBUG: " ^ prompt ^ " are now: [");
			List.app (fn e => print(" " ^ Bool.toString(e))) in_list; print("]\n")		
		);
	in		
		(*Set alter Crims to reflect selected Perps*)
		List.app (fn e=> alterCrims(e,true, false)) (!perps);
		
		(*Set Non Perps*)
		nonperps := getNonPerps();
				
		(*Round Start*)		
		while(isPlayerLeft(!players) andalso isPerpsLeft((!perps))) do 
			(let
				val dispPerps = Random.randRange(0, min(2, List.length (!perps))) r;
				val toDisplay = ListMergeSort.sort op> ((RandomlyGetN((!perps), dispPerps))@(RandomlyGetN((!nonperps), 3- dispPerps)));
				(*val activePlayers = getActivePlayers();*)
			in
				(*Print List of Hints*)
				print("\nOf the following criminals, " ^ Int.toString(dispPerps) ^ " are perpetrators:\n");				
				printCrimNames (toDisplay);
							
				
				(*Start Player Turns*)
				List.app (fn player =>
					(let
						val guess = getInputChar("Player: " ^ Int.toString(player+1) ^ " your guess (enter nothing to skip your turn): ", true, true, #"a", #1(List.last (!crims)) );
						val iguess = (if(guess= #"\n") then ~1 else (GetIndexForName guess));
					in
						if(guess <> #"\n")then (*player made a valid guess, validate it*)
						(
							if(#2(List.nth((!crims),iguess))=true)then (*Player Guess Successfully*)
							(
								print("Your guess is correct!\n");
								alterCrims(iguess, true, true);
								perps := removeElem iguess (!perps)								
							)
							else (*Player Guess Incorrectly*)
							(
								print("You guess is incorrect! You have been eliminated!\n");
								players := alterIndex(player, false, (!players)) (*eliminate player*)								
							)
						)
						else()
						
					end)
				) (getActivePlayers())
			end
			);
			
		(*Game Over Check for condition that ended the game*)
		if(isPerpsLeft((!perps))=false)then
		(
			print("GAME OVER: All perpetrators have been correctly identified, Players WIN!")
		)
		else if(isPlayerLeft(!players) = false) then
		(
			print("GAME OVER: All players have been eliminated, Players LOSE!");
			print("Remaining Perpetrators: ");
			
			List.app (fn i => ()) (List.filter 
			(fn crim =>
				if(#2(crim)=true andalso #3(crim)=false)then
					true
				else
					false
			)
			(!crims))
		)
		else();
		print("\n")
	end;
	
three();