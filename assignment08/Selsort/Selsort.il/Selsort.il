
	// Methods
	.method public hidebysig static 
		void Main (
			string[] args
		) cil managed 
	{
		// Method begins at RVA 0x2050
		// Header size: 12
		// Code size: 110 (0x6e)
		.maxstack 4
		.entrypoint
		.locals init (
			[0] int32, // count
			[1] int32[], // list
			[2] int32, // i
			[3] bool, // i < count
			[4] int32, // i, again
			[5] bool // i < count 
		)

		IL_0000: nop
		IL_0001: ldc.i4.s 10 // set count to 10
		IL_0003: stloc.0 // store count
		IL_0004: ldloc.0 // Load local variable (count ? )
		IL_0005: newarr [System.Runtime]System.Int32 // new array with length count
		IL_000a: stloc.1 // store array
		IL_000b: ldc.i4.0 // i = 0
		IL_000c: stloc.2 // store i at 2
		IL_000d: br.s IL_0025 // jump to for loop
		// loop start (head: IL_0025)
			IL_000f: ldloc.1 // load array
			IL_0010: ldloc.2 // load i
			IL_0011: ldsfld class [System.Runtime]System.Random Selsort::rnd // load random
			IL_0016: ldc.i4 1000000 // random with value 1000000
			IL_001b: callvirt instance int32 [System.Runtime]System.Random::Next(int32)
			IL_0020: stelem.i4 // store element
			IL_0021: ldloc.2 // load i
			IL_0022: ldc.i4.1 // push 1
			IL_0023: add // i + 1
			IL_0024: stloc.2 // store i

			IL_0025: ldloc.2 // load i
			IL_0026: ldloc.0 // load count
			IL_0027: clt // compare less than (i < count)
			IL_0029: stloc.3 // store bool
			IL_002a: ldloc.3 // load bool
			IL_002b: brtrue.s IL_000f // jumps to start of loop if i < count
		// end loop

		IL_002d: ldloc.1 // load list
		IL_002e: call void Selsort::SelectionSort(int32[]) // call selectionsort
		IL_0033: nop
		IL_0034: ldc.i4.0 // push 0
		IL_0035: stloc.s 4 // store on i
		IL_0037: br.s IL_005c
		// loop start (head: IL_005c)
			IL_0039: ldloc.1 // load count
			IL_003a: ldloc.s 4 // load i
			IL_003c: ldelema [System.Runtime]System.Int32
			IL_0041: call instance string [System.Runtime]System.Int32::ToString() // integers to string
			IL_0046: ldstr " " // space between..
			IL_004b: call string [System.Runtime]System.String::Concat(string, string) // combine strings together
			IL_0050: call void [System.Console]System.Console::Write(string) // Print to console
			IL_0055: nop
			IL_0056: ldloc.s 4 // load i
			IL_0058: ldc.i4.1 // push 1
			IL_0059: add // i + 1
			IL_005a: stloc.s 4 store i + 1

			IL_005c: ldloc.s 4 // load i 
			IL_005e: ldloc.0 // count
			IL_005f: clt // i < count
			IL_0061: stloc.s 5 // store bool
			IL_0063: ldloc.s 5 // load bool
			IL_0065: brtrue.s IL_0039 // jump to start of loop if true
		// end loop

		IL_0067: call void [System.Console]System.Console::WriteLine() // print line idk
		IL_006c: nop // nothing
		IL_006d: ret // return
	} // end of method Selsort::Main

	


