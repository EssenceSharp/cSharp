# Class Integer

superclass: [Number](Number);
instanceArchitecture: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #binding method:
	[## valueIf: aCollection else: absentBlock
			"2
				valueIf: 
					{[3 + 4](3-+-4). 
					[DateAndTime now](DateAndTime-now). 
					['Hello, world']('Hello,-world')} 
				else: ['Nobody here but us Smalltalkers']('Nobody-here-but-us-Smalltalkers')"
			
			"| dict | dict := Dictionary new. dict at: #red put: [3 + 4](3-+-4). dict at: #green put: [DateAndTime now](DateAndTime-now). dict at: #blue put: ['Hello, world']('Hello,-world').
			#green 
				valueIf: dict 
				else:  ['Nobody here but us Smalltalkers']('Nobody-here-but-us-Smalltalkers')"
			
			^aCollection metaValueAtIndex: self ifAbsent: absentBlock
	];
	
	protocol: #converting method:
	[## coerce: operand
		
		^operand asInteger
	]}}