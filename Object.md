# Class Object

superclass: nil;
instanceArchitecture: #Stateless

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #accessing method: 
	[## class

		"Answer the class of the receiver"
		
		<primitive: class domain: Object>
	];

	protocol: #accessing method: 
	[## yourself 

		"Answer the receiver"
		
		^self
	];
		
	protocol: #binding method:
	[## valueIf: aCollection else: absentBlock
			"#green  
				valueIf: 
					{#red -> [3 + 4](3-+-4). 
					#green -> [DateAndTime now](DateAndTime-now). 
					#blue -> ['Hello, world']('Hello,-world')} 
				else: ['Nobody here but us Smalltalkers']('Nobody-here-but-us-Smalltalkers')"
	
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
	
			^aCollection metaValueAt: self ifAbsent: absentBlock
	];
	
	protocol: #'control flow' method: 
	[## ifNil: nilAction

		"If the receiver is nil, answer the result of evaluating <nilAction>. 
		Otherwise, answer the receiver."
		
		^self
	];
			
	protocol: #'control flow' method: 
	[## ifNotNil: notNilAction

		"If the receiver is not nil, answer the result of evaluating <notNilAction>. 
		Otherwise, answer nil."
		
		<primitive: #ifNotNil: domain: Object>
	];

		
	protocol: #'control flow' method: 
	[## ifNil: nilAction ifNotNil: notNilAction

		"If the receiver is nil, answer the result of evaluating <nilAction>. 
		Otherwise, answer the result of evaluating <notNilAction>."
		
		<primitive: #ifNil:ifNotNil: domain: Object>
	];

	protocol: #'initialize-release' method:
	[## initialize

		"Initialize the receiver's state"

		^self
	];

	protocol: #'object creation' method: 
	[## -> aValue

		"Answer a new Association whose key is the receiver and whose value is <aValue>"
		
		<primitive: #-> domain: Object>
	];
		
	protocol: #'system testing' method: 
	[## showCr: aValue

		aValue showCr
	];

	protocol: #'system testing' method: 
	[## reportToConsole

		('This is a Smalltalk.Object: class = ', self class name) showCr
	];
			
	protocol: #testing method:
	[## isNil

		"Answer whether the receiver is nil"
		
		^false
	];
			
	protocol: #testing method:
	[## notNil

		"Answer whether the receiver is not nil"
		
		^true
	];

	protocol: #testing method:
	[## isTrue

		"Answer whether the receiver is true."

		^false
	];

	protocol: #testing method:
	[## isFalse

		"Answer whether the receiver is false."

		^false
	];

	protocol: #'system primitives' method:
	[## class

		<primitive: class domain: Object>
	];

	protocol: #'system primitives' method:
	[## isMemberOf: aBehavior 

		<primitive: #isMemberOf: domain: Object>
	];

	protocol: #'system primitives' method:
	[## isKindOf: aBehavior 

		<primitive: #isKindOf: domain: Object>
	];

	protocol: #'system primitives' method:
	[## == other

		<primitive: #== domain: Object>
	];

	protocol: #'system primitives' method:
	[## = comparand

		<primitive: #= domain: Object>
	];

	protocol: #'system primitives' method:
	[## identityHash

		<primitive: identityHash domain: Object>
	];

	protocol: #'system primitives' method:
	[## hash

		<primitive: hash domain: Object>
	];

	protocol: #'system primitives' method:
	[## ifNotNil: notNilAction 

		<primitive: #ifNotNil: domain: Object>
	];

	protocol: #'system primitives' method:
	[## ifNil: nilAction ifNotNil: notNilAction 

		<primitive: #ifNil:ifNotNil: domain: Object>
	];

	protocol: #'system primitives' method:
	[## ifNotNil: notNilAction ifNil: nilAction 

		<primitive: #ifNotNil:ifNil: domain: Object>
	];

	protocol: #'system primitives' method:
	[## isImmutable

		<primitive: isImmutable domain: Object>
	];

	protocol: #'system primitives' method:
	[## beImmutable

		<primitive: beImmutable domain: Object>
	];

	protocol: #'system primitives' method:
	[## shallowCopy

		<primitive: shallowCopy domain: Object>
	];

	protocol: #'system primitives' method:
	[## asMutable

		<primitive: asMutable domain: Object>
	];

	protocol: #'system primitives' method:
	[## asImmutable

		<primitive: asImmutable domain: Object>
	];

	protocol: #'system primitives' method:
	[## -> value

		<primitive: #-> domain: Object>
	];

	protocol: #'system primitives' method:
	[## size

		<primitive: size domain: Object>
	];

	protocol: #'system primitives' method:
	[## instVarValueAt: slotIndex 

		<primitive: #instVarValueAt: domain: Object>
	];

	protocol: #'system primitives' method:
	[## instVarValueAtName: name 

		<primitive: #instVarValueAtName: domain: Object>
	];

	protocol: #'system primitives' method:
	[## instVarValueAt: slotIndex put: newValue 

		<primitive: #instVarValueAt:put: domain: Object>
	];

	protocol: #'system primitives' method:
	[## instVarValueAtName: name put: newValue 

		<primitive: #instVarValueAtName:put: domain: Object>
	];

	protocol: #'system primitives' method:
	[## respondsTo: selector 

		<primitive: #respondsTo: domain: Object>
	];

	protocol: #'system primitives' method:
	[## respondTo: message 

		<primitive: #respondTo: domain: Object>
	];

	protocol: #'system primitives' method:
	[## perform: selector 

		<primitive: #perform: domain: Object>
	];

	protocol: #'system primitives' method:
	[## perform: selector with: a1 

		<primitive: #perform:with: domain: Object>
	];

	protocol: #'system primitives' method:
	[## perform: selector with: a1 with: a2 

		<primitive: #perform:with:with: domain: Object>
	];

	protocol: #'system primitives' method:
	[## perform: selector with: a1 with: a2 with: a3 

		<primitive: #perform:with:with:with: domain: Object>
	];

	protocol: #'system primitives' method:
	[## perform: selector with: a1 with: a2 with: a3 with: a4 

		<primitive: #perform:with:with:with:with: domain: Object>
	];

	protocol: #'system primitives' method:
	[## perform: selector withArguments: arguments 

		<primitive: #perform:withArguments: domain: Object>
	];

	protocol: #'system primitives' method:
	[## halt

		<primitive: halt domain: Object>
	];

	protocol: #'system primitives' method:
	[## show

		<primitive: show domain: Object>
	];

	protocol: #'system primitives' method:
	[## crShow

		<primitive: crShow domain: Object>
	];

	protocol: #'system primitives' method:
	[## showCr

		<primitive: showCr domain: Object>
	]}}