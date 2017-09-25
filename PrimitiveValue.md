# Class PrimitiveValue

superclass: [CLR.System.Object](CLR.System.Object);
instanceArchitecture: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #accessing method:
	[## size
	
		"Primitive values do not have indexed slots (they aren't arrays.)"
		
		^0
	];
		
	protocol: #copying method:
	[## copy

		"Answer a copy of the receiver. 
		If the receiver is a singleton or canonical value, answer the receiver."

		^self
	];

	protocol: #copying method:
	[## shallowCopy

		"Answer a shallow copy of the receiver (such that the copy references identical objects as the receiver.) 
		If the receiver is a singleton or canonical value, answer the receiver."

		^self
	];
			
	protocol: #mutability method:
	[## asMutable
	
		"There is no state to be mutated, so this is a no op"
		
		^self
	];
			
	protocol: #mutability method:
	[## asImmutable
	
		"There is no state to be mutated, so this is a no op"
		
		^self
	];
			
	protocol: #mutability method:
	[## beImmutable
	
		"Primitive values are always immutable. So this is a no op"
		
		^self
	];
		
	protocol: #querying method:
	[## between: minValue and: maxValue
	
		"Answer whether the receiver is greater than or equal to <minValue> and also less than or equal to <maxValue>"
		
		^(self < minValue or: [maxValue < self](maxValue-_-self)) not
	];
			
	protocol: #testing method:
	[## isImmutable
	
		^true
	]}}