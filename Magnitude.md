# Class Magnitude

superclass: [Object](Object);
instanceArchitecture: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #comparing method:
	[## > comparand
	
		^comparand < self
	];
			
	protocol: #comparing method:
	[## < comparand
	
			"Most subclasses will implement #< as a primitive, so this is the fallback implementation."
			
			^(self compareTo: comparand) < 0
	];
				
	protocol: #comparing method:
	[## = comparand
	
			"Most subclasses will implement #= as a primitive, so this is the fallback implementation."
	
			^(self compareTo: comparand) = 0
	];
		
	protocol: #comparing method:
	[## <= comparand
	
			^self < comparand or: [self = comparand](self-=-comparand)
	];
	
	protocol: #comparing method:
	[## >= comparand
	
			^self > comparand or: [self = comparand](self-=-comparand)
	];
				
	protocol: #querying method:
	[## between: minValue and: maxValue
	
		"Answer whether the receiver is greater than or equal to <minValue> and also less than or equal to <maxValue>"
		
		^(self < minValue or: [maxValue < self](maxValue-_-self)) not
	]}}