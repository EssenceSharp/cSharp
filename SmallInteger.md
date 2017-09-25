# Class SmallInteger

superclass: [Integer](Integer);
instanceArchitecture: #SmallInteger

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #'arithmetic-double dispatching' method:
	[## sumWithFloat: operand
	
		^operand + self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## differenceFromFloat: operand
	
		^operand - self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## productWithFloat: operand
	
		^operand * self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## ratioToFloat: operand
	
		^operand / self asFloat
	];		
		
	protocol: #'arithmetic-double dispatching' method:
	[## integerRatioToFloat: operand
	
		^operand quo: self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## flooredToIntegerRatioToFloat: operand
	
		^operand // self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## remainderFromDividingFloat: operand
	
		^operand rem: self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## residueFromDividingFloat: operand
	
		^operand \\ self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsExponentToFloat: operand
	
		^operand ** self asFloat
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsLogBaseToFloat: operand
	
		^operand log: self asFloat
	];
				
	protocol: #'arithmetic-double dispatching' method:
	[## sumWithDouble: operand
	
		^operand + self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## differenceFromDouble: operand
	
		^operand - self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## productWithDouble: operand
	
		^operand * self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## ratioToDouble: operand
	
		^operand / self asDouble
	];		
		
	protocol: #'arithmetic-double dispatching' method:
	[## integerRatioToDouble: operand
	
		^operand quo: self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## flooredToIntegerRatioToDouble: operand
	
		^operand // self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## remainderFromDividingDouble: operand
	
		^operand rem: self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## residueFromDividingDouble: operand
	
		^operand \\ self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsExponentToDouble: operand
	
		^operand ** self asDouble
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsLogBaseToDouble: operand
	
		^operand log: self asDouble
	];
			
	protocol: #'comparing-double dispatch' method:
	[## inverseCompareToSmallInteger: aSmallInteger
	
		^(aSmallInteger - self) sign
	];
			
	protocol: #'comparing-double dispatch' method:
	[## inverseCompareToCharacter: aCharacter
	
		^(aCharacter asInteger - self) sign
	];
			
	protocol: #'comparing-double dispatch' method:
	[## inverseCompareToFloat: aFloat
	
		^(aFloat - self asFloat) sign
	];
			
	protocol: #'comparing-double dispatch' method:
	[## inverseCompareToDouble: aDouble
	
		^(aDouble - self asDouble) sign
	];
			
	protocol: #generality method:
	[## withHighestGenerality: operand
	
		"Answers whichever value has the highest generality."
		
		^operand withGeneralityEqualToOrGreaterThanSmallInteger: self
	];
			
	protocol: #generality method:
	[## withGeneralityEqualToOrGreaterThanSmallInteger: operand
	
		"Answers whichever value has the highest generality. Assumes the operand is a SmallInteger."
	
		^self
	];
		
	protocol: #generality method:
	[## withGeneralityEqualToOrGreaterThanFloat: operand
	
		"Answers whichever value has the highest generality. Assumes the operand is a Float."
	
		^operand
	];
			
	protocol: #generality method:
	[## withGeneralityEqualToOrGreaterThanDouble: operand
	
		"Answers whichever value has the highest generality. Assumes the operand is a Double."
	
		^operand
	]

	protocol: #'system primitives' method:
	[## class

		<primitive: class domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## isMemberOf: aBehavior 

		<primitive: #isMemberOf: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## isKindOf: aBehavior 

		<primitive: #isKindOf: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## = comparand

		<primitive: #= domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## hash

		<primitive: hash domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## compareTo: comparand 

		<primitive: #compareTo: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## < comparand

		<primitive: #< domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## asCharacter

		<primitive: asCharacter domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## asFloat

		<primitive: asFloat domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## asDouble

		<primitive: asDouble domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## asQuad

		<primitive: asQuad domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## sign

		<primitive: sign domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## isZero

		<primitive: isZero domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## positive

		<primitive: positive domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## negative

		<primitive: negative domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## abs

		<primitive: abs domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## negated

		<primitive: negated domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## + operand

		<primitive: #+ domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## - operand

		<primitive: #- domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## * operand

		<primitive: #* domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## / operand

		<primitive: #/ domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## reciprocal

		<primitive: reciprocal domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## roundTo: operand 

		<primitive: #roundTo: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## truncateTo: modulus 

		<primitive: #truncateTo: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## quo: operand 

		<primitive: #quo: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## // operand

		<primitive: #// domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## rem: operand 

		<primitive: #rem: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## \\ operand

		<primitive: #\\ domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## ** operand

		<primitive: #** domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## log: operand 

		<primitive: #log: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## ln

		<primitive: ln domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## timesRepeat: action 

		<primitive: #timesRepeat: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## to: end do: enumerator 

		<primitive: #to:do: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## to: end by: step do: enumerator 

		<primitive: #to:by:do: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## bitAnd: operand 

		<primitive: #bitAnd: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## bitOr: operand 

		<primitive: #bitOr: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## bitXor: operand 

		<primitive: #bitXor: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## bitShift: operand 

		<primitive: #bitShift: domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## << operand

		<primitive: #<< domain: SmallInteger>
	];

	protocol: #'system primitives' method:
	[## >> operand

		<primitive: #>> domain: SmallInteger>
	]}}
