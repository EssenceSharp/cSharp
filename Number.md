# Class Number

superclass: [ArithmeticValue](ArithmeticValue);
instanceArchitecture: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #'arithmetic-double dispatching' method:
	[## integerRatioToSmallInteger: operand
	
		^(self coerce: operand) quo: self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## flooredToIntegerRatioToSmallInteger: operand
	
		^(self coerce: operand) // self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## remainderFromDividingSmallInteger: operand
	
		^(self coerce: operand) rem: self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## residueFromDividingSmallInteger: operand
	
		^(self coerce: operand) \\ self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsExponentToSmallInteger: operand
	
		^(self coerce: operand) ** self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsLogBaseToSmallInteger: operand
	
		^(self coerce: operand) log: self
	];
			
	protocol: #'arithmetic-double dispatching' method:
	[## integerRatioToFloat: operand
	
		^(self coerce: operand) quo: self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## flooredToIntegerRatioToFloat: operand
	
		^(self coerce: operand) // self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## remainderFromDividingFloat: operand
	
		^(self coerce: operand) rem: self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## residueFromDividingFloat: operand
	
		^(self coerce: operand) \\ self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsExponentToFloat: operand
	
		^(self coerce: operand) ** self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsLogBaseToFloat: operand
	
		^(self coerce: operand) log: self
	];
	
	protocol: #'arithmetic-double dispatching' method:
	[## integerRatioToDouble: operand
	
		^(self coerce: operand) quo: self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## flooredToIntegerRatioToDouble: operand
	
		^(self coerce: operand) // self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## remainderFromDividingDouble: operand
	
		^(self coerce: operand) rem: self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## residueFromDividingDouble: operand
	
		^(self coerce: operand) \\ self
	];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsExponentToDouble: operand
	
		^(self coerce: operand) ** self];

	protocol: #'arithmetic-double dispatching' method:
	[## valueAsLogBaseToDouble: operand
	
		^(self coerce: operand) log: self
	];
	
	protocol: #converting method:
	[## asNumber
	
		^self
	];
	
	protocol: #converting method:
	[## asReal
	
		^self asDouble
	];
			
	protocol: #converting method:
	[## coerce: operand
	
		^operand asNumber
	]}}
