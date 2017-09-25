# Class Character

superclass: [PrimitiveValue](PrimitiveValue);
instanceArchitecture: #Char

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #'comparing-double dispatch' method:
[## inverseCompareToCharacter: aChar

        ^(aChar asInteger - self asInteger) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToSmallInteger: anInteger

        ^(anInteger - self asInteger) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToFloat: aFloat

        ^(aFloat asInteger - self asInteger) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToDouble: aDouble

        ^(aDouble asInteger - self asInteger) sign
];

protocol: #converting method:
[## asCharacter

        ^self
];

protocol: #converting method:
[## coerce: operand

        ^operand asCharacter
]}}