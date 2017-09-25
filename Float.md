# Class Float

superclass: [InvariantPrecisionReal](InvariantPrecisionReal);
instanceArchitecture: #SinglePrecision

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #'arithmetic-double dispatching' method:
[## differenceFromSmallInteger: operand

        ^operand asFloat - self
];

protocol: #'arithmetic-double dispatching' method:
[## productWithSmallInteger: operand

        ^operand asFloat * self
];

protocol: #'arithmetic-double dispatching' method:
[## ratioToSmallInteger: operand

        ^operand asFloat / self
]; 

protocol: #'arithmetic-double dispatching' method:
[## integerRatioToSmallInteger: operand

        ^operand asFloat quo: self
];

protocol: #'arithmetic-double dispatching' method:
[## flooredToIntegerRatioToSmallInteger: operand

        ^operand asFloat // self
];

protocol: #'arithmetic-double dispatching' method:
[## remainderFromDividingSmallInteger: operand

        ^operand asFloat rem: self
];

protocol: #'arithmetic-double dispatching' method:
[## residueFromDividingSmallInteger: operand

        ^operand asFloat \\ self
];

protocol: #'arithmetic-double dispatching' method:
[## valueAsExponentToSmallInteger: operand

        ^operand asFloat ** self
];

protocol: #'arithmetic-double dispatching' method:
[## valueAsLogBaseToSmallInteger: operand

        ^operand asFloat log: self
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

        ^(aSmallInteger asFloat - self) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToCharacter: aCharacter

        ^(aCharacter asFloat - self) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToFloat: aFloat

        ^(aFloat - self) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToDouble: aDouble

        ^(aDouble - self asDouble) sign
];

protocol: #converting
method:
[## coerce: operand

        ^operand asFloat
];

protocol: #generality method:
[## withHighestGenerality: operand

        "Answers whichever value has the highest generality."

        ^operand withGeneralityEqualToOrGreaterThanFloat: self
];

protocol: #generality method:
[## withGeneralityEqualToOrGreaterThanSmallInteger: operand

        "Answers whichever value has the highest generality. Assumes the operand is a SmallInteger."

        ^self
];

protocol: #generality method:
[## withGeneralityEqualToOrGreaterThanFloat: operand

        "Answers whichever value has the highest generality. Assumes the operand is a Float."

        ^self
];

protocol: #generality method:
[## withGeneralityEqualToOrGreaterThanDouble: operand

        "Answers whichever value has the highest generality. Assumes the operand is a Double."

        ^operand
];

protocol: #'system primitives' method:
[## class

        <primitive: class domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## isMemberOf: aBehavior

        primitive: #isMemberOf: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## isKindOf: aBehavior

        <primitive: #isKindOf: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## = comparand

        <primitive: #= domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## hash

        <primitive: hash domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## compareTo: comparand

        <primitive: #compareTo: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## < comparand

        <primitive: #< domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## asCharacter

        <primitive: asCharacter domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## asInteger

        <primitive: asInteger domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## asDouble

        <primitive: asDouble domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## asQuad

        <primitive: asQuad domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## sign

        <primitive: sign domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## isZero

        <primitive: isZero domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## positive

        <primitive: positive domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## negative

        <primitive: negative domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## abs

        <primitive: abs domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## negated

        <primitive: negated domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## + operand

        <primitive: #+ domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## - operand

        <primitive: #- domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## * operand

        <primitive: #* domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## / operand

        <primitive: #/ domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## reciprocal

        <primitive: reciprocal domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## roundTo: operand

        <primitive: #roundTo: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## truncateTo: modulus

        <primitive: #truncateTo: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## quo: operand

        <primitive: #quo: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## // operand

        <primitive: #// domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## rem: operand

        <primitive: #rem: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## \\ operand

        <primitive: #\\ domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## ** operand

        <primitive: #** domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## log: operand

        <primitive: #log: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## ln

        <primitive: ln domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## to: end do: enumerator

        <primitive: #to:do: domain: SinglePrecision>
];

protocol: #'system primitives' method:
[## to: end by: step do: enumerator

        <primitive: #to:by:do: domain: SinglePrecision>
]}}