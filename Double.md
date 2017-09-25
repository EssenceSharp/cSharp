# Class Double

superclass: [InvariantPrecisionReal](InvariantPrecisionReal);
instanceArchitecture: #DoublePrecision

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #'arithmetic-double dispatching' method:
[## sumWithSmallInteger: operand

        ^operand asDouble + self
];

protocol: #'arithmetic-double dispatching' method:
[## differenceFromSmallInteger: operand

        ^operand asDouble - self
];

protocol: #'arithmetic-double dispatching' method:
[## productWithSmallInteger: operand

        ^operand asDouble * self
];

protocol: #'arithmetic-double dispatching' method:
[## ratioToSmallInteger: operand

        ^operand asDouble / self
]; 

protocol: #'arithmetic-double dispatching' method:
[## integerRatioToSmallInteger: operand

        ^operand asDouble quo: self
];

protocol: #'arithmetic-double dispatching' method:
[## flooredToIntegerRatioToSmallInteger: operand

        ^operand asDouble // self
];

protocol: #'arithmetic-double dispatching' method:
[## remainderFromDividingSmallInteger: operand

        ^operand asDouble rem: self
];

protocol: #'arithmetic-double dispatching' method:
[## residueFromDividingSmallInteger: operand

        ^operand asDouble \\ self
];

protocol: #'arithmetic-double dispatching' method:
[## valueAsExponentToSmallInteger: operand

        ^operand asDouble ** self
];

protocol: #'arithmetic-double dispatching' method:
[## valueAsLogBaseToSmallInteger: operand

        ^operand asDouble log: self
];

protocol: #'arithmetic-double dispatching' method:
[## sumWithFloat: operand

        ^operand asDouble + self
];

protocol: #'arithmetic-double dispatching' method:
[## differenceFromFloat: operand

        ^operand asDouble - self
];

protocol: #'arithmetic-double dispatching' method:
[## productWithFloat: operand

        ^operand asDouble * self
];

protocol: #'arithmetic-double dispatching' method:
[## ratioToFloat: operand

        ^operand asDouble / self
]; 

protocol: #'arithmetic-double dispatching' method:
[## integerRatioToFloat: operand

        ^operand asDouble quo: self
];

protocol: #'arithmetic-double dispatching' method:
[## flooredToIntegerRatioToFloat: operand

        ^operand asDouble // self
];

protocol: #'arithmetic-double dispatching' method:
[## remainderFromDividingFloat: operand

        ^operand asDouble rem: self
];

protocol: #'arithmetic-double dispatching' method:
[## residueFromDividingFloat: operand

        ^operand asDouble \\ self
];

protocol: #'arithmetic-double dispatching' method:
[## valueAsExponentToFloat: operand

        ^operand asDouble ** self
];

protocol: #'arithmetic-double dispatching' method:
[## valueAsLogBaseToFloat: operand

        ^operand asDouble log: self
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToSmallInteger: aSmallInteger

        ^(aSmallInteger asDouble - self) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToCharacter: aCharacter

        ^(aCharacter asDouble - self) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToDouble: aFloat

        ^(aFloat asDouble - self) sign
];

protocol: #'comparing-double dispatch' method:
[## inverseCompareToDouble: aDouble

        ^(aDouble - self) sign
];

protocol: #converting
method:
[## coerce: operand

        ^operand asDouble

];

protocol: #generality method:
[## withHighestGenerality: operand

        "Answers whichever value has the highest generality."

        ^operand withGeneralityEqualToOrGreaterThanDouble: self
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

        self
];

protocol: #'system primitives' method:
[## class

        <primitive: class domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## isMemberOf: aBehavior

        <primitive: #isMemberOf: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## isKindOf: aBehavior

        <primitive: #isKindOf: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## = comparand

        <primitive: #= domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## hash

        <primitive: hash domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## compareTo: comparand

        <primitive: #compareTo: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## < comparand

        <primitive: #< domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## asCharacter

        <primitive: asCharacter domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## asInteger

        <primitive: asInteger domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## asFloat

        <primitive: asFloat domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## asQuad

        <primitive: asQuad domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## sign

        <primitive: sign domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## isZero

        <primitive: isZero domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## positive

        <primitive: positive domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## negative

        <primitive: negative domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## abs

        <primitive: abs domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## negated

        <primitive: negated domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## + operand

        <primitive: #+ domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## - operand

        <primitive: #- domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## * operand

        <primitive: #* domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## / operand

        <primitive: #/ domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## reciprocal

        <primitive: reciprocal domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## roundTo: operand

        <primitive: #roundTo: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## truncateTo: modulus

        <primitive: #truncateTo: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## quo: operand

        <primitive: #quo: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## // operand

        <primitive: #// domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## rem: operand

        <primitive: #rem: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## \\ operand

        <primitive: #\\ domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## ** operand

        <primitive: #** domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## log: operand

        <primitive: #log: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## ln

        <primitive: ln domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## to: end do: enumerator

        <primitive: #to:do: domain: DoublePrecision>
];

protocol: #'system primitives' method:
[## to: end by: step do: enumerator

        <primitive: #to:by:do: domain: DoublePrecision>
]}}